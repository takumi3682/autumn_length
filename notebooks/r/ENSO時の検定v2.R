library(tidyverse)
library(here)
library(glue)
library(showtext)
library(ggExtra)
library(readr)

showtext_auto()

source(here("notebooks", "r", "theme.R"))
source(here("notebooks", "r", "scales.R"))
source(here("notebooks", "r", "plot_scatter_facet.R"))

# =========================================================
# 設定
# =========================================================
stn <- "仙台"

DATA_RAW_DIR  <- here("data", "raw")
DATA_PROC_DIR <- here("data", "processed")
OUTPUT_DIR    <- here("outputs")

dir.create(DATA_PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR,    recursive = TRUE, showWarnings = FALSE)


# =========================================================
# ENSO期間
# =========================================================
elnino <- tibble(
  start = c(1951,1957,1963,1965,1968,1972,1976,1979,1982,1986,1991,1993,1997,
            2002,2009,2015,2018,2023),
  end   = c(1952,1958,1964,1966,1970,1973,1977,1980,1983,1988,1992,1993,1998,
            2003,2010,2016,2019,2024),
  type  = "ElNino"
)

lanina <- tibble(
  start = c(1954,1964,1967,1970,1973,1975,1984,1988,1995,1998,2007,2010,2017,2020),
  end   = c(1956,1965,1968,1972,1974,1976,1985,1989,1996,2000,2008,2011,2018,2022),
  type  = "LaNina"
)

enso_periods <- bind_rows(elnino, lanina)

# =========================================================
# 基本関数
# =========================================================

get_enso <- function(year){
  state <- "Neutral"
  for(i in seq_len(nrow(enso_periods))){
    s <- enso_periods$start[i]
    e <- enso_periods$end[i]
    if(year >= s && year <= e){
      state <- enso_periods$type[i]
    }
  }
  state
}

# ---- 元データ作成 ----
build_analysis_data <- function(stn){
  
  df <- read_csv(
    here("data", "raw", glue("生物季節観測_{stn}_DOY_Temp.csv")),
    show_col_types = FALSE
  )
  
  enso_tbl <- tibble(
    年 = sort(unique(df$年))
  ) %>%
    mutate(
      ENSO_summer      = map_chr(年, get_enso),
      ENSO_winter      = map_chr(年, get_enso),
      ENSO_next_winter = lead(ENSO_winter)
    )
  
  df2 <- df %>%
    left_join(enso_tbl, by = "年") %>%
    relocate(ENSO_summer, ENSO_winter, ENSO_next_winter, .after = 年) %>%
    mutate(
      across(
        matches("月$"),
        ~ as.numeric(str_extract(as.character(.x), "[0-9.]+"))
      )
    ) %>%
    mutate(
      春平均気温     = rowMeans(across(c(`3月`, `4月`, `5月`)), na.rm = TRUE),
      夏平均気温     = rowMeans(across(c(`6月`, `7月`, `8月`)), na.rm = TRUE),
      秋平均気温     = rowMeans(across(c(`9月`, `10月`, `11月`)), na.rm = TRUE),
      冬平均気温     = rowMeans(cbind(lag(`12月`), `1月`, `2月`), na.rm = TRUE),
      次の冬平均気温 = rowMeans(cbind(`12月`, lead(`1月`), lead(`2月`)), na.rm = TRUE)
    )
  
  out_file <- here("data", "processed", glue("生物季節観測_{stn}_DOY_Temp_ENSO.csv"))
  write_csv(df2, out_file)
  
  df2
}

# ---- 任意列を detrend して *_dt を追加 ----
add_detrended_cols <- function(df, cols, year_col = "年"){
  
  df_out <- df
  
  for(col in cols){
    dt_name <- paste0(col, "_dt")
    fit <- lm(reformulate(year_col, response = col), data = df_out)
    df_out[[dt_name]] <- resid(fit)
  }
  
  df_out
}

# =========================================================
# 汎用：Fisher解析データ
# =========================================================
build_fisher_data_generic <- function(df, x_col, y_col, enso_col = "ENSO_next_winter"){
  df %>%
    drop_na(all_of(c(x_col, y_col, enso_col))) %>%
    mutate(
      cold_x = .data[[x_col]] < 0,
      warm_y = .data[[y_col]] > 0,
      quad = case_when(
        .data[[x_col]] < 0 & .data[[y_col]] > 0  ~ "冷側・暖側",
        .data[[x_col]] < 0 & .data[[y_col]] <= 0 ~ "冷側・寒側",
        .data[[x_col]] >= 0 & .data[[y_col]] > 0 ~ "暖側・暖側",
        TRUE                                     ~ "暖側・寒側"
      )
    )
}

summarise_fisher_generic <- function(df_fisher, x_col, y_col){
  
  tab <- table(df_fisher$cold_x, df_fisher$warm_y)
  ft  <- fisher.test(tab)
  ct  <- cor.test(df_fisher[[x_col]], df_fisher[[y_col]], method = "pearson")
  
  quad_counts <- df_fisher %>%
    count(quad) %>%
    mutate(
      x = c(-2.2, -2.2,  2.2,  2.2),
      y = c( 1.55, -1.55, 1.55, -1.55)
    )
  
  lab_stats <- paste0(
    "Pearson's r = ", sprintf("%.2f", unname(ct$estimate)), "\n",
    "p = ", sprintf("%.3f", ct$p.value), "\n",
    "n = ", nrow(df_fisher), "\n\n",
    "Fisher exact test\n",
    "p = ", sprintf("%.3f", ft$p.value)
  )
  
  list(
    tab = tab,
    fisher = ft,
    cor = ct,
    quad_counts = quad_counts,
    lab_stats = lab_stats
  )
}

summarise_fisher_by_enso_generic <- function(df_fisher, enso_col = "ENSO_next_winter"){
  df_fisher %>%
    group_by(.data[[enso_col]]) %>%
    group_modify(~{
      tab <- table(.x$cold_x, .x$warm_y)
      tibble(
        p_value = fisher.test(tab)$p.value,
        n = sum(tab)
      )
    }) %>%
    ungroup()
}

# =========================================================
# 汎用：Fisher散布図（修正版）
# =========================================================

plot_fisher_scatter_generic <- function(
    df_fisher,
    fisher_summary,
    x_col,
    y_col,
    enso_col = "ENSO_next_winter",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    quad_labels = c("冷側・暖側", "冷側・寒側", "暖側・暖側", "暖側・寒側"),
    legend_position = "right",
    xlim = c(-3, 3),
    ylim = c(-2, 2)
){
  # ★ここが足りていませんでした：座標の幅を計算
  dx <- diff(xlim)
  dy <- diff(ylim)
  
  p <- ggplot(df_fisher, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    annotate(
      "rect",
      xmin = -Inf, xmax = 0,
      ymin = 0, ymax = Inf,
      alpha = 0.08,
      fill = "orange"
    ) +
    # ゼロ軸（少し目立たせる）
    geom_vline(xintercept = 0, linetype = 2, color = "grey40", linewidth = 0.5) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey40", linewidth = 0.5) +
    geom_point(
      aes(color = .data[[enso_col]], shape = .data[[enso_col]]),
      size = 4, alpha = 0.85 # スライド用に点を大きく
    ) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
    
    # 象限ラベル（TEXT_SIZE_MMを使用して特大化）
    annotate("text", 
             x = xlim[1] + 0.05 * dx, y = ylim[2] - 0.08 * dy,
             label = quad_labels[1], size = TEXT_SIZE_MM, hjust = 0, vjust = 1, fontface = "bold") +
    annotate("text", 
             x = xlim[1] + 0.05 * dx, y = ylim[1] + 0.08 * dy,
             label = quad_labels[2], size = TEXT_SIZE_MM, hjust = 0, vjust = 0, fontface = "bold") +
    annotate("text", 
             x = xlim[2] - 0.05 * dx, y = ylim[2] - 0.08 * dy,
             label = quad_labels[3], size = TEXT_SIZE_MM, hjust = 1, vjust = 1, fontface = "bold") +
    annotate("text",
             x = xlim[2] - 0.05 * dx, y = ylim[1] + 0.08 * dy,
             label = quad_labels[4], size = TEXT_SIZE_MM, hjust = 1, vjust = 0, fontface = "bold") +
    
    # 各象限の件数 (n=...)
    geom_text(
      data = fisher_summary$quad_counts,
      aes(x = x, y = y, label = paste0("n = ", n)),
      inherit.aes = FALSE,
      size = TEXT_SIZE_MM * 1.1,
      fontface = "bold"
    ) +
    
    # 図全体の統計量
    annotate(
      "text",
      x = xlim[1] + 0.05 * dx,
      y = ylim[2] - 0.25 * dy,
      label = fisher_summary$lab_stats,
      hjust = 0,
      vjust = 1,
      size = TEXT_SIZE_MM * 0.8,
      lineheight = 0.8,
      color = "grey20"
    ) +
    
    scale_enso() +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_presentation(legend_position = legend_position)
  
  return(p)
}

plot_fisher_scatter_marginal <- function(p){
  ggMarginal(
    p,
    type = "density",
    size = 6,
    groupColour = TRUE,
    groupFill = FALSE
  )
}

# =========================================================
# 実行
# =========================================================

# 1. 前処理
df2 <- build_analysis_data(stn)

# 2. 必要列を detrend
cols_to_detrend <- c("夏平均気温", "秋平均気温", "次の冬平均気温")
df3 <- add_detrended_cols(df2, cols_to_detrend)

# ---------------------------------------------------------
# 例1：夏 → 次の冬
# ---------------------------------------------------------
x_col <- "夏平均気温_dt"
y_col <- "次の冬平均気温_dt"

p_facet_summer <- plot_scatter_facet_generic(
  df = df3,
  x_col = x_col,
  y_col = y_col,
  enso_col = "ENSO_next_winter",
  title = paste("夏平均気温と次の冬平均気温の関係", stn),
  subtitle = "ENSO別散布図（長期トレンド除去後）",
  xlab = "夏平均気温偏差（トレンド除去後）",
  ylab = "次の冬平均気温偏差（トレンド除去後）",
  xlim = c(-3, 3),
  ylim = c(-2, 2),
)

p_facet_summer

ggsave(
  file.path(OUTPUT_DIR, paste0("夏_次冬_facet_", stn, ".png")),
  p_facet_summer, width = paper_width, height = paper_height, dpi = 300
)

df_fisher_summer <- build_fisher_data_generic(df3, x_col, y_col)
fisher_sum_summer <- summarise_fisher_generic(df_fisher_summer, x_col, y_col)
fisher_by_enso_summer <- summarise_fisher_by_enso_generic(df_fisher_summer)

print(fisher_sum_summer$tab)
print(fisher_sum_summer$fisher)
print(fisher_sum_summer$cor)
print(fisher_by_enso_summer)

p_summer <- plot_fisher_scatter_generic(
  df_fisher = df_fisher_summer,
  fisher_summary = fisher_sum_summer,
  x_col = x_col,
  y_col = y_col,
  title = paste("冷夏・暖冬の関係", stn),
  subtitle = "長期トレンド除去後の夏偏差と次冬偏差",
  xlab = "夏平均気温偏差（トレンド除去後）",
  ylab = "次の冬平均気温偏差（トレンド除去後）",
  quad_labels = c("冷夏・暖冬", "冷夏・寒冬", "暑夏・暖冬", "暑夏・寒冬"),
  legend_position = "right",
  xlim = c(-3, 3),
  ylim = c(-2, 2)
)

p_summer

ggsave(
  file.path(OUTPUT_DIR, paste0("夏_次冬_fisher_", stn, ".png")),
  p_summer, width = paper_width, height = paper_height, dpi = 300
)

#p_summer_marginal <- plot_fisher_scatter_marginal(p_summer)
#
#p_summer_marginal
#
#ggsave(
#  file.path(OUTPUT_DIR, paste0("夏_次冬_fisher_marginal_", stn, ".png")),
#  p_summer_marginal, width = 12, height = 6.75, dpi = 300
#)

# ---------------------------------------------------------
# 例2：秋 → 次の冬
# ---------------------------------------------------------
x_col2 <- "秋平均気温_dt"
y_col2 <- "次の冬平均気温_dt"

p_facet_autumn <- plot_scatter_facet_generic(
  df = df3,
  x_col = x_col2,
  y_col = y_col2,
  enso_col = "ENSO_next_winter",
  title = paste("秋平均気温と次の冬平均気温の関係", stn),
  subtitle = "ENSO別散布図（長期トレンド除去後）",
  xlab = "秋平均気温偏差（トレンド除去後）",
  ylab = "次の冬平均気温偏差（トレンド除去後）",
  xlim = c(-3, 3),
  ylim = c(-2, 2),
  show_legend = FALSE
)

p_facet_autumn

ggsave(
  file.path(OUTPUT_DIR, paste0("秋_次冬_facet_", stn, ".png")),
  p_facet_autumn, width = paper_width, height = paper_height, dpi = 300
)

df_fisher_autumn <- build_fisher_data_generic(df3, x_col2, y_col2)
fisher_sum_autumn <- summarise_fisher_generic(df_fisher_autumn, x_col2, y_col2)
fisher_by_enso_autumn <- summarise_fisher_by_enso_generic(df_fisher_autumn)

print(fisher_sum_autumn$tab)
print(fisher_sum_autumn$fisher)
print(fisher_sum_autumn$cor)
print(fisher_by_enso_autumn)

p_autumn <- plot_fisher_scatter_generic(
  df_fisher = df_fisher_autumn,
  fisher_summary = fisher_sum_autumn,
  x_col = x_col2,
  y_col = y_col2,
  title = paste("高温秋・暖冬の関係", stn),
  subtitle = "長期トレンド除去後の秋偏差と次冬偏差",
  xlab = "秋平均気温偏差（トレンド除去後）",
  ylab = "次の冬平均気温偏差（トレンド除去後）",
  quad_labels = c("低温秋・暖冬", "低温秋・寒冬", "高温秋・暖冬", "高温秋・寒冬"),
  legend_position = "right",
  xlim = c(-3, 3),
  ylim = c(-2, 2)
)

p_autumn

ggsave(
  file.path(OUTPUT_DIR, paste0("秋_次冬_fisher_", stn, ".png")),
  p_autumn, width = paper_width, height = paper_height, dpi = 300
)

