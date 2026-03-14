library(tidyverse)
library(here)
library(glue)
stn <- "仙台"

DATA_DIR <- here("data", "processed")
OUTPUT_DIR <- here("outputs")
print(DATA_DIR)

# =========================
# 1 CSV読み込み
# =========================

df <- read_csv(here("data", "raw", glue("生物季節観測_{stn}_DOY_Temp.csv")))

# =========================
# 2 ENSO期間（気象庁公式）
# =========================

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

# =========================
# 3 年ごとのENSO判定関数
# =========================

get_enso <- function(year, season){
  
  months <- if(season == "summer"){
    c(6,7,8)
  } else{
    c(12,1,2)
  }
  
  state <- "Neutral"
  
  for(i in 1:nrow(enso_periods)){
    
    s <- enso_periods$start[i]
    e <- enso_periods$end[i]
    
    if(year >= s & year <= e){
      state <- enso_periods$type[i]
    }
  }
  
  return(state)
}

# =========================
# 4 ENSO列作成・元データに追加
# =========================

enso_tbl <- tibble(
  年 = unique(df$年)
) %>%
  arrange(年) %>%
  mutate(
    ENSO_summer = map_chr(年, get_enso, season="summer"),
    ENSO_winter = map_chr(年, get_enso, season="winter"),
    ENSO_next_winter = lead(ENSO_winter),
  )
  df2 <- df %>%
    left_join(enso_tbl, by="年") %>%
    relocate(ENSO_summer, ENSO_winter, ENSO_next_winter, .after=年) %>%
    mutate(
      across(matches("月$"),
           ~as.numeric(str_extract(as.character(.x), "[0-9.]+"))
      )
  ) %>%
  mutate(
    春平均気温 = rowMeans(across(c(`3月`,`4月`,`5月`)), na.rm=TRUE),
    夏平均気温 = rowMeans(across(c(`6月`,`7月`,`8月`)), na.rm=TRUE),
    秋平均気温 = rowMeans(across(c(`9月`,`10月`,`11月`)), na.rm=TRUE),
    冬平均気温 = rowMeans(cbind(lag(`12月`), `1月`, `2月`), na.rm=TRUE),
    次の冬平均気温 = rowMeans(cbind(`12月`, lead(`1月`), lead(`2月`)), na.rm = TRUE)
  )

# =========================
# 5 保存
# =========================

out_file <- here("data", "processed", 
                 glue("生物季節観測_{stn}_DOY_Temp_ENSO.csv")
          )
write_csv(df2, out_file)

# =========================
# 以下、解析
# =========================
# 図化オプション

# ***********************
#library(tidyverse)
library(showtext)
showtext_auto()

theme_presentation <- function(base_size = 20){
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(size = 28, face = "bold"),
      plot.subtitle = element_text(size = 20),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      strip.text = element_text(size = 18, face = "bold"),
      legend.position = "none",
      legend.background =
        element_rect(fill = scales::alpha("white", 0.6), color = NA)
    )
}

scale_enso <- function(){
  list(
    scale_color_manual(
      values = c(
        ElNino  = "#d73027",
        LaNina  = "#1a9850",
        Neutral = "#4575b4"
      ),
      na.translate = FALSE
    ),
    scale_shape_manual(
      values = c(
        ElNino = 16,
        LaNina = 17,
        Neutral = 15
      ),
      na.translate = FALSE
    )
  )
}

plot_scatter_facet_enso <- function(
    df,
    x,
    y,
    enso_col = ENSO_next_winter,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    xlim = c(19, 25.5),
    ylim = c(0.8, 5.2),
    method = "pearson"   # "pearson" or "spearman"
){
  # 解析に必要な列だけ残す
  df_plot <- df %>%
    drop_na({{ enso_col }}, {{ x }}, {{ y }}) %>%
    mutate(.enso = {{ enso_col }})
  
  # パネルごとの相関とp値
  ann <- df_plot %>%
    group_by(.enso) %>%
    summarise(
      n = n(),
      r = cor({{ x }}, {{ y }}, method = method),
      p = cor.test({{ x }}, {{ y }}, method = method, exact = FALSE)$p.value,
      .groups = "drop"
    ) %>%
    mutate(
      label = if (method == "spearman") {
        paste0("rho = ", sprintf("%.2f", r), "\n",
               "p = ", sprintf("%.3f", p), "\n",
               "n = ", n)
      } else {
        paste0("r = ", sprintf("%.2f", r), "\n",
               "p = ", sprintf("%.3f", p), "\n",
               "n = ", n)
      },
      # 各パネルの右上に置く
      x_lab = xlim[2] - 0.15,
      y_lab = ylim[2] - 0.15
    )
  
  ggplot(
    df_plot,
    aes(
      x = {{ x }},
      y = {{ y }},
      color = .enso,
      shape = .enso
    )
  ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      alpha = 0.12,
      linewidth = 1.3
    ) +
    geom_point(size = 3) +
    geom_text(
      data = ann,
      aes(x = x_lab, y = y_lab, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 1,
      size = 6
    ) +
    scale_enso() +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab,
      color = "ENSO",
      shape = "ENSO"
    ) +
    coord_cartesian(
      xlim = xlim,
      ylim = ylim
    ) +
    facet_wrap(~ .enso, ncol = 3) +
    theme_presentation()
}

p_facet <- plot_scatter_facet_enso(
  df2,
  夏平均気温,
  次の冬平均気温,
  enso_col = ENSO_next_winter,
  title = paste("夏平均気温と次の冬平均気温の関係", stn),
  subtitle = "ENSO_next_winter ごとの散布図と回帰線",
  xlab = "夏平均気温 (6–8月)",
  ylab = "次の冬平均気温 (12月–翌2月)",
  xlim = c(19, 25.5),
  ylim = c(0.8, 5.2),
  method = "pearson"
)

p_facet

ggsave(
  filename = file.path(OUTPUT_DIR, paste0("夏ー冬トレンド_ENSO_facet_", stn, ".png")),
  plot = p_facet,
  width = 12,
  height = 5,
  dpi = 300
)

df3 <- df2 %>%
  drop_na(ENSO_next_winter, 夏平均気温, 次の冬平均気温, 年) %>%
  mutate(
    夏平均気温_dt = resid(lm(夏平均気温 ~ 年, data = cur_data())),
    次の冬平均気温_dt = resid(lm(次の冬平均気温 ~ 年, data = cur_data()))
  )

p_facet_dt <- plot_scatter_facet_enso(
  df3,
  夏平均気温_dt,
  次の冬平均気温_dt,
  enso_col = ENSO_next_winter,
  title = paste("夏平均気温と次の冬平均気温の関係", stn),
  subtitle = "ENSO_next_winter ごとの散布図と回帰線（長期トレンド除去後）",
  xlab = "夏平均気温偏差（トレンド除去後）",
  ylab = "次の冬平均気温偏差（トレンド除去後）",
  xlim = c(-3, 3),
  ylim = c(-2, 2),
  method = "pearson"
)

p_facet_dt

ggsave(
  filename = file.path(OUTPUT_DIR, paste0("夏ー冬トレンド_ENSO_facet_detrend_", stn, ".png")),
  plot = p_facet_dt,
  width = 12,
  height = 5,
  dpi = 300
)
