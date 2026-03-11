library(dplyr)
library(readr)
library(stringr)
library(here)
stn <- "仙台"

DATA_DIR <- here("data", "processed")
OUTPUT_DIR <- here("outputs")
print(DATA_DIR)

# =========================
# 1 CSV読み込み
# =========================

df <- read_csv(
  file.path("data", "raw",
            paste0("生物季節観測_", stn, "_DOY_Temp.csv"))
)
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
# 4 ENSO列作成
# =========================

enso_tbl <- tibble(
  年 = unique(df$年)
) %>%
  mutate(
    ENSO_summer = sapply(年, get_enso, season="summer"),
    ENSO_winter = sapply(年, get_enso, season="winter"),
    ENSO_next_winter = lead(ENSO_winter),
  )

# =========================
# 5 元データに追加
# =========================

df2 <- df %>%
  left_join(enso_tbl, by="年") %>%
  relocate(ENSO_summer, ENSO_winter, ENSO_next_winter, .after=年)

df2 <- df2 %>%
  arrange(年) %>%
  mutate(
    ENSO_next_winter = lead(ENSO_winter)
  )

df2 <- df2 %>%
  mutate(
    across(matches("月$"),
           ~as.numeric(str_extract(.x, "[0-9.]+")))
  )


df2 <- df2 %>%
  arrange(年) %>%
  mutate(
    春平均気温 = rowMeans(across(c(`3月`,`4月`,`5月`)), na.rm=TRUE),
    夏平均気温 = rowMeans(across(c(`6月`,`7月`,`8月`)), na.rm=TRUE),
    秋平均気温 = rowMeans(across(c(`9月`,`10月`,`11月`)), na.rm=TRUE),
    冬平均気温 = rowMeans(cbind(lag(`12月`), `1月`, `2月`), na.rm=TRUE),
    次の冬平均気温 = rowMeans(cbind(`12月`, lead(`1月`), lead(`2月`)), na.rm = TRUE)
  )

# =========================
# 6 保存
# =========================

out_file <- file.path("data", "processed",
          paste0("生物季節観測_", stn, "_DOY_Temp_ENSO.csv"))
write_csv(df2, out_file)

# =========================
# 以下、解析
# =========================
# 全体
df2 %>%
  summarise(
    n = sum(complete.cases(夏平均気温, 次の冬平均気温)),
    r_pearson = cor(夏平均気温, 次の冬平均気温, use = "complete.obs"),
    r_spearman = cor(夏平均気温, 次の冬平均気温, use = "complete.obs", method = "spearman")
  )

cor.test(
  df2$夏平均気温,
  df2$次の冬平均気温,
  use = "complete.obs",
  method = "spearman"
)
# 次の冬　ENSO別
df2 %>%
  group_by(ENSO_next_winter) %>%
  summarise(
    n = sum(complete.cases(夏平均気温, 次の冬平均気温)),
    r_pearson = cor(夏平均気温, 次の冬平均気温, use = "complete.obs"),
    r_spearman = cor(夏平均気温, 次の冬平均気温, use = "complete.obs", method = "spearman")
  )

# 夏 ENSO別
df2 %>%
  group_by(ENSO_summer) %>%
  summarise(
    n = sum(complete.cases(夏平均気温, 次の冬平均気温)),
    r_pearson = cor(夏平均気温, 次の冬平均気温, use = "complete.obs"),
    r_spearman = cor(夏平均気温, 次の冬平均気温, use = "complete.obs", method = "spearman")
  )


library(ggplot2)
library(showtext)
showtext_auto()

df_plot <- df2 |>
  dplyr::filter(!is.na(ENSO_next_winter))

p <- ggplot(
  df_plot,
  aes(
    x = 夏平均気温, y = 次の冬平均気温, 
    color = ENSO_next_winter,
    shape = ENSO_next_winter
  )
) +
  scale_shape_manual(
      values = c(
        ElNino = 16,   # ●
        LaNina = 17,   # ▲
        Neutral = 15   # ■
      )
  ) + 
  scale_color_brewer(palette = "Dark2") +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.6) +
  labs(
    title = paste("夏平均気温と次の冬平均気温の関係",stn),
    subtitle = "ENSO_next_winter 別の回帰線",
    x = "夏平均気温 (6–8月)",
    y = "次の冬平均気温 (12月–翌2月)",
    color = "次の冬 ENSO",
    shape = "次の冬 ENSO"
  ) +
  theme_bw(base_size=22) +
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    plot.subtitle = element_text(size = 24),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    legend.position = c(0.20, 0.85),
    legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA)
  )
p
ggsave(
  filename = file.path(OUTPUT_DIR, paste0("夏ー冬トレンド_ENSO_",stn,".png")),
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

# ***
library(dplyr)
library(ggplot2)
library(scales)

# ---------------------------------
# 0) 作図データ
# ---------------------------------
df_plot <- df2 %>%
  filter(
    !is.na(夏平均気温),
    !is.na(次の冬平均気温),
    !is.na(ENSO_next_winter)
  )

# ---------------------------------
# 1) 軸範囲をここで指定
#    NULL なら自動
# ---------------------------------
x_min <- NULL
x_max <- NULL
y_min <- NULL
y_max <- NULL

# 例:
# x_min <- 18
# x_max <- 28
# y_min <- 0
# y_max <- 5

# ---------------------------------
# 2) プレゼン用テーマ
# ---------------------------------
theme_presentation <- function(base_size = 20) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size * 1.4, face = "bold"),
      plot.subtitle = element_text(size = base_size * 1.0),
      axis.title = element_text(size = base_size * 1.0),
      axis.text = element_text(size = base_size * 0.82),
      legend.title = element_text(size = base_size * 0.9),
      legend.text = element_text(size = base_size * 0.82),
      legend.position = c(0.18, 0.84),
      legend.background = element_rect(
        fill = alpha("white", 0.65),
        color = NA
      ),
      panel.grid.minor = element_blank()
    )
}

# ---------------------------------
# 3) 図中注記（Pearson の r, p）
#    群ごとに表示
# ---------------------------------
ann <- df_plot %>%
  group_by(ENSO_next_winter) %>%
  summarise(
    n = sum(complete.cases(夏平均気温, 次の冬平均気温)),
    r = cor(夏平均気温, 次の冬平均気温, use = "complete.obs"),
    p = cor.test(夏平均気温, 次の冬平均気温)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      ENSO_next_winter, "\n",
      "r = ", sprintf("%.2f", r), "\n",
      "p = ", sprintf("%.3f", p)
    )
  )

# 注記位置は必要に応じて調整
# 右上に縦に並べる例
x_right <- if (is.null(x_max)) max(df_plot$夏平均気温, na.rm = TRUE) else x_max
y_top   <- if (is.null(y_max)) max(df_plot$次の冬平均気温, na.rm = TRUE) else y_max

ann <- ann %>%
  arrange(desc(ENSO_next_winter)) %>%
  mutate(
    x = x_right,
    y = seq(y_top, y_top - 0.9, length.out = n())
  )

# ---------------------------------
# 4) 図本体
# ---------------------------------
p <- ggplot(
  df_plot,
  aes(
    x = 夏平均気温,
    y = 次の冬平均気温,
    color = ENSO_next_winter,
    shape = ENSO_next_winter
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1.0,
    alpha = 0.18
  ) +
  geom_point(
    size = 3.2,
    alpha = 0.9
  ) +
  geom_text(
    data = ann,
    aes(x = x, y = y, label = label, color = ENSO_next_winter),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 1,
    size = 5.2
  ) +
  scale_shape_manual(
    values = c(
      ElNino = 16,   # ●
      LaNina = 17,   # ▲
      Neutral = 15   # ■
    ),
    na.translate = FALSE
  ) +
  scale_color_manual(
    values = c(
      ElNino = "#d73027",
      LaNina = "#1a9850",
      Neutral = "#4575b4"
    ),
    na.translate = FALSE
  ) +
  labs(
    title = paste("夏平均気温と次の冬平均気温の関係", stn),
    subtitle = "ENSO_next_winter 別の回帰線",
    x = "夏平均気温 (6–8月)",
    y = "次の冬平均気温 (12月–翌2月)",
    color = "次の冬 ENSO",
    shape = "次の冬 ENSO"
  ) +
  coord_cartesian(
    xlim = c(x_min, x_max),
    ylim = c(y_min, y_max)
  ) +
  theme_presentation(base_size = 20)

p

# ---------------------------------
# 5) 保存
# ---------------------------------
ggsave(
  filename = file.path(OUTPUT_DIR, paste0("夏ー冬トレンド_ENSO_", stn, ".png")),
  plot = p,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# ******
# 長期トレンドを除去した場合

df3 <- df2 %>%
  mutate(
    夏_det = resid(lm(夏平均気温 ~ 年)),
    冬_det = resid(lm(次の冬平均気温 ~ 年))
  )

cor.test(
  df3$夏_det,
  df3$冬_det,
  method = "spearman"
)

df3 %>%
  group_by(ENSO_next_winter) %>%
  summarise(
    r = cor(夏_det, 冬_det, use="complete.obs"),
    rho = cor(夏_det, 冬_det, method="spearman", use="complete.obs")
  )

ggplot(df3,
       aes(x = 夏_det, y = 冬_det,
           color = ENSO_next_winter)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  labs(
    title = paste("夏平均気温と次の冬平均気温の関係",stn),
    subtitle = "ENSO_next_winter 別の回帰線　（長期トレンド除去後）",
    x = "夏平均気温 (6–8月)",
    y = "次の冬平均気温 (12月–翌2月)",
    color = "次の冬 ENSO"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.2, 0.9),
    legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA)
  )


