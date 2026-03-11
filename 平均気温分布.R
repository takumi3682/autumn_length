library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)
library(here)
library(showtext)
showtext_auto()

DATA_DIR <- here("data", "processed")
OUTPUT_DIR <- here("outputs")
print(DATA_DIR)

station_id <- "b47590"
station_name <- "仙台"
START <- 1960
END   <- 2025

parquet_file <- file.path(
  "/Volumes/Transcend/projects/MetData/JMA",
  "daily_monthly",
  station_id,
  paste0(station_id, "_temp.parquet")
)

df <- read_parquet(parquet_file) |>
  mutate(
    date = as.Date(date),
    year = year(date)
  ) |>
  filter(year >= START, year <= END)

# YEAR年の日平均気温分布
YEAR <- 2020

df |>
  filter(year == YEAR) |>
  ggplot(aes(x = temp_mean)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = paste0(YEAR, "年の日平均気温分布"),
    x = "日平均気温 (°C)",
    y = "頻度"
  )

# 指定期間（START〜ENDのヒストグラム）

START = 1991
END = 2020

df_period <- df |>
  filter(year >= START, year <= END)

ggplot(df_period, aes(x = temp_mean)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(
    title = paste(START, "–", END, "年日平均気温分布／", station_name),
    x = "日平均気温 (°C)",
    y = "頻度"
  )

# KDE（カーネル密度)

start1 = 1961
end1 = 1990
start2 = 1991
end2 = 2020

ggplot(df_period, aes(x = temp_mean)) +
  geom_density(adjust = 0.8, fill = "skyblue", alpha = 0.4) +
  labs(
    title = paste(START, "–", END, "年　日平均気温　カーネル密度（KDE）／", station_name),
    x = "日平均気温 (°C)",
    y = "密度"
  )

# KDEの「2峰構造」を可視化する
#library(dplyr)
#library(ggplot2)

plot_kde_season_total <- function(data, start_year, end_year, station_name = NULL){
  
  df_plot <- data |>
    filter(year >= start_year, year <= end_year) |>
    mutate(
      season = ifelse(month >= 5 & month <= 10, "暖季(5–10月)", "寒季(11–4月)")
    )
  
  title_txt <- paste0(start_year,"–",end_year,"年 日平均気温 KDE")
  
  if(!is.null(station_name)){
    title_txt <- paste0(title_txt," / ",station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean)) +
    
    # 暖季寒季
    geom_density(
      aes(fill = season, color = season),
      alpha = 0.25,
      linewidth = 1.1,
      adjust = 0.9
    ) +
    
    # 通年
    geom_density(
      color = "black",
      linewidth = 1.4,
      adjust = 0.9
    ) +
    
    scale_fill_manual(values = c(
      "暖季(5–10月)" = "red",
      "寒季(11–4月)" = "blue"
    )) +
    
    scale_color_manual(values = c(
      "暖季(5–10月)" = "red",
      "寒季(11–4月)" = "blue"
    )) +
    
    labs(
      title = title_txt,
      x = "日平均気温 (°C)",
      y = "密度",
      fill = NULL,
      color = NULL
    ) +
    
    theme_bw(base_size = 16) +
    theme(legend.position = c(0.83, 0.90)) 
}

p <- plot_kde_season_total(df2, 1991, 2020, "仙台")
p
ggsave(
  filename = file.path(OUTPUT_DIR, "kde_warm-cold_end1-end2.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

#　2つの期間のKDEを重ねる
plot_kde_two_periods <- function(data,
                                 start1, end1,
                                 start2, end2,
                                 station_name = NULL) {
  
  label1 <- paste0(start1, "–", end1)
  label2 <- paste0(start2, "–", end2)
  
  df_plot <- data |>
    filter(
      (year >= start1 & year <= end1) |
        (year >= start2 & year <= end2)
    ) |>
    mutate(
      period = case_when(
        year >= start1 & year <= end1 ~ label1,
        year >= start2 & year <= end2 ~ label2
      )
    )
  
  title_txt <- if (is.null(station_name)) {
    "日平均気温 KDE 比較"
  } else {
    paste0("日平均気温 KDE 比較 / ", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean, fill = period, color = period)) +
    geom_density(alpha = 0.20, linewidth = 1.1, adjust = 0.9) +
    labs(
      title = title_txt,
      x = "日平均気温 (°C)",
      y = "密度",
      fill = NULL,
      color = NULL
    ) +
    theme_bw(base_size = 16) +
    theme(
      legend.position = c(0.85,0.9) ,
      legend.background = element_rect(fill="transparent"),
      legend.key = element_rect(fill="transparent")
    )
}

p_all <- plot_kde_two_periods(df2, start1, end1, start2, end2, station_name = station_name)
p_all
ggsave(
  filename = file.path(OUTPUT_DIR, "kde_compare_end1-end2.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

# 春夏秋冬に4分してKDEを描く

#library(dplyr)
#library(ggplot2)
#library(lubridate)

# 例:
# df <- read.csv("daily_temp.csv")
# df$date <- as.Date(df$date)

df_season <- df %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn",
      month %in% c(12, 1, 2)  ~ "Winter"
    )
  )

ggplot(df_season, aes(x = temp_mean, color = season, fill = season)) +
  geom_density(alpha = 0.2, linewidth = 1) +
  labs(
    title = paste("Seasonal KDE of Daily Mean Temperature",station_name),
    x = "Daily Mean Temperature (°C)",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.82, 0.82),
    legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA)
  )

# 期間を2つに分けて比較
#library(dplyr)
#library(ggplot2)
#library(lubridate)

START1 <- 1961
END1   <- 1990
START2 <- 1991
END2   <- 2020

df_compare <- df %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn",
      month %in% c(12, 1, 2)  ~ "Winter"
    ),
    period = case_when(
      year >= START1 & year <= END1 ~ paste0(START1, "-", END1),
      year >= START2 & year <= END2 ~ paste0(START2, "-", END2),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

ggplot(df_compare, aes(x = temp_mean, color = period, fill = period)) +
  geom_density(alpha = 0.2, linewidth = 1) +
  facet_wrap(~ season, ncol = 2, scales = "fixed") +
  labs(
    title = paste("Seasonal KDE Comparison by Period", station_name),
    x = "Daily Mean Temperature (°C)",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.85, 0.12),
    legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA)
  )
