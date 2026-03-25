library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)
library(here)
library(glue)
library(showtext)
showtext_auto()

#========================================
# 保存先
#========================================
OUTPUT_DIR <- here("outputs")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

#========================================
# 1. データ読み込み関数
#========================================
read_temp_parquet <- function(station_id,
                              base_dir = "/Volumes/Transcend/projects/MetData/JMA") {
  
  parquet_file <- file.path(
    base_dir,
    "daily_monthly",
    station_id,
    paste0(station_id, "_temp.parquet")
  )
  
  read_parquet(parquet_file) |>
    mutate(
      date  = as.Date(date),
      year  = year(date),
      month = month(date)
    )
}

#========================================
# 2. 保存関数
#========================================
save_plot_png <- function(plot_obj, filename,
                          output_dir = OUTPUT_DIR,
                          width = paper_width,
                          height = paper_height,
                          dpi = 300) {
  
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}

#========================================
# 3. 4季節ラベル付与
#========================================
add_4season <- function(data) {
  data |>
    mutate(
      season4 = case_when(
        month %in% c(3, 4, 5)   ~ "Spring",
        month %in% c(6, 7, 8)   ~ "Summer",
        month %in% c(9, 10, 11) ~ "Autumn",
        month %in% c(12, 1, 2)  ~ "Winter"
      ),
      season4 = factor(
        season4,
        levels = c("Spring", "Summer", "Autumn", "Winter")
      )
    )
}

#========================================
# 4. 単年ヒストグラム
#========================================
plot_hist_single_year <- function(data,
                                  year_target,
                                  station_name = NULL,
                                  binwidth = 1,
                                  base_size = font_size,
                                  legend_position = "none") {
  
  df_plot <- data |>
    filter(year == year_target)
  
  title_txt <- paste0(year_target, "Density of Daily Mean Temperature")
  if (!is.null(station_name)) {
    title_txt <- paste0(title_txt, " / ", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean)) +
    geom_histogram(binwidth = binwidth, fill = "skyblue", color = "black") +
    labs(
      title = title_txt,
      x = "Daily Mean Temperature (°C)",
      y = "Frequency"
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position) +
    theme(
      panel.grid.major = element_blank()
    )
}

#========================================
# 5. 指定期間ヒストグラム
#========================================
plot_hist_period <- function(data,
                             start_year,
                             end_year,
                             station_name = NULL,
                             bins = 40,
                             base_size = font_size,
                             legend_position = "none") {
  
  df_plot <- data |>
    filter(year >= start_year, year <= end_year)
  
  title_txt <- paste0(start_year, "–", end_year, " Daily Mean Temperature Distribution")
  if (!is.null(station_name)) {
    title_txt <- paste0(title_txt, " / ", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean)) +
    geom_histogram(bins = bins, fill = "skyblue", color = "black") +
    labs(
      title = title_txt,
      x = "Daily Mean Temperature (°C)",
      y = "Frequency"
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position) +
    theme(
      panel.grid.major = element_blank()
    )
}

#========================================
# 6. 指定期間KDE
#========================================
plot_kde_period <- function(data,
                            start_year,
                            end_year,
                            station_name = NULL,
                            adjust = 0.8,
                            base_size = font_size,
                            legend_position = "none") {
  
  df_plot <- data |>
    filter(year >= start_year, year <= end_year)
  
#  title_txt <- paste0(start_year, "–", end_year, "年 日平均気温 KDE")
  title_txt <- paste0(start_year, "–", end_year, " KDE of Daily Mean Temperature")
  if (!is.null(station_name)) {
    title_txt <- paste0(title_txt, " / ", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean)) +
    geom_density(adjust = adjust, fill = "skyblue", alpha = 0.4) +
    labs(
      title = title_txt,
#      x = "日平均気温 (°C)",
#      y = "密度"
      x = "Daily Mean Temperature (°C)",
      y = "Density"
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position)
}

#========================================
# 7. 暖季・寒季＋通年KDE
#========================================
plot_kde_warm_cold_total <- function(data,
                                     start_year,
                                     end_year,
                                     station_name = NULL,
                                     adjust = 0.9,
                                     base_size = font_size,
                                     legend_position = c(0.83, 0.90)) {
  
  df_plot <- data |>
    filter(year >= start_year, year <= end_year) |>
    mutate(
#      season2 = ifelse(month >= 5 & month <= 10, "暖季(5–10月)", "寒季(11–4月)")
      season2 = ifelse(month >= 5 & month <= 10, 
                       "Warm Season (May-Oct)", 
                       "Cold Season (Nov–Apr)")
    )
  
#  title_txt <- paste0(start_year, "–", end_year, "年 日平均気温 KDE")
  title_txt <- paste0(start_year, "–", end_year, " KDE of Daily Mean Temperature")
  if (!is.null(station_name)) {
    title_txt <- paste0(title_txt, " / ", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean)) +
    geom_density(
      aes(fill = season2, color = season2),
      alpha = 0.25,
      linewidth = 1.1,
      adjust = adjust
    ) +
    geom_density(
      color = "black",
      linewidth = 1.4,
      adjust = adjust
    ) +
    scale_fill_manual(values = c(
#      "暖季(5–10月)" = "red",
#      "寒季(11–4月)" = "blue"
      "Warm Season (May-Oct)" = "red",
      "Cold Season (Nov–Apr)" = "blue"
    )) +
    scale_color_manual(values = c(
#      "暖季(5–10月)" = "red",
#      "寒季(11–4月)" = "blue"
      "Warm Season (May-Oct)" = "red",
      "Cold Season (Nov–Apr)" = "blue"
    )) +
    labs(
      title = title_txt,
#      x = "日平均気温 (°C)",
#      y = "密度",
      x = "Daily Mean Temperature (°C)",
      y = "Density",
      fill = NULL,
      color = NULL
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position) +
    theme(
      legend.background = element_rect(fill = alpha("white", 0.5), color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.key.size = unit(1.2, "lines")
    )
}

#========================================
# 8. 2期間比較KDE
#========================================
plot_kde_two_periods <- function(data,
                                 start1,
                                 end1,
                                 start2,
                                 end2,
                                 station_name = NULL,
                                 adjust = 0.9,
                                 base_size = font_size,
                                 legend_position = c(0.85, 0.90)) {
  
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
  
#  title_txt <- "日平均気温 KDE 比較"
  title_txt <- "KDE Comparison of Daily Mean Temperature"
  if (!is.null(station_name)) {
    title_txt <- paste0(title_txt, " / ", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean, fill = period, color = period)) +
    geom_density(alpha = 0.20, linewidth = 1.1, adjust = adjust) +
    labs(
      title = title_txt,
#      x = "日平均気温 (°C)",
#      y = "密度",
      x = "Daily Mean Temperature (°C)",
      y = "Density",
      fill = NULL,
      color = NULL
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position) +
    theme(
      legend.background = element_rect(fill = alpha("white", 0.5), color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.key.size = unit(1.2, "lines")
    )
}

#========================================
# 9. 春夏秋冬KDE
#========================================
plot_kde_four_seasons <- function(data,
                                  station_name = NULL,
                                  base_size = font_size,
                                  legend_position = c(0.82, 0.82)) {
  df_plot <- data |>
    add_4season() |>
    filter(!is.na(season4), is.finite(temp_mean))

  title_txt <- "Seasonal KDE of Daily Mean Temperature"
  if (!is.null(station_name)) {
    title_txt <- paste(title_txt, "/", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean, color = season4, fill = season4)) +
    geom_density(alpha = 0.2, linewidth = 1) +
    labs(
      title = title_txt,
      x = "Daily Mean Temperature (°C)",
      y = "Density",
      color = NULL,
      fill = NULL
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position) +
    theme(
      legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
      legend.key.size = unit(1.2, "lines")
    )
}

#========================================
# 10. 春夏秋冬 × 2期間比較
#========================================
plot_kde_four_seasons_compare <- function(data,
                                          start1,
                                          end1,
                                          start2,
                                          end2,
                                          station_name = NULL,
                                          base_size = font_size,
                                          legend_position = c(0.85, 0.12)) {
  
  label1 <- paste0(start1, "-", end1)
  label2 <- paste0(start2, "-", end2)
  
  df_plot <- data |>
    add_4season() |>
    mutate(
      period = case_when(
        year >= start1 & year <= end1 ~ label1,
        year >= start2 & year <= end2 ~ label2,
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period))
  
  title_txt <- "Seasonal KDE Comparison by Period"
  if (!is.null(station_name)) {
    title_txt <- paste(title_txt, "/", station_name)
  }
  
  ggplot(df_plot, aes(x = temp_mean, color = period, fill = period)) +
    geom_density(alpha = 0.2, linewidth = 1) +
    facet_wrap(~ season4, ncol = 2, scales = "fixed") +
    labs(
      title = title_txt,
      x = "Daily Mean Temperature (°C)",
      y = "Density",
      color = NULL,
      fill = NULL
    ) +
    theme_presentation(base_size = base_size, legend_position = legend_position) +
    theme(
      legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
      legend.key.size = unit(1.2, "lines")
    )
}

source(here("notebooks", "r", "theme.R"))

save_png <- function(p, file_name, paper_width, paper_height)
{
  ggsave(
    here(OUTPUT_DIR, file_name),
    p, width = paper_width, height = paper_height, dpi = 300
  )
}

station_id   <- "b47590"
station_name <- "仙台"
station_name_e <- "Sendai"

df <- read_temp_parquet(station_id)

p1 <- plot_hist_single_year(df, 2020, station_name)
p1

p2 <- plot_hist_period(df, 1991, 2020, station_name)
p2

p3 <- plot_kde_period(df, 1991, 2020, station_name_e)
p3
save_png(p3, glue(station_name, "_Tmean_KDE_1991-2020.png"), paper_width, paper_height)

p4 <- plot_kde_warm_cold_total(df, 1991, 2020, station_name_e)
p4
save_png(p4, glue(station_name, "_Tmean_KDE_warm-cold_1991-2020.png"), paper_width, paper_height)

p5 <- plot_kde_two_periods(df, 1961, 1990, 1991, 2020, station_name_e)
p5
save_png(p5, glue(station_name, "_Tmean_KDE_1961-1990_vs_1991-2020.png"), paper_width, paper_height)

p6 <- plot_kde_four_seasons(df, station_name_e)
p6
save_png(p6, glue(station_name, "_Tmean_KDE_4seasons_1991-2020.png"), paper_width, paper_height)

p7 <- plot_kde_four_seasons_compare(df, 1961, 1990, 1991, 2020, station_name_e)
p7
save_png(p7, glue(station_name, "_Tmean_KDE_warm-cold_4panel_1991-2020.png"), paper_width, paper_height)
