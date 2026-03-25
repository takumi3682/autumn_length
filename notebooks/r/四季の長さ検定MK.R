library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(modifiedmk)
library(glue)

DATA_DIR <- here("data", "processed")
OUTPUT_DIR <- here("outputs")
print(DATA_DIR)

# 仙台:b47590、東京:b47662、大阪:b47772、福岡:b47807
station_name = "FUKUOKA"

df <- read.csv(here(DATA_DIR, "b47807_4season_1982_2022.csv"))

# 1. 縦長に変換
df_long <- df %>%
  select(year, spring_days, summer_days, autumn_days, winter_days) %>%
  pivot_longer(
    cols = -year,
    names_to = "season",
    values_to = "days"
  ) %>%
  mutate(
    season = factor(
      season,
      levels = c("spring_days", "summer_days", "autumn_days", "winter_days"),
      labels = c("Spring", "Summer", "Autumn", "Winter")
    )
  )

# 2. 季節ごとに modified MK
mk_result <- df_long %>%
  group_by(season) %>%
  summarise(
    tau   = unname(mmkh(days)[["Tau"]]),
    p     = unname(mmkh(days)[["new P-value"]]),
    slope = unname(mmkh(days)[["Sen's slope"]]),
    x_pos = min(year, na.rm = TRUE) + 1,
    y_pos = max(days, na.rm = TRUE) - 1,
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("tau = %.2f\np = %.3g\nSen's slope = %.2f day/yr",
                    tau, p, slope)
  )

# 3. 図
p <- ggplot(df_long, aes(x = year, y = days)) +
  geom_point(size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ season, ncol = 2, scales = "free_y") +
  geom_text(
    data = mk_result,
    aes(x = x_pos, y = y_pos, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3.5
  ) +
  labs(
    title = paste("Trends in Seasonal Length /", station_name),
    x = "Year",
    y = "Length (days)"
  ) +
  theme_bw(base_size = 12)

# ---- PNG保存 ----
ggsave(
  filename = file.path(
    OUTPUT_DIR, 
    glue("{station_name}_season_length_trend.png")
  ),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

print(p)

mk_table <- df_long %>%
  group_by(season) %>%
  summarise(
    tau   = unname(mmkh(days)[["Tau"]]),
    p     = unname(mmkh(days)[["new P-value"]]),
    slope = unname(mmkh(days)[["Sen's slope"]]),
    .groups = "drop"
  )

print(mk_table)




