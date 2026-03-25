library(tidyverse)
library(here)
library(glue)
library(showtext)
showtext_auto()

stn <- "東京"
df <- read_csv(here("data", "processed", glue("生物季節観測_{stn}_DOY_Temp_ENSO.csv")),)

source(here("notebooks", "r", "theme.R"))
source(here("notebooks", "r", "scales.R"))
source(here("notebooks", "r", "plot_fisher_quad.R"))

DATA_RAW_DIR  <- here("data", "raw")
DATA_PROC_DIR <- here("data", "processed")
OUTPUT_DIR    <- here("outputs")

# ----------------
# 図作成
# ----------------

file_in <- here("data", "processed", glue("生物季節観測_{stn}_DOY_Temp_ENSO.csv"))
file_in
df <- read_csv(file_in)

df %>%
  summarise(
    n = n(),
    mean_annual = mean(年の値, na.rm = TRUE),
    mean_summer = mean(夏平均気温, na.rm = TRUE),
    mean_next_winter = mean(次の冬平均気温, na.rm = TRUE)
  )

p <- plot_fisher_quad_enso(
  df,
  夏平均気温,
  次の冬平均気温,
  title = "ENSO位相別にみた冷夏・暖冬の関係",
  subtitle = paste("夏季気温と翌冬季気温の四象限解析／", stn),
  xlab = "夏季平均気温（°C）",
  ylab = "冬季平均気温（°C)"
)+
  theme(legend.box.margin = margin(l = -20))


p

ggsave(
  file.path(OUTPUT_DIR, paste0("fisher_quad_enso_", stn, ".png")),
  p, width = paper_width, height = paper_height, dpi = 300
)

