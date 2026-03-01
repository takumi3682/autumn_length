library(readr)
library(dplyr)
library(ggplot2)
library(mblm)

# 日本語が豆腐になるのを避けたい場合（必要なら）
library(showtext)
library(sysfonts)
font_add("jp", "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc")
showtext_auto()

get_proj_dir <- function() {
  #  file.path(getwd(), "analysis-autumn_length")
  file.path(getwd())
}

proj_dir <- get_proj_dir()

# ---- ソースデータを読み込む
data_fp = file.path(proj_dir, "data")
print(data_fp)
df1 <- readr::read_csv(file.path(data_fp, "raw", "生物季節観測_仙台_DOY_Temp.csv"))
df2 <- readr::read_csv(file.path(data_fp, "raw", "生物季節観測_山形_DOY_Temp.csv"))

#months <- paste0(1:12, "月")


# グループラベル付与
df1$group <- "仙台"
df2$group <- "山形"

df_all <- bind_rows(df1, df2) |>
  filter(!is.na(`すすき開花`))

# ---- Sen's slope をグループごとに計算 ----
sen_coef <- df_all |>
  group_by(group) |>
  do({
    fit <- mblm(`すすき開花` ~ 年, data = .)
    data.frame(
      intercept = coef(fit)[1],
      slope     = coef(fit)[2]
    )
  })

# ---- 描画 ----
p <- ggplot(df_all,
  aes(x = 年, y = `すすき開花`, color = group)) +
  theme_minimal(base_size = 18)　 +
  geom_point(size = 3) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 18),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill="white", colour="black"),
    legend.title = element_blank(),
    legend.text = element_text(size=16)
  ) +
  geom_abline(data = sen_coef,
              aes(intercept = intercept,
                  slope = slope,
                  color = group),
              linewidth = 1) +
  labs(x = "年",
       y = "すすき開花（DOY）",
       color = "データ",
       title = "すすき開花の年次推移（Sen's slope）") +
  theme(
    legend.position = c(0.85, 0.85),   # 右上（0〜1で指定）
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.title = element_blank()
  )

ggsave("outputs/すすき開花比較.png",
       plot = p,
       width = 13,
       height = 7.5,
       dpi = 150)

