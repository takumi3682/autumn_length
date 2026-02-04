library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

df <- read_csv("data/raw/生物季節観測_仙台_DOY_Temp.csv")
df
# 日本語が豆腐になるのを避けたい場合（必要なら）
library(showtext)
library(sysfonts)
font_add("jp", "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc")
showtext_auto()

doy_to_md <- function(doy) {
  as.Date(doy - 1, origin = "2001-01-01") |>
    format("%m/%d")
}

doy_to_month <- function(doy) {
  as.Date(doy - 1, origin = "2001-01-01") |>
    format("%m") |>         # "06", "07", ...
    paste0("月")
}

# ---- データを縦長にする（2系列をまとめる） ----
df_long <- df %>%
  select(
    年,
    うめ開花,
    さくら開花,
    さくら満開,
    あじさい開花,
    すすき開花,
    いちょう黄葉,
    いちょう落葉,
    かえで紅葉,
    かえで落葉
  ) %>%
  pivot_longer(
    -年,
    names_to = "項目",
    values_to = "DOY"
  ) %>%
  filter(is.finite(DOY))

df_long2 <- df_long %>%
  filter(is.finite(DOY)) %>%
  filter(between(DOY, 150, 350))

df_long %>%
  summarise(
    n_total = n(),
    n_na = sum(is.na(DOY)),
    n_finite = sum(is.finite(DOY)),
    n_in_100_350 = sum(is.finite(DOY) & DOY >= 150 & DOY <= 350)
  )

# -------- 2×
theme_big <- theme_bw() +
  theme(
    plot.title   = element_text(size = 24, face = "bold"),
    axis.title   = element_text(size = 20),
    axis.text    = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 18),
    strip.text   = element_text(size = 18),
    theme(legend.position = "bottom")
  )

library(ggplot2)
library(dplyr)
library(patchwork)
library(mblm)

sen_coef <- df_long |>
  mutate(
    年  = as.numeric(年),
    DOY = as.numeric(DOY)
  ) |>
  filter(!is.na(年), !is.na(DOY)) |>
  group_by(項目) |>
  group_modify(~{
    d <- as.data.frame(.x[, c("年", "DOY")])   # ← 必ず data.frame 化
    if (nrow(d) < 2) {
      return(tibble(slope = NA_real_, intercept = NA_real_))
    }
    m <- mblm::mblm(DOY ~ 年, data = d, repeated = FALSE)
    cf <- coef(m)
    tibble(
      intercept = unname(cf[1]),
      slope     = unname(cf[2])
    )
  }) |>
  ungroup()


year_min <- min(df_long$年, na.rm = TRUE)
year_max <- max(df_long$年, na.rm = TRUE)
x_breaks <- seq(1950, year_max, by = 10)

grp1 <- c("うめ開花","さくら開花","さくら満開")
grp2 <- c("あじさい開花","すすき開花")
grp3 <- c("いちょう黄葉","いちょう落葉")
grp4 <- c("かえで紅葉","かえで落葉")

plot_group <- function(data, vars, title, xlim, breaks) {
  ggplot(
    data |> dplyr::filter(項目 %in% vars),
    aes(x = 年, y = DOY, color = 項目)
  ) +
    geom_point(alpha = 0.7) +
#    geom_smooth(se = FALSE, method = "loess") +
    geom_abline(
      data = sen_coef |> filter(項目 %in% vars),
      aes(slope=slope, intercept = intercept, color=項目),
      linewidth=0.9,
      linetype="dashed",
      inherit.aes = FALSE
    ) +
    scale_x_continuous(limits = xlim, breaks = breaks) +
    labs(title = title, x = "Year", y = "DOY", color = NULL) +
    theme_big
}

p1 <- plot_group(df_long, grp1, "うめ・さくら", xlim = c(year_min, year_max), breaks = x_breaks)
p2 <- plot_group(df_long, grp2, "あじさい・すすき", xlim = c(year_min, year_max), breaks = x_breaks)
p3 <- plot_group(df_long, grp3, "いちょう",       xlim = c(year_min, year_max), breaks = x_breaks)
p4 <- plot_group(df_long, grp4, "かえで",         xlim = c(year_min, year_max), breaks = x_breaks)

p_all <- wrap_plots(p1, p2, p3, p4, ncol = 2)

ggsave(
  filename = "outputs/phenology_timeseries_2x2.png",
  plot     = p_all,
  width    = 10,     # inch
  height   = 8,      # inch
  dpi      = 300
)


# --------------


p <- ggplot(df_long2, aes(x = 年, y = DOY, color = 項目, shape = 項目)) +
  geom_point(size = 2) +
  labs(title="生物季節観測 [仙台]") +
  scale_color_manual(values = c(
#    "すすき開花"   = "green3",
    "いちょう黄葉" = "red",
    "いちょう落葉" = "darkred",
    "かえで紅葉"   = "orange",
    "かえで落葉"   = "blue"
  )) +
  scale_shape_manual(values = c(
#    "すすき開花"   = 1,
    "いちょう黄葉" = 2,
    "いちょう落葉" = 5,
    "かえで紅葉"   = 0,
    "かえで落葉"   = 4
  )) +
# coord_cartesian(ylim = c(290, 350)) +
  scale_x_continuous(
    breaks = seq(
      floor(min(df_long2$年, na.rm = TRUE) / 10) * 10,
      ceiling(max(df_long2$年, na.rm = TRUE) / 10) * 10,
      by = 10
    )
  ) +
#  scale_y_continuous(
#    limits = c(290, 350),
#    breaks = seq(290, 350, by = 30),
#    labels = doy_to_md
    
    scale_y_continuous(
      limits = c(270, 360),
      breaks = c(274, 305, 335),  # 各月の代表日
      labels = doy_to_month
    ) +
    
labs(x = "年", y = "月") +
  theme_minimal(base_family = "jp") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    
    # 凡例を枠の中へ
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      colour = "black",
      linewidth = 0.5
    ) 
)
print(p)

# ---- PNG保存（raggを使う）----
ragg::agg_png("outputs/生物季節観測結果.png", width = 1200, height = 800, res = 144)
print(p)
dev.off()

#print(df$年)

#d <- df[complete.cases(df$年, df$さくら開花, df$さくら満開), ]
#print(d)
plot(df$年, df$すすき開花, type="p", ylim=c(200,350),
     xlab="Year", ylab="Days of Year", col="green", pch=1)
points(df$年, df$いちょう黄葉, col="red", pch=2)
ragg::agg_png("fig.png", width=1200, height=800, res=144)
par(family="jp")
plot()
dev.off()

plot(df$年, df$いちょう黄葉-df$すすき開花,
     xlab="Year", ylab="いちょう黄葉 − すすき開花 (DOY)", col="blue")

plot(df$年, df$いちょう落葉-df$いちょう黄葉, ylim=c(0,40),
     xlab="Year", ylab="黄葉／紅葉 (DOY)", col="red", pch=16)
points(df$年, df$かえで落葉-df$かえで紅葉,
     col="skyblue", pch=17)

legend("topright",
       legend = c("いちょう黄葉期間", "かえで紅葉期間"),
       pch = c(16, 17),
       pt.bg = c("red", "skyblue"),
       col = c("red", "skyblue"),
       bty = "n")   # 枠線なし（好み）
