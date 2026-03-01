library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(knitr)
library(glue)

stn = "仙台"

get_proj_dir <- function() {
#  file.path(getwd(), "analysis-autumn_length")
  file.path(getwd())
}

proj_dir <- get_proj_dir()

# ---- ソースデータを読み込む
data_fp = file.path(proj_dir, "data")
print(data_fp)
df <- readr::read_csv(file.path(data_fp, "raw", glue("生物季節観測_{stn}_DOY_Temp.csv")))

months <- paste0(1:12, "月")

df2 <- df %>%
  mutate(across(all_of(months), ~ parse_number(as.character(.x))))

df2 <- df2 %>%
  mutate(
    春 = rowMeans(across(c(`3月`,`4月`,`5月`)), na.rm = TRUE),
    夏 = rowMeans(across(c(`6月`,`7月`,`8月`)), na.rm = TRUE),
    秋 = rowMeans(across(c(`9月`,`10月`,`11月`)), na.rm = TRUE),
    冬 = rowMeans(across(c(`12月`,`1月`,`2月`)), na.rm = TRUE)
  )

# *** RDS保存 ***
saveRDS(df2, file.path(data_fp, "processed", glue("df2_{stn}.rds")))

# 解析対象
var1 <- "すすき開花"
var2 <- "夏"

df2 <- df2 %>%
  dplyr::select(年, dplyr::all_of(c(var1, var2))) %>%
  dplyr::filter(complete.cases(.))

# 基本相関
cor(df2[[var1]], df2[[var2]], method="spearman")

# 線形トレンド除去後の相関
res1 <- resid(lm(df2[[var1]] ~ df2$年))
res2 <- resid(lm(df2[[var2]] ~ df2$年))

cor(res1, res2, method="spearman")

# Sen's slope除去後の相関（ロバスト）

library(mblm)

fit1 <- mblm(reformulate("年", response= var1), data=df2)
fit2 <- mblm(reformulate("年", response= var2), data=df2)

res1 <- df2[[var1]] - predict(fit1)
res2 <- df2[[var2]] - predict(fit2)

cor(res1, res2, method="spearman", use="complete.obs")

# 多変量回帰
#summary(lm(var1 ~ var2 + 年, data=df2))
model <- lm(reformulate(c(var2, "年"), response = var1), data = df2)
tidy(model) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  kable()

#　図化　残差vs残差
library(ggplot2)
# 日本語が豆腐になるのを避けたい場合（必要なら）
library(showtext)
library(sysfonts)
font_add("jp", "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc")
showtext_auto()

df_res <- data.frame(
  x = res1,
  y = res2
)
cor.test(res1, res2, method="spearman")
summary(lm(res2 ~ res1))
p <- ggplot(df_res, aes(x, y)) +
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
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(
    x = paste(var1, "残差"),
    y = paste(var2, "残差"),
    title = paste("長期トレンド除去後の相関:", stn)
  )
# ---- PNG保存----
ggsave(glue("outputs/長期トレンド除去後の相関_{stn}.png"),
       plot = p,
       width = 13,
       height = 7.5,
       dpi = 150)

