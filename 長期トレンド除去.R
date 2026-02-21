library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(knitr)

get_proj_dir <- function() {
  file.path(getwd(), "analysis-autumn_length")
}

proj_dir <- get_proj_dir()

# ---- ソースデータを読み込む
data_fp = file.path(proj_dir, "data")
print(data_fp)
df <- readr::read_csv(file.path(data_fp, "raw", "生物季節観測_仙台_DOY_Temp.csv"))

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
saveRDS(df2, file.path(data_fp, "processed", "df2.rds"))

# さくら開花と春の気温
# 基本相関
cor(df2$さくら開花, df2$春, method="spearman", use="complete.obs")

# 線形トレンド除去後の相関
res_doy  <- resid(lm(さくら開花 ~ 年, data=df2))
res_spr  <- resid(lm(春 ~ 年, data=df2))

cor(res_doy, res_spr, method="spearman", use="complete.obs")

# Sen's slope除去後の相関（ロバスト）

library(mblm)

fit1 <- mblm(さくら開花 ~ 年, data=df2)
fit2 <- mblm(春 ~ 年, data=df2)

res1 <- df2$さくら開花 - predict(fit1)
res2 <- df2$春 - predict(fit2)

cor(res1, res2, method="spearman", use="complete.obs")

# 多変量回帰
#summary(lm(さくら開花 ~ 春 + 年, data=df2))
model <- lm(さくら開花 ~ 春 + 年, data=df2)
tidy(model) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  kable()


# 夏の気温と冬の気温の相関
# 基本相関
cor(df2$夏, df2$冬, method="spearman", use="complete.obs")

# 線形トレンド除去後の相関
res_sum  <- resid(lm(夏 ~ 年, data=df2))
res_win  <- resid(lm(冬 ~ 年, data=df2))

cor(res_sum, res_win, method="spearman", use="complete.obs")

# Sen's slope除去後の相関（ロバスト）

#library(mblm)

fit1 <- mblm(夏 ~ 年, data=df2)
fit2 <- mblm(冬 ~ 年, data=df2)

res1 <- df2$夏 - predict(fit1)
res2 <- df2$冬 - predict(fit2)

cor(res1, res2, method="spearman", use="complete.obs")

# 多変量回帰
#summary(lm(夏 ~ 冬 + 年, data=df2))
model2 <- lm(夏 ~ 冬 + 年, data=df2)
tidy(model2) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  kable()


# すすき開花と夏の気温の相関
# 基本相関
cor(df2$すすき開花, df2$夏, method="spearman", use="complete.obs")

# 線形トレンド除去後の相関
df_sub <- df2 |>
  dplyr::select(年, すすき開花, 夏) |>
  na.omit()

# df_sub <- df2 |>  ;df2を加工してdf_subを作る
# dplyr::select(年, すすき開花, 夏) ;
#   年、すすき開花、夏　の3列だけを取り出す（不要な列を消している）
# na.omit() ;この3列の内、どれか1つでもNAがある行を丸ごと削除
# 要は、年・すすき開花・夏の3つがすべて揃っている年だけを抽出する

fit <- lm(すすき開花 ~ 年, data=df_sub)
df_sub$res_doy <- resid(fit)

fit2 <- lm(夏 ~ 年, data=df_sub)
df_sub$res_spr <- resid(fit2)

cor(df_sub$res_doy, df_sub$res_spr, method="spearman")

# Sen's slope除去後の相関（ロバスト）

#library(mblm)

fit1 <- mblm(すすき開花 ~ 年, data=df_sub)
fit2 <- mblm(夏 ~ 年, data=df_sub)

res1 <- df_sub$すすき開花 - predict(fit1)
res2 <- df_sub$夏 - predict(fit2)

cor(res1, res2, method="spearman", use="complete.obs")

# 多変量回帰
model <- lm(すすき開花 ~ 夏 + 年, data=df2)
tidy(model) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  kable()


# すすき開花と秋の気温の相関
# 基本相関
cor(df2$すすき開花, df2$秋, method="spearman", use="complete.obs")

# 線形トレンド除去後の相関
df_sub <- df2 |>
  dplyr::select(年, すすき開花, 秋) |>
  na.omit()

fit <- lm(すすき開花 ~ 年, data=df_sub)
df_sub$res_doy <- resid(fit)

fit2 <- lm(秋 ~ 年, data=df_sub)
df_sub$res_spr <- resid(fit2)

cor(df_sub$res_doy, df_sub$res_spr, method="spearman")

# Sen's slope除去後の相関（ロバスト）

#library(mblm)

fit1 <- mblm(すすき開花 ~ 年, data=df_sub)
fit2 <- mblm(秋 ~ 年, data=df_sub)

res1 <- df_sub$すすき開花 - predict(fit1)
res2 <- df_sub$秋 - predict(fit2)

cor(res1, res2, method="spearman", use="complete.obs")

# 多変量回帰
model <- lm(すすき開花 ~ 秋 + 年, data=df2)
tidy(model) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  kable()

