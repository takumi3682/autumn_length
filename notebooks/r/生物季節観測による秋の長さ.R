# 生物季節観測の相互相関を解析する（ヒートマップ化）
# 長期トレンド除去後についても解析する

library(readr)
library(dplyr)
library(tidyr)

out_dir <- get_out_dir()

# ---- ソースデータを読み込む ----
df <- read_csv("data/raw/生物季節観測_仙台_DOY_Temp.csv")

months <- paste0(1:12, "月")

df2 <- df %>%
  mutate(across(all_of(months), ~ {
    s <- as.character(.x)                 # ← まず文字列化（数値列でもOK）
    s[grepl("\\]$", s)] <- NA_character_  # ]で終わるものは欠損に
    parse_number(s)                       # )などが付いても数値だけ取る
  }))

# ---- 3〜5月をspring、6〜8月をsummer、･･･と設定 ----
df_season <- df2 %>%
  pivot_longer(cols = all_of(months), names_to = "month", values_to = "value") %>%
  mutate(
    month_num = parse_number(month),
    season = case_when(
      month_num %in% 3:5  ~ "spring",
      month_num %in% 6:8  ~ "summer",
      month_num %in% 9:11 ~ "autumn",
      TRUE                ~ "winter"
    ),
    season_year = if_else(season == "winter" & month_num %in% c(1, 2),
                          年 - 1, 年)
  ) %>%
  group_by(season_year, season) %>%
  summarise(
    season_mean = mean(value, na.rm = TRUE),
    n_month = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(season_year, season)

df_season_wide <- df_season %>%
  select(season_year, season, season_mean) %>%
  pivot_wider(names_from = season, values_from = season_mean) %>%
  select(season_year, spring, summer, autumn, winter) %>%
  rename(年 = season_year)

df_merged <- df2 %>%
  left_join(df_season_wide, by = "年")

vars <- c("うめ開花","さくら開花","さくら満開","あじさい開花",
          "すすき開花","いちょう黄葉","いちょう落葉",
          "かえで紅葉","かえで落葉",
          "spring","summer","autumn","winter")

X <- df_merged[, vars]
X <- dplyr::mutate(X, dplyr::across(dplyr::everything(), as.numeric))

cor_mat <- cor(X, use = "pairwise.complete.obs", method = "pearson")
cor_mat

round(cor_mat, 2)

# install.packages("corrplot")  # 未導入なら一度だけ
#library(dplyr)
library(corrplot)
library(ragg)

vars <- c("うめ開花","さくら開花","さくら満開","あじさい開花",
          "すすき開花","いちょう黄葉","いちょう落葉",
          "かえで紅葉","かえで落葉",
          "spring","summer","autumn","winter")

X <- df_merged[, vars] %>%
  mutate(across(everything(), as.numeric))

# ---- Spearman 相関 ----
cor_mat <- cor(X, use = "pairwise.complete.obs", method = "spearman")

plot_corr <- function(cor_mat, main = "Spearmanによる相関係数") {
  par(family = "Hiragino Sans")
  corrplot(cor_mat,
           method = "color",
           type = "upper",
           order = "original",
           addCoef.col = "black",
           tl.cex = 0.8,
           mar = c(0, 0, 5, 0),
           number.cex = 0.7)
  mtext(main, side = 3, line = 1, cex = 1.2)
}

# 1) Plots に表示
plot_corr(cor_mat)

# 2) PNG に保存
#out_dir <- Sys.getenv("OUT_DIR")
stopifnot(nzchar(out_dir))  # 空ならここで止める（安全）
png(file.path(out_dir, "Heatmap.png"), width = 800, height = 800, res = 120)
plot_corr(cor_mat)
dev.off()


# 長期トレンド除去
# 残差相関（線形トレンド除去）
library(dplyr)

X <- df_merged %>%
  select(年, all_of(vars)) %>%
  mutate(across(all_of(vars), as.numeric))

# 行数を保ったまま残差を返す関数
resid_lm_keep <- function(y, x){
  fit <- lm(y ~ x, na.action = na.exclude)
  r <- resid(fit)  # 元の行に対応したNA入りの残差になる
  as.numeric(r)
}

X_resid <- X
X_resid[vars] <- lapply(vars, function(v) resid_lm_keep(X[[v]], X$年))

cor_resid <- cor(X_resid[vars], use = "pairwise.complete.obs", method = "spearman")
round(cor_resid, 2)

# install.packages("corrplot")  # 未導入なら一度だけ
library(corrplot)

par(family = "Hiragino Sans")  # 日本語豆腐対策（Mac想定）

plot_corr(cor_resid, main = "Spearmanによる相関係数（長期トレンド除去後）")

#out_dir <- Sys.getenv("OUT_DIR")
stopifnot(nzchar(out_dir))  # 空ならここで止める（安全）
png(file.path(out_dir, "Heatmap_resid.png"), width = 800, height = 800, res = 120)
plot_corr(cor_resid, main = "Spearmanによる相関係数（長期トレンド除去後）")
dev.off()

