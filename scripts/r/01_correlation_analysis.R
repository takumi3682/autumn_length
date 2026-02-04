# ==========================================
# 生物季節観測 相関解析（解析専用スクリプト）
# ==========================================

library(readr)
library(dplyr)
library(tidyr)
library(corrplot)
library(ragg)

out_dir <- get_out_dir()
stopifnot(nzchar(out_dir))

print(data)
# ---- データ読み込み ----
df <- read_csv("data/raw/生物季節観測_仙台_DOY_Temp.csv")

months <- paste0(1:12, "月")

# ---- 月別データの数値化 ----
df2 <- df %>%
  mutate(across(all_of(months), ~ {
    s <- as.character(.x)
    s[grepl("\\]$", s)] <- NA_character_
    parse_number(s)
  }))

# ---- 季節平均 ----
df_season <- df2 %>%
  pivot_longer(cols = all_of(months),
               names_to = "month",
               values_to = "value") %>%
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
  summarise(season_mean = mean(value, na.rm = TRUE), .groups = "drop")

df_season_wide <- df_season %>%
  pivot_wider(names_from = season, values_from = season_mean) %>%
  rename(年 = season_year)

df_merged <- df2 %>%
  left_join(df_season_wide, by = "年")

# ---- 中間結果を保存（重要）----
saveRDS(df_merged, file.path("data", "processed", "df_merged.rds"))

# ---- 相関解析（Spearman）----
vars <- c("うめ開花","さくら開花","さくら満開","あじさい開花",
          "すすき開花","いちょう黄葉","いちょう落葉",
          "かえで紅葉","かえで落葉",
          "spring","summer","autumn","winter")

X <- df_merged[, vars] %>%
  mutate(across(everything(), as.numeric))

cor_raw <- cor(X, use = "pairwise.complete.obs", method = "spearman")
saveRDS(cor_raw, file.path(out_dir, "cor_raw_spearman.rds"))

# ---- プロット関数 ----
plot_corr <- function(cor_mat, main) {
  par(family = "Hiragino Sans")
  corrplot(cor_mat,
           method = "color",
           type = "upper",
           addCoef.col = "black",
           tl.cex = 0.8,
           mar = c(0,0,5,0),
           number.cex = 0.7)
  mtext(main, side = 3, line = 1)
}

png(file.path(out_dir, "Heatmap.png"), 800, 800, res = 120)
plot_corr(cor_raw, "Spearman相関")
dev.off()

# ---- 長期トレンド除去 ----
resid_lm_keep <- function(y, x){
  resid(lm(y ~ x, na.action = na.exclude))
}

X_resid <- df_merged %>%
  select(年, all_of(vars)) %>%
  mutate(across(all_of(vars), as.numeric))

X_resid[vars] <- lapply(vars, \(v) resid_lm_keep(X_resid[[v]], X_resid$年))

cor_resid <- cor(X_resid[vars],
                 use = "pairwise.complete.obs",
                 method = "spearman")

saveRDS(cor_resid, file.path(out_dir, "cor_resid_spearman.rds"))

png(file.path(out_dir, "Heatmap_resid.png"), 800, 800, res = 120)
plot_corr(cor_resid, "Spearman相関（長期トレンド除去後）")
dev.off()
