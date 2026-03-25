# -----------------------------------
# 3) 四象限 + Fisher 検定 図関数
# -----------------------------------
plot_fisher_quad_enso <- function(
    df,
    x,
    y,
    enso_col = ENSO_next_winter,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    xlim = NULL,
    ylim = NULL,
    base_size = 20
) {
  
  message("DEBUG: plot_fisher_quad_enso called")
  print(subtitle)
  
  print(
    df %>% summarise(
      n = n(),
      mean_x = mean({{x}}, na.rm = TRUE),
      mean_y = mean({{y}}, na.rm = TRUE)
    )
  )  
  
  # -----------------------------
  # 作図データ
  # -----------------------------
  df_plot <- df %>%
    drop_na({{ enso_col }}, {{ x }}, {{ y }}) %>%
    mutate(.enso = {{ enso_col }})
  
  # -----------------------------
  # 全期間共通中央値
  # -----------------------------
  x_cut <- median(pull(df_plot, {{ x }}), na.rm = TRUE)
  y_cut <- median(pull(df_plot, {{ y }}), na.rm = TRUE)
  
  # -----------------------------
  # 四象限分類
  # -----------------------------
  df_quad <- df_plot %>%
    mutate(
      x_group = if_else({{ x }} >= x_cut, "High", "Low"),
      y_group = if_else({{ y }} >= y_cut, "High", "Low"),
      quadrant = case_when(
        x_group == "High" & y_group == "High" ~ "Q1",
        x_group == "Low"  & y_group == "High" ~ "Q2",
        x_group == "Low"  & y_group == "Low"  ~ "Q3",
        x_group == "High" & y_group == "Low"  ~ "Q4"
      )
    )
  
  # -----------------------------
  # 軸範囲
  # -----------------------------
  x_rng <- range(pull(df_quad, {{ x }}), na.rm = TRUE)
  y_rng <- range(pull(df_quad, {{ y }}), na.rm = TRUE)
  
  x_use <- if (is.null(xlim)) x_rng else xlim
  y_use <- if (is.null(ylim)) y_rng else ylim
  
  # 少し余白
  x_span <- diff(x_use)
  y_span <- diff(y_use)
  
  # -----------------------------
  # ENSOごとのFisher検定
  # -----------------------------
  fisher_tbl <- df_quad %>%
    group_by(.enso) %>%
    group_modify(~{
      tab <- table(.x$x_group, .x$y_group)
      ft  <- fisher.test(tab)
      
      tibble(
        n = nrow(.x),
        p_value = ft$p.value,
        odds_ratio = ifelse(is.null(ft$estimate), NA_real_, unname(ft$estimate)),
        q1 = sum(.x$quadrant == "Q1"),
        q2 = sum(.x$quadrant == "Q2"),
        q3 = sum(.x$quadrant == "Q3"),
        q4 = sum(.x$quadrant == "Q4")
      )
    }) %>%
    ungroup() %>%
    mutate(
      positive_pair = q1 + q3,
      negative_pair = q2 + q4,
      label = paste0(
        "Fisher p = ", sprintf("%.3f", p_value), "\n",
        "Q1=", q1, "  Q2=", q2, "\n",
        "Q3=", q3, "  Q4=", q4, "\n",
        "Pos=", positive_pair, "  Neg=", negative_pair, "\n",
        "n=", n
      ),
      x_lab = x_use[2] - 0.03 * x_span,
      y_lab = y_use[2] - 0.03 * y_span
    )
  
  # -----------------------------
  # 象限カウントを中央寄りに置く
  # -----------------------------
  quad_labels <- fisher_tbl %>%
    transmute(
      .enso,
      q1, q2, q3, q4
    ) %>%
    pivot_longer(cols = starts_with("q"), names_to = "quadrant", values_to = "count") %>%
    mutate(
      x = case_when(
        quadrant %in% c("q1", "q4") ~ x_cut + 0.30 * (x_use[2] - x_cut),
        quadrant %in% c("q2", "q3") ~ x_use[1] + 0.30 * (x_cut - x_use[1])
      ),
      y = case_when(
        quadrant %in% c("q1", "q2") ~ y_cut + 0.30 * (y_use[2] - y_cut),
        quadrant %in% c("q3", "q4") ~ y_use[1] + 0.30 * (y_cut - y_use[1])
      ),
      label = as.character(count)
    )
  
  # -----------------------------
  # 背景矩形（Q1,Q3を薄く着色）
  # -----------------------------
  bg_rect <- tibble(
    xmin = c(x_cut, x_use[1]),
    xmax = c(x_use[2], x_cut),
    ymin = c(y_cut, y_use[1]),
    ymax = c(y_use[2], y_cut),
    fill_group = c("positive", "positive")
  )
  
  # -----------------------------
  # 作図
  # -----------------------------
  ggplot(
    df_quad,
    aes(
      x = {{ x }},
      y = {{ y }},
      color = .enso,
      shape = .enso
    )
  ) +
    geom_rect(
      data = bg_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey85",
      alpha = 0.25
    ) +
    geom_vline(xintercept = x_cut, linetype = "dashed", linewidth = 1.1, color = "grey30") +
    geom_hline(yintercept = y_cut, linetype = "dashed", linewidth = 1.1, color = "grey30") +
    geom_point(size = 3.2, alpha = 0.9) +
    geom_text(
      data = quad_labels,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 7,
      fontface = "bold",
      color = "black"
    ) +
    geom_text(
      data = fisher_tbl,
      aes(x = x_lab, y = y_lab, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 1,
      size = 5.5,
      color = "black"
    ) +
    scale_enso() +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    coord_cartesian(
      xlim = x_use,
      ylim = y_use
    ) +
    facet_wrap(~ .enso, ncol = 3) +
    theme_presentation(base_size = base_size)
}