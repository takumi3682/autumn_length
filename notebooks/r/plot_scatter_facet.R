# =========================================================
# 汎用：ENSO別ファセット散布図
# =========================================================
library(ggplot2)

plot_scatter_facet_generic <- function(
    df,
    x_col,
    y_col,
    enso_col = "ENSO_next_winter",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    xlim = NULL,
    ylim = NULL,
    method = "pearson",
    show_legend = FALSE
){
  
  df_plot <- df %>%
    drop_na(all_of(c(x_col, y_col, enso_col))) %>%
    mutate(.enso = .data[[enso_col]])
  
  ann <- df_plot %>%
    group_by(.enso) %>%
    summarise(
      n = n(),
      r = cor(.data[[x_col]], .data[[y_col]], method = method),
      p = cor.test(.data[[x_col]], .data[[y_col]], method = method, exact = FALSE)$p.value,
      .groups = "drop"
    ) %>%
    mutate(
      label = if(method == "spearman"){
        paste0("rho = ", sprintf("%.2f", r), "\n",
               "p = ", sprintf("%.3f", p), "\n",
               "n = ", n)
      } else {
        paste0("r = ", sprintf("%.2f", r), "\n",
               "p = ", sprintf("%.3f", p), "\n",
               "n = ", n)
      },
      x_lab = xlim[2] - 0.15,
      y_lab = ylim[2] - 0.15
    )
  
  ggplot(df_plot, aes(x = .data[[x_col]], y = .data[[y_col]], color = .enso, shape = .enso)) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.12, linewidth = 1.3) +
    geom_point(size = 3) +
    geom_text(
      data = ann,
      aes(x = x_lab, y = y_lab, label = label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1,
      size = 6
    ) +
    scale_enso() +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab,
      color = "ENSO",
      shape = "ENSO"
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    facet_wrap(~ .enso, ncol = 3) +
    theme_presentation(
      legend_position = if(show_legend) "right" else "none"
    )
}

