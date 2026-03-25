# -----------------------------------
# 2) ENSOの色・形
# -----------------------------------
scale_enso <- function(name = "ENSO"){
  list(
    scale_color_manual(
      name = name,
      values = c(
        ElNino  = "#d73027",
        LaNina  = "#1a9850",
        Neutral = "#4575b4"
      ),
      breaks = c("ElNino", "LaNina", "Neutral"),
      na.translate = FALSE
    ),
    scale_shape_manual(
      name = name,
      values = c(
        ElNino  = 16,
        LaNina  = 17,
        Neutral = 15
      ),
      breaks = c("ElNino", "LaNina", "Neutral"),
      na.translate = FALSE
    )
  )
}
