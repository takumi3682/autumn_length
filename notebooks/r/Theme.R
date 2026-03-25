# -----------------------------------
# 1) プレゼン用テーマ
# -----------------------------------
# フルサイズスライドなら 12×6.75インチ、base_size = 28 
# 2/3サイズなら　9×5.06インチ、base_size = 22

paper_width = 9
paper_height = 5.06
font_size = 22

# annotate等のサイズをptからmmに変換する係数
# (base_size=28 と同等にするなら 28 / 2.845 ≒ 10)
TEXT_SIZE_MM <- font_size / 2.845 

theme_presentation <- function(base_size = font_size, legend_position = "right"){
  theme_bw(base_size = base_size) +
    theme(
      plot.title      = element_text(size = rel(1.5), face = "bold", margin = margin(b = 15)),
      plot.subtitle   = element_text(size = rel(1.2)),
      plot.margin     = margin(10, 10, 10, 10), # 全体の余白を少し詰める
      
      axis.title      = element_text(size = rel(1.3)),
      axis.text       = element_text(color = "black", size = rel(1.1)),
      
      # 凡例：サイズを大きくし、グラフに近づける
      legend.key.size = unit(2.5, "lines"), # マークをさらに大きく
      legend.title    = element_text(size = rel(1.2), face = "bold"),
      legend.text     = element_text(size = rel(1.1)),
      legend.position = legend_position,
      legend.background = element_blank(),
#      legend.box.margin = margin(l = -20), # ★ここ！凡例を左（グラフ側）に寄せる
      
      # グリッド：極限まで薄く、細く
      panel.grid.major = element_line(linewidth = 0.1, color = "grey95"), # ★ここ！
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(linewidth = 0.5, color = "grey50"), # 外枠も少し薄く
      plot.background  = element_blank()
    )
}
