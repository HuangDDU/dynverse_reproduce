library(testthat)
library(dynplot)
library(tibble)
library(dplyr)


# 这个函数没有开放，这里手动复制过来, 就是对于RGB颜色加权计算
mix_colors <- function(milid, milpct, milestone_colors) {
  color_rgb <- apply(milestone_colors[milid, , drop = FALSE], 2, function(x) sum(x * milpct))
  color_rgb[color_rgb < 0] <- 0
  color_rgb[color_rgb > 256] <- 256
  do.call(grDevices::rgb, as.list(c(color_rgb, maxColorValue = 256)))
}


# 数据参考：/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_waypoints_new.R
milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "a", "W", 1,
  "b", "W", .2,
  "b", "X", .8,
  "c", "X", .8,
  "c", "Z", .2,
  "d", "Z", 1,
  "e", "X", .3,
  "e", "Y", .2,
  "e", "Z", .5,
  "f", "Z", .8,
  "f", "A", .2
)

# R语言这里的RGB:256对应十六进制:FF
milestone_colors <- tribble(
  ~red, ~green, ~blue,
  256, 0, 0,
  256, 128, 0,
  0, 0, 256,
  256, 256, 0,
  0, 256, 0,
)
milestone_colors <- as.matrix(milestone_colors)
rownames(milestone_colors) <- c("W", "X", "Y", "Z", "A")

milestone_percentages %>%
  group_by(.data$cell_id) %>%
  summarise(color = mix_colors(.data$milestone_id, .data$percentage, milestone_colors))
