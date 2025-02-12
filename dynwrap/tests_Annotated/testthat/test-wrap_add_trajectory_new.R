# 调整里程碑milestone和细胞cell的命名

library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing add_trajectory")

id <- "add_trajectory_new"
cell_ids <- c("a", "b", "c", "d", "e", "f")

milestone_ids <-  c("A", "B", "C", "D", "E", "F", "G")

milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "A", "B", 1, TRUE,
  "B", "C", 2, TRUE,
  "B", "D", 3, TRUE,
  "C", "E", 4, TRUE,
  "D", "F", 5, TRUE,
  "E", "G", 6, TRUE,
  "F", "G", 7, TRUE
)
milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "a", "A", .8,
  "a", "B", .2,
  "b", "B", .3,
  "b", "C", .2,
  "b", "D", .5,
  "c", "C", 0,
  "c", "E", 1,
  "d", "E", .5,
  "d", "G", .5,
  "e", "E", .9,
  "e", "G", .1,
  "f", "F", .6,
  "f", "G", .4
)
divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "BCD", "B", TRUE,
  "BCD", "C", FALSE,
  "BCD", "D", FALSE
)

dataset <- wrap_data(
  id = id,
  cell_ids = cell_ids
)
trajectory <- add_trajectory(
  dataset,
  milestone_network = milestone_network,
  divergence_regions = divergence_regions,
  milestone_percentages = milestone_percentages
) # 这样分两步走方便调试
# TODO: 这里需要额外加对于trajectory的测试
# 当前测试用例下,从milestone_percentages -> progressions的生成


test_that("Testing add_trajectory with milestone_percentages", {
  # 这里同时测试了gather_cells_at_milestones函数
  gathered_trajectory <- gather_cells_at_milestones(trajectory)

  testthat::expect_equal(unique(gathered_trajectory$milestone_percentages$percentage), 1) # 所有的概率都是1
  testthat::expect_equal(gathered_trajectory$milestone_percentages$milestone_id, c("A", "D", "E", "E", "E", "F"))
})

# 方便复制测试用例到PPT上
a <- data.frame(cell_ids)
b <- data.frame(milestone_ids)
