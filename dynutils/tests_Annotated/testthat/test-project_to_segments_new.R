library(testthat)
library(dynutils)
library(tidyverse)

context("Test project_to_segments")

# 与add_dimred_projection测试用例保持一致
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_dimred_projection_new.R")

segment_start <- dimred_milestones[milestone_network$from,]
segment_end <- dimred_milestones[milestone_network$to,]
x <- dimred

# 构造预期输出
expected_x_proj <- tribble(
  ~cell_id, ~comp_1, ~comp_2,
  "a", 0.0, 1.0,
  "b", 0.8, 1.0,
  "c", 1.2, 1.0,
  "d", 2.0, 1.0,
  "e", 1.0, 1.5,
  "f", 2.4, 1.0,
) %>%
  column_to_rownames("cell_id") %>%
  as.matrix()
# 投影后的坐标
expected_distance <- c(0.25, 0.25, 0.25, 0.25, 0.04, 0.25) # 源点到投影点的距离平方
expected_segment <- c(1, 1, 3, 3, 2, 4) # 投影到在第几条边上
expected_progression <- expected_progressions$percentage # 投影点在所在边额比例

# 添加细胞名称
cell_ids <- rownames(dimred)
names(expected_distance) <- cell_ids
names(expected_segment) <- cell_ids
names(expected_progression) <- cell_ids


test_that("project_to_segments works", {
  out <- project_to_segments(x, segment_start, segment_end)
  # 调用了RcppExports/project_to_segments, 本质内部调用了C++代码src/project_to_segments.cpp

  expect_equal(out$x_proj, expected_x_proj)
  expect_equal(out$distance, expected_distance)
  expect_equal(out$segment, expected_segment)
  expect_equal(out$progression, expected_progression)

})

