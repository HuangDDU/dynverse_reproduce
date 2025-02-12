library(testthat)
library(dynwrap)
library(tidyverse)

# 复用/test-calculate_trajectory_dimred_new的数据，就是test-wrap_add_waypoints_new添加好轨迹的数据数据
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-calculate_trajectory_dimred_new.R")

context("Testing add_grouping")

test_that("Testing add_grouping", {
  trajectory_edges_group <- group_onto_trajectory_edges(trajectory) # 聚集到边上
  nearest_milestones_group <- group_onto_nearest_milestones(trajectory) # 聚集到最近的里程碑上
  expected_trajectory_edges_group <- c("W->X", "W->X", "X->Z", "X->Z", "X->Z", "Z->A")
  expected_nearest_milestones_group <- c("W", "X", "X", "Z", "Z", "Z")
  expect_equivalent(trajectory_edges_group, expected_trajectory_edges_group)
  expect_equivalent(nearest_milestones_group, expected_nearest_milestones_group)
})