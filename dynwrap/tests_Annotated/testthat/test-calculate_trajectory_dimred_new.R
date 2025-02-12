library(testthat)
library(dynplot)
library(ggplot2)

# 手动添加该路径
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynplot/R/expect_ggplot.R")
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_waypoints_new.R")

context("calculate_trajectory_dimred")

# 复用test-wrap_add_waypoints_new数据
test_wrap_data <- get_test_wrap_data()
dataset <- test_wrap_data$dataset
milestone_ids <- test_wrap_data$milestone_ids
milestone_network <- test_wrap_data$milestone_network
divergence_regions <- test_wrap_data$divergence_regions
milestone_percentages <- test_wrap_data$milestone_percentages

trajectory <- dataset%>%
  add_trajectory(milestone_ids=milestone_ids,
                 milestone_network=milestone_network,
                 divergence_regions=divergence_regions,
                 milestone_percentages = milestone_percentages)

# 额外数据
cell_ids <- trajectory$cell_ids
milestone_ids <- trajectory$milestone_ids

test_that("calculate_trajectory_dimred output format is correct", {
  dimred <- calculate_trajectory_dimred(trajectory)

  expect_equal(sort(names(dimred)), c("cell_positions", "divergence_edge_positions",  "divergence_polygon_positions", "edge_positions", "milestone_positions"))

  edge_positions <- dimred$edge_positions
  expect_equal(colnames(edge_positions), c("from", "to", "length", "directed", "comp_1_from", "comp_2_from", "comp_1_to", "comp_2_to"))
  join_check <- edge_positions %>% inner_join(milestone_network, by = c("from", "to"))
  expect_equal(join_check$length.x, join_check$length.y)

  milestone_positions <- dimred$milestone_positions
  expect_equal(colnames(milestone_positions), c("milestone_id", "comp_1", "comp_2"))
  expect_true(all(milestone_ids %in% milestone_positions$milestone_id))

  cell_positions <- dimred$cell_positions
  expect_equal(colnames(cell_positions), c("cell_id", "comp_1", "comp_2"))
  expect_true(all(cell_ids %in% cell_positions$cell_id))

  divergence_edge_positions <- dimred$divergence_edge_positions
  expect_equal(colnames(divergence_edge_positions), c("from", "to", "comp_1_from", "comp_2_from", "comp_1_to", "comp_2_to"))

  divergence_polygon_positions <- dimred$divergence_polygon_positions
  expect_equal(colnames(divergence_polygon_positions), c("triangle_id", "triangle_part", "milestone_id", "comp_1", "comp_2"))
})
