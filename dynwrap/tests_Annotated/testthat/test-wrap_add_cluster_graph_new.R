library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing add_cluster_graph")


test_that("Testing add_cluster_graph", {
  # 手动添加该路径
  source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_waypoints_new.R")
  test_wrap_data <- get_test_wrap_data()
  dataset <- test_wrap_data$dataset
  milestone_network <- test_wrap_data$milestone_network
  grouping <- c("W", "X", "X", "Z", "Z", "Z")

  trajectory <- dataset %>%
    add_cluster_graph(
      milestone_network = milestone_network,
      grouping = grouping
    )


  expect_equal(trajectory$milestone_percentages %>%
                 filter(percentage == 1) %>%
                 pull("milestone_id"), grouping)

})

# TODO: 不连通的分量测试