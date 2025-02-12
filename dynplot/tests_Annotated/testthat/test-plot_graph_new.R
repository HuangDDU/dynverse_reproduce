library(testthat)
library(dynplot)
library(ggplot2)

# 手动添加该路径
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynplot/R/expect_ggplot.R")
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_waypoints_new.R")

context("Test plot_graph")

test_wrap_data <- get_test_wrap_data()

dataset <- test_wrap_data$dataset
milestone_ids <- test_wrap_data$milestone_ids
milestone_network <- test_wrap_data$milestone_network
divergence_regions <- test_wrap_data$divergence_regions
milestone_percentages <- test_wrap_data$milestone_percentages

dataset <- dataset%>%
  # add_expression(counts=space, expression=space) %>% # 二维表达与降维结果一致
  add_trajectory(milestone_ids=milestone_ids,
                 milestone_network=milestone_network,
                 divergence_regions=divergence_regions,
                 milestone_percentages = milestone_percentages)


test_that(paste0("plot_graph on ", dataset$id, " with milestone"), {
  g <- plot_graph(dataset, "milestone")
  expect_ggplot(g)
})