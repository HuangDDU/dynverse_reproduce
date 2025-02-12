library(testthat)
library(dynplot)
library(ggplot2)

# 手动添加该路径
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynplot/R/expect_ggplot.R")
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_waypoints_new.R")

context("Test plot_dimred")

test_wrap_data <- get_test_wrap_data()
dataset <- test_wrap_data$dataset
milestone_ids <- test_wrap_data$milestone_ids
milestone_network <- test_wrap_data$milestone_network
divergence_regions <- test_wrap_data$divergence_regions
milestone_percentages <- test_wrap_data$milestone_percentages

# 此处需要的额外数据
grouping <- c(1,1,2,2,2,3)
# 手动指定降维结果
space <- tibble::tribble(
  ~comp_1, ~comp_2,
  0,     10,
  8,     10,
  12,    12,
  20,    20,
  15,    16,
  22,    20
)
rownames(space) <- dataset$cell_ids # 设置行名后自动tibble转化为dataframe
dataset <- dataset %>% dynutils::extend_with("dynwrap::with_dimred", dim=space)
# feature_oi <- first(colnames(dataset$expression)) # first函数丢失
# feature_oi <- colnames(dataset$expression)[1]
# grouping <- dataset$prior_information$grouping_assignment


dataset <- dataset%>%
  # add_expression(counts=space, expression=space) %>% # 二维表达与降维结果一致
  add_trajectory(milestone_ids=milestone_ids,
                 milestone_network=milestone_network,
                 divergence_regions=divergence_regions,
                 milestone_percentages = milestone_percentages)

# 这是quickstart中的使用方法, 主要关注这里
test_that(paste0("plot_dimred on ", dataset$id, " with grouping"), {
  g <- plot_dimred(dataset, grouping = grouping, dimred = space)
  expect_ggplot(g)
})
g
