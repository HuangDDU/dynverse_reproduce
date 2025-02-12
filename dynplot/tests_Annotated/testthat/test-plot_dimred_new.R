library(testthat)
library(dynplot)
library(ggplot2)

# 手动添加该路径
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynplot/R/expect_ggplot.R")

context("Test plot_dimred")

dataset <- dynplot::example_bifurcating # 测试用例, 二分支, 这个最直观


space <- dyndimred::dimred_pca(dataset$expression)
# # feature_oi <- first(colnames(dataset$expression)) # first函数丢失
# feature_oi <- colnames(dataset$expression)[1]
grouping <- dataset$prior_information$grouping_assignment

# 这是quickstart中的使用方法, 主要关注这里
test_that(paste0("plot_dimred on ", dataset$id, " with grouping"), {
  g <- plot_dimred(dataset, grouping = grouping, dimred = space)
  expect_ggplot(g)
})
g
