library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing add_dimred_projection")

# 手动添加该路径
source("/home/huang/RCode/scrna_tools/dynverse_reproduce/dynwrap/tests_Annotated/testthat/test-wrap_add_waypoints_new.R")


# 测试用例1： 不带聚类标签
test_wrap_data <- get_test_wrap_data() # 复用之前带数据
dataset <- test_wrap_data$dataset
milestone_network <- test_wrap_data$milestone_network
divergence_regions <- test_wrap_data$divergence_regions

# 手动指定2维降维
dimred <- tribble(
  ~cell_id, ~comp_1, ~comp_2,
  "a", 0, 1.5,
  "b", 0.8, 0.5,
  "c", 1.2, 0.5,
  "d", 2, 0.5,
  "e", 1.2, 1.5,
  "f", 2.4, 1.5,
)
dimred <- dimred %>%
  column_to_rownames(var = "cell_id") %>%  # 将cell_id列设置为行名
  select(comp_1, comp_2) %>% # 选择需要转换为矩阵的列
  as.matrix()

dimred_milestones <- tribble(
  ~milesonte_id, ~comp_1, ~comp_2,
  "W", 0, 1,
  "X", 1, 1,
  "Y", 1, 2,
  "Z", 2, 1,
  "A", 4, 1,
)
dimred_milestones <- dimred_milestones %>%
  column_to_rownames(var = "milesonte_id") %>%
  select(comp_1, comp_2) %>%
  as.matrix()

# 预期输出
# expected_milestone_percentages <- test_wrap_data$milestone_percentages %>%
#   filter(cell_id != "e") %>%
#   bind_rows(
#     tribble(
#       ~cell_id, ~milestone_id, ~percentage,
#       "e", "X", 0.5,
#       "e", "Y", 0.5,
#     )) # 对于细胞e微调
expected_progressions <- tribble(
  ~cell_id, ~from, ~to, ~percentage,
  "a", "W", "X", 0,
  "b", "W", "X", 0.8,
  "c", "X", "Z", 0.2,
  "d", "X", "Z", 1,
  "e", "X", "Y", 0.5,
  "f", "Z", "A", 0.2,
)

# expected_dimred_segment_points # 预期输出的降维


test_that("Testing add_dimred_projection", {
  trajectory <- dataset %>%
    add_dimred_projection(
      milestone_network = milestone_network,
      dimred = dimred,
      dimred_milestones = dimred_milestones,
    )

  # 轨迹添加部分
  # # percentage是progression的间接结果
  # sorted_milestone_percentages <- trajectory$milestone_percentages %>%
  #   filter(percentage != 0) %>%
  #   arrange(cell_id, milestone_id) # 过滤掉percetange为0的条目后排序
  # sorted_expected_milestone_percentages <- expected_milestone_percentages %>% arrange(cell_id, milestone_id)
  # expect_equivalent(sorted_milestone_percentages, sorted_expected_milestone_percentages)
  # progression是直接结果
  expect_equivalent(trajectory$progressions, expected_progressions) # expect_equivalent可以表格元数据，例如dimname

  # 投影添加部分
  expect_equivalent(trajectory$dimred, dimred)
  # expect_equivalent(trajectory$dimred_milestones, dimred_milestones)
})


# 测试用例2：带聚类标签
grouping <- c("X", "X", "X", "Z", "Z", "Z") # 聚类标签名称应该与里程碑名称一致
expected_progressions <- tribble(
  ~cell_id, ~from, ~to, ~percentage,
  "a", "W", "X", 0,
  "b", "W", "X", 0.8,
  "c", "X", "Z", 0.2,
  "d", "X", "Z", 1,
  "e", "X", "Z", 0.2, # 此时e只能投影到Z相关边上，与之前有细微改变
  "f", "Z", "A", 0.2,
)
test_that("Testing add_dimred_projection with group", {
  trajectory <- dataset %>%
    add_dimred_projection(
      milestone_network = milestone_network,
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      grouping = grouping
    )
  expect_equivalent(trajectory$progressions, expected_progressions) # expect_equivalent可以表格元数据，例如dimname
})