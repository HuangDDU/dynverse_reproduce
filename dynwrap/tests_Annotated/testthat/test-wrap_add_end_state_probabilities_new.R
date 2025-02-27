library(testthat)
library(dynwrap)
library(dynutils)
library(tidyverse)

context("Testing add_end_state_probabilities")

# cell data
id <- "a"
cell_ids <- c("a", "aa", "b", "bb", "c", "cc")
end_state_ids <- c("A", "B", "C")
end_state_probabilities <- tribble(
  ~cell_id, ~A, ~B, ~C,
  "a", .5, 0, 0,
  "aa", 1, 0, 0,
  "b", 0, .5, 0,
  "bb", 0, 1, 0,
  "c", 0, 0, .5,
  "cc", 0, 0, 1,
)
end_state_probabilities <- as.data.frame(end_state_probabilities)
pseudotime <- c(.5, 1, .5, 1, .5, 1)
names(pseudotime) <- cell_ids

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

# 指定3个状态的星型轨迹
test_that("Testing add_end_state_probabilities 3 state", {
  wr <- wr_orig %>%
    add_end_state_probabilities(
      end_state_probabilities = end_state_probabilities,
      pseudotime = pseudotime
    )

  # 预期输出
  # 这边偷懒了，基本从add_end_state_probabilities复制代码
  start_milestone_id <- "milestone_begin"
  milestone_ids <- c(start_milestone_id, end_state_ids)
  expected_milestone_network <- tibble(
    from = start_milestone_id,
    to = end_state_ids,
    length = 1,
    directed = TRUE
  )
  expected_divergence_regions <- tibble(
    milestone_id = milestone_ids,
    divergence_id = "D",
    is_start = milestone_ids == start_milestone_id
  )
  scaled_pseudotime <- scale_minmax(pseudotime)
  expected_progressions <- end_state_probabilities %>%
    gather("to", "percentage", -cell_id) %>% # 忽略cell_id列，宽数据转化为长数据
    mutate(from = start_milestone_id) %>%
    group_by(cell_id) %>%
    mutate(percentage = percentage / sum(percentage) * scaled_pseudotime[cell_id]) %>%  # 缩放使其之和为1 # scale percentage so that sum = 1
    ungroup()

  expect_equal(wr$milestone_network, expected_milestone_network)
  expect_equal(wr$divergence_regions, expected_divergence_regions %>% select(colnames(wr$divergence_regions)))
  expect_equal(wr$progressions, expected_progressions %>% select(colnames(wr$progressions)))
})

# # TODO: progression转化为mielstone_percentage中milestone_begin重复的问题
# # 一个细胞在多个终端状态上都有概率
# test_that("Testing add_end_state_probabilities 3 state", {
#   end_state_probabilities <- tribble(
#     ~cell_id, ~A, ~B, ~C,
#     "a", .5, .3, .2,
#     "aa", 1, 0, 0,
#     "b", .3, .5, .2,
#     "bb", 0, 1, 0,
#     "c", .3, .2, .5,
#     "cc", 0, 0, 1,
#   )
#   pseudotime <- c(.5, 1, .5, 1, 0, 1)
#   names(pseudotime) <- cell_ids
#   wr <- wr_orig %>%
#     add_end_state_probabilities(
#       end_state_probabilities = end_state_probabilities,
#       pseudotime = pseudotime
#     )
# })

# 不指定状态，就是线性轨迹，直接使用伪时间
test_that("Testing add_end_state_probabilities without state", {
  # test with only one end states

  wr <- wr_orig %>%
    add_end_state_probabilities(
      end_state_probabilities = end_state_probabilities[, "cell_id", drop = F], # 不指定状态
      pseudotime = pseudotime
    )

  # 预期输出，相当于直接调用线性轨迹
  expected_wr <- wr_orig %>%
    add_linear_trajectory(
      pseudotime = pseudotime,
      directed = TRUE,
      do_scale_minmax = TRUE
    )

  expect_equal(wr$milestone_network, expected_wr$milestone_network)
  expect_equal(wr$divergence_regions, expected_wr$divergence_regions)
  expect_equivalent(wr$progressions, expected_wr$progressions)
})

