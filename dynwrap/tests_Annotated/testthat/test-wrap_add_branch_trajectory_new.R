library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing add_trajectory")

# 测试样例1: 简单分支结构
id <- "a"
cell_ids <- c("a", "b", "c", "d", "e", "f")
branch_ids <- c("A", "B", "C", "D")

branch_network <- tribble(
  ~from, ~to,
  "A", "B",
  "A", "C",
  "B", "D"
)
branch_progressions <- tibble(
  cell_id = cell_ids,
  branch_id = c("A", "A", "B", "B", "C", "D"),
  percentage = c(0, 0.8, 0.2, 1, 0.2, 0.2)
) # 此处e细胞微调到分支C 0.2上
branches <- tibble(
  branch_id = branch_ids,
  length = c(1,1,1,2),
  directed = TRUE
)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_trajectory with milestone_percentages", {
  wr <-
    wr_orig %>%
    add_branch_trajectory(
      branch_ids = branch_ids,
      branch_network = branch_network,
      branch_progressions = branch_progressions,
      branches = branches
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$milestone_network$from, c("1", "2", "2", "3"))
  expect_equivalent(wr$milestone_network$to, c("2", "3", "4", "5"))
})



# 测试样例2: branch有环
id <- "a"
cell_ids <- c("a", "b", "c", "d", "e", "f")
branch_ids <- c("A", "B", "C")

branch_network <- tribble(
  ~from, ~to,
  "A", "B",
  "B", "C",
  "B", "A",
  "A", "C"
)
branch_progressions <- tibble(
  cell_id = cell_ids,
  branch_id = c("A", "A", "A", "B", "B", "C"),
  percentage = c(0, 0.5, 1, 0.1, 0.2, 0.3)
)
branches <- tibble(
  branch_id = branch_ids,
  length = as.numeric(1:3),
  directed = TRUE
)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_trajectory with milestone_percentages", {
  wr <-
    wr_orig %>%
      add_branch_trajectory(
        branch_ids = branch_ids,
        branch_network = branch_network,
        branch_progressions = branch_progressions,
        branches = branches
      )

  expect_equivalent(wr$milestone_network$from, c("1", "1", "1", "1-Aa", "1-Ab", "1-Ba", "1-Bb"))
})
