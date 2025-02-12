# 简化轨迹
library(testthat)
library(dynwrap)
library(dplyr)
library(tidyverse)

context("Testing simplify_trajectory_new")

test_that("directed_linear", {
  id <- "directed_linear"
  cell_ids <- c("a", "b", "c", "d", "e")
  milestone_ids <- c("A", "B", "C", "D")
  milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", 1, TRUE,
    "B", "C", 1, TRUE,
    "C", "D", 1, TRUE,
  )
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "A", "B", 0.3,
    "b", "A", "B", 0.6,
    "c", "B", "C", 0.2,
    "d", "B", "C", 0.8,
    "e", "C", "D", 0.4,
  )
  trajectory <-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        progressions = progressions
      )
  simp <- simplify_trajectory(trajectory)

  # 预期输出
  expected_milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "D", 3, TRUE
  )
  expected_progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "A", "D", 0.1,
    "b", "A", "D", 0.2,
    "c", "A", "D", 0.4,
    "d", "A", "D", 0.6,
    "e", "A", "D", 0.8,
  )

  testthat::expect_equal(simp$milestone_network, as.data.frame(expected_milestone_network))
  testthat::expect_equal(simp$progression, as.data.frame(expected_progressions))
})
