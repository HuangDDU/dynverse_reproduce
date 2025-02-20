# 简化轨迹
library(testthat)
library(dynwrap)
library(dplyr)
library(tidyverse)

context("Testing simplify_trajectory_new")
# ===================================================linear===========================================================

# input data
id <- "directed linear"
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

# expected result
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

test_that("directed linear", {
  # execute function
  simp <- simplify_trajectory(trajectory)

  testthat::expect_equal(simp$milestone_network, as.data.frame(expected_milestone_network))
  testthat::expect_equal(simp$progression, as.data.frame(expected_progressions))
})


id <- "undirected linear"
test_that("undirected linear", {
  # input data
  milestone_network["directed"] <- FALSE # 设置为无向图
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

  # expected result
  expected_milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "D", 3, FALSE
  )
  # execute function
  simp <- simplify_trajectory(trajectory)

  testthat::expect_equal(simp$milestone_network, as.data.frame(expected_milestone_network))
  testthat::expect_equal(simp$progression, as.data.frame(expected_progressions))
})


# ===================================================bifurcation===========================================================
# input data
id <- "directed bifurcation"
cell_ids <- c("a", "b", "c", "d", "e", "f")
milestone_ids <- c("A", "B", "C", "D", "E", "F", "G")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "A", "B", 4, TRUE,
  "A", "C", 4, TRUE,
  "B", "D", 1, TRUE,
  "C", "E", 1, TRUE,
  "E", "F", 1, TRUE,
  "E", "G", 1, TRUE,
)
progressions <- tribble(
  ~cell_id, ~from, ~to, ~percentage,
  "a", "A", "B", 0.5,
  "b", "A", "C", 0.5,
  "c", "B", "D", 0.5,
  "d", "C", "E", 0.5,
  "e", "E", "F", 0.5,
  "f", "E", "G", 0.5,
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

# expected result
expected_milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "A", "D", 5, TRUE,
  "A", "E", 5, TRUE,
  "E", "F", 1, TRUE,
  "E", "G", 1, TRUE
)
expected_progressions <- tribble(
  ~cell_id, ~from, ~to, ~percentage,
  "a", "A", "D", 0.4,
  "b", "A", "E", 0.4,
  "c", "A", "D", 0.9,
  "d", "A", "E", 0.9,
  "e", "E", "F", 0.5,
  "f", "E", "G", 0.5,
)

test_that("directed bifurcation", {
  # execute function
  simp <- simplify_trajectory(trajectory)

  testthat::expect_equal(simp$milestone_network %>% arrange(from, to), as.data.frame(expected_milestone_network) %>% arrange(from, to))
  testthat::expect_equal(simp$progression %>% arrange(cell_id), as.data.frame(expected_progressions) %>% arrange(cell_id))
})

id <- "undirected bifurcation"
test_that("undirected bifurcation", {
  # input data
  milestone_network["directed"] <- FALSE # 设置为无向图
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

  # expected result
  expected_milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "D", "E", 10, FALSE,
    "E", "F", 1, FALSE,
    "E", "G", 1, FALSE,
  )
  expected_progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "D", "E", 0.3,
    "b", "D", "E", 0.7,
    "c", "D", "E", 0.05,
    "d", "D", "E", 0.95,
    "e", "E", "F", 0.5,
    "f", "E", "G", 0.5,
  )

  # execute function
  simp <- simplify_trajectory(trajectory)

  testthat::expect_equal(simp$milestone_network %>% arrange(from, to), as.data.frame(expected_milestone_network) %>% arrange(from, to))
  testthat::expect_equal(simp$progression %>% arrange(cell_id) , as.data.frame(expected_progressions) %>% arrange(cell_id))
})