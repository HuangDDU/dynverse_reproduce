library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing add_cyclic_trajectory")

# 使用与add_linear_trajectory相同的数据
id <- "test"
cell_ids <- c("a", "b", "c", "d", "e", "f")
extras <- list("extras")

pseudotime <- c(0, .1, .4, .5, .8, 1) %>% set_names(cell_ids)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_cyclic_trajectory", {
  wr <-
    wr_orig %>%
      add_cyclic_trajectory(
        pseudotime = pseudotime,
        do_scale_minmax = TRUE,
        directed = FALSE,
        extras = extras
      )

  # 预期输出
  expected_milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", 1, FALSE,
    "B", "C", 1, FALSE,
    "C", "A", 1, FALSE,
  )
  expected_progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "A", "B", 0,
    "b", "A", "B", 0.3,
    "c", "B", "C", 0.2,
    "d", "B", "C", 0.5,
    "e", "C", "A", 0.4,
    "f", 'C', "A", 1,
  )

  expect_equal(wr$milestone_network, expected_milestone_network)
  expect_equivalent(wr$progressions, expected_progressions)
})

