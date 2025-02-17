library(testthat)
library(dynwrap)
library(tidyverse)


context("Testing add_cell_graph")

# cell data
cell_ids <- c("W", "X", "Y", "Z", "A", "WbX", "XcZ", "XeY", "ZfA", "a", "b", "c", "d", "e", "f")

wr_orig <- wrap_data(
  id = "test",
  cell_ids = cell_ids
)


test_that("Testing add_cell_graph", {
  cell_graph <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    "W", "WbX", 0.8, F,
    "WbX", "X", 0.2, F,
    "X", "XeY", 0.5, F,
    "XeY", "Y", 0.5, F,
    "X", "XcZ", 0.2, F,
    "XcZ", "Z", 0.8, F,
    "Z", "ZfA", 0.2, F,
    "ZfA", "A", 0.8, F,
    "W", "a", 0.5, F,
    "WbX", "b", 0.5, F,
    "XcZ", "c", 0.5, F,
    "Z", "d", 0.5, F,
    "XeY", "e", 0.2, F,
    "ZfA", "f", 0.5, F,
  )

  to_keep <- c(W = T, X = T, Y = T, Z = T, A = T, WbX = T, XcZ = T, XeY = T, ZfA = T, a = F, b = F, c = F, d = F, e = F, f = F)

  wr <- wr_orig %>% add_cell_graph(
    cell_graph = cell_graph,
    to_keep = to_keep,
    milestone_prefix = "ML_"
  )

  expected_milestone_ids <- paste0("ML_", c("W", "X", "Y", "A"))
  expected_milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "ML_W", "ML_X", 1, F,
    "ML_X", "ML_Y", 1, F,
    "ML_X", "ML_A", 2, F,
  )
  as.list(expected_milestone_network)

  expected_progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "W", "ML_W", "ML_X", 0.0,
    "X", "ML_W", "ML_X", 1.0,
    "Y", "ML_X", "ML_Y", 1.0,
    "Z", "ML_X", "ML_A", 0.5,
    "A", "ML_X", "ML_A", 1.0,
    "WbX", "ML_W", "ML_X", 0.8,
    "XcZ", "ML_X", "ML_A", 0.1,
    "XeY", "ML_X", "ML_Y", 0.5,
    "ZfA", "ML_X", "ML_A", 0.6,
    "a", "ML_W", "ML_X", 0.0,
    "b", "ML_W", "ML_X", 0.8,
    "c", "ML_X", "ML_A", 0.1,
    "d", "ML_X", "ML_A", 0.5,
    "e", "ML_X", "ML_Y", 0.5,
    "f", "ML_X", "ML_A", 0.6,
  )

  expect_equal(wr$milestone_ids, expected_milestone_ids)
  expect_equivalent(wr$milestone_network %>% arrange(from, to), expected_milestone_network %>% arrange(from, to)) # 排序后对比，expect_equivalent忽略表格类型
  expect_equivalent(wr$progressions %>% arrange(cell_id), expected_progressions %>% arrange(cell_id))
})
