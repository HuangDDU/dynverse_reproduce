library(testthat)
library(dynwrap)
library(tidyverse)

id <- "test"
cell_ids <- c("a", "b", "c", "d", "e", "f")
extras <- list("extras")

pseudotime <- c(0, .1, .4, .5, .8, 1) %>% set_names(cell_ids)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_linear_trajectory", {
  wr <-
    wr_orig %>%
    add_linear_trajectory(
      pseudotime = pseudotime,
      do_scale_minmax = TRUE,
      directed = FALSE,
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_trajectory(wr))

  # wrap_data测试
  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)

  expect_gt(cor(wr$pseudotime[cell_ids], pseudotime[cell_ids]), .9)
  # milestone相关测试
  expect_equivalent(length(wr$milestone_ids), 2)
  expect_equivalent(nrow(wr$milestone_network), 1)
  expect_equivalent(set_names(sort(unlist(wr$milestone_network[,c("from", "to")])), NULL), sort(wr$milestone_ids))
  expect_true(all(cell_ids %in% wr$progressions$cell_id))
  expect_equivalent(nrow(wr$progressions), length(cell_ids))
  # 类型测试
  expect_equivalent(wr$trajectory_type, "linear")
  expect_equivalent(wr$directed, FALSE)
})

