library(testthat)
library(dynwrap)
library(tidyverse)
context("test convert_progressions_to_milestone_percentages")

# 星形网络
id <- "a"
cell_ids <- c("a", "b", "c")
milestone_ids <- c("milestone_begin", "A", "B", "C")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "milestone_begin", "A", 1, TRUE,
  "milestone_begin", "B", 1, TRUE,
  "milestone_begin", "C", 1, TRUE,
)
end_state_probabilities <- tribble(
  ~cell_id, ~A, ~B, ~C,
  "a", 0.5, 0.2, 0.2,
  "b", 0.2, 0.5, 0.2,
  "c", 0.2, 0.2, 0.5,
)
progressions <- end_state_probabilities %>%
  gather("to", "percentage", -cell_id) %>%
  mutate(from = "milestone_begin") %>%
  select(cell_id, from, to, percentage)


test_that("Testing add_end_state_probabilities 3 state", {
  milestone_percentages <- convert_progressions_to_milestone_percentages(
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions
  )
  expected_milestone_percentages <- tribble(
    ~cell_id, ~milestone_id, ~percentage,
    "a", "milestone_begin", 0.1, # 对于起点处， percentage = 1 - sum(other percentage)
    "a", "A", 0.5,
    "a", "B", 0.2,
    "a", "C", 0.2,
    "b", "milestone_begin", 0.1,
    "b", "A", 0.2,
    "b", "B", 0.5,
    "b", "C", 0.2,
    "c", "milestone_begin", 0.1,
    "c", "A", 0.2,
    "c", "B", 0.2,
    "c", "C", 0.5,
  )
  expect_equal(milestone_percentages %>% arrange(cell_id, milestone_id), expected_milestone_percentages %>% arrange(cell_id, milestone_id))
})



