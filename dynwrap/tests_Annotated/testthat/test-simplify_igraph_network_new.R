# 简化igraph网络
library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing simplify_igraph_network")

# input data
directed <- TRUE
net <- tribble(
  ~from, ~to,
  "A", "B",
  "B", "C",
  "C", "D",
)
gr <- igraph::graph_from_data_frame(net, directed = directed)
edge_points <- tribble(
  ~id, ~from, ~to, ~percentage,
  "a", "A", "B", 0.3,
  "b", "A", "B", 0.6,
  "c", "B", "C", 0.2,
  "d", "B", "C", 0.8,
  "e", "C", "D", 0.4,
)

# expected result
expected_net <- tribble(
  ~from, ~to, ~weight, ~directed,
  "A", "D", 3, TRUE
)
expected_edge_points <- tribble(
  ~id, ~from, ~to, ~percentage,
  "a", "A", "D", 0.1,
  "b", "A", "D", 0.2,
  "c", "A", "D", 0.4,
  "d", "A", "D", 0.6,
  "e", "A", "D", 0.8,
)

test_that("directed linear", {
  # execute function
  out <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, edge_points = edge_points)
  new_gr <- out$gr
  new_edge_points <- out$edge_points
  new_net <- tibble(igraph::as_data_frame(new_gr))
  new_edge_points <- tibble(new_edge_points[, c("id", "from", "to", "percentage")])

  testthat::expect_equal(new_net, expected_net)
  testthat::expect_equal(new_edge_points, expected_edge_points)
})


test_that("undirected linear", {
  # input data
  gr <- igraph::graph_from_data_frame(net, directed = FALSE)
  expected_net["directed"] <-  FALSE

  # expected result
  expected_milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "D", 3, FALSE
  )
  # execute function
  out <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, edge_points = edge_points)
  new_gr <- out$gr
  new_edge_points <- out$edge_points
  new_net <- tibble(igraph::as_data_frame(new_gr))
  new_edge_points <- tibble(new_edge_points[, c("id", "from", "to", "percentage")])

  testthat::expect_equal(new_net, expected_net)
  testthat::expect_equal(new_edge_points, expected_edge_points)
})
