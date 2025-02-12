# 简化igraph网络
library(testthat)
library(dynwrap)
library(tidyverse)

context("Testing simplify_igraph_network")

test_that("directed_linear", {
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

  # 简化milestone network的同时，需要简化网络上边的细胞节点
  out <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, edge_points = edge_points)
  new_gr <- out$gr
  new_edge_points <- out$edge_points

  # 预期输出
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

  new_net <- tibble(igraph::as_data_frame(new_gr))
  testthat::expect_equal(new_net, expected_net)
  new_edge_points <- tibble(new_edge_points[, c("id", "from", "to", "percentage")])
  testthat::expect_equal(new_edge_points, expected_edge_points)
})
