library(testthat)
library(dynwrap)
library(tidyverse)


context("Testing waypoints")

get_test_wrap_data <- function (){
  cell_ids <- c("a", "b", "c", "d", "e", "f") # 简化细胞数量
  milestone_ids <- c("W", "X", "Y", "Z", "A")

  milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "W", "X", 1, TRUE,
    "X", "Y", 1, TRUE,
    "X", "Z", 1, TRUE,
    "Z", "A", 2, TRUE
  ) # 简化milstone网络长度,边数+n_waypoint

  divergence_regions <- tribble(
    ~divergence_id, ~milestone_id, ~is_start,
    "XYZ", "X", TRUE,
    "XYZ", "Y", FALSE,
    "XYZ", "Z", FALSE
  )

  milestone_percentages <- tribble(
    ~cell_id, ~milestone_id, ~percentage,
    "a", "W", 1,
    "b", "W", .2,
    "b", "X", .8,
    "c", "X", .8,
    "c", "Z", .2,
    "d", "Z", 1,
    "e", "X", .3,
    "e", "Y", .2,
    "e", "Z", .5,
    "f", "Z", .8,
    "f", "A", .2
  )

  dataset <- wrap_data("test", cell_ids)

  return(list(
    dataset=dataset,
    milestone_ids=milestone_ids,
    milestone_network=milestone_network,
    divergence_regions=divergence_regions,
    milestone_percentages=milestone_percentages)
  )
}

# 主要测试放在函数里,因为后需要source到其他R文件
test_wrap_add_waypoints <- function (){
  # 获得数据
  test_wrap_data <- get_test_wrap_data()
  dataset <- test_wrap_data$dataset
  milestone_ids <- test_wrap_data$milestone_ids
  milestone_network <- test_wrap_data$milestone_network
  divergence_regions <- test_wrap_data$divergence_regions
  milestone_percentages <- test_wrap_data$milestone_percentages

  trajectory <- dataset %>%
    add_trajectory(milestone_ids=milestone_ids,
                   milestone_network=milestone_network,
                   divergence_regions=divergence_regions,
                   milestone_percentages = milestone_percentages)
  wp <- select_waypoints(trajectory, trafo=function (x){x}, resolution=1) # milstone边上每间隔1个单位找一个waypoint
}

