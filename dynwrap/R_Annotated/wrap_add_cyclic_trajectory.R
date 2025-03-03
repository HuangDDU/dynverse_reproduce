#' Constructs a circular trajectory using the pseudotime values of each cell.
#'
#' The pseudotime is divided into three equally sized segments, and are placed within a trajectory in the form A -> B -> C -> A
#'
#' @inheritParams common_param
#' @param pseudotime A named vector of pseudo times.
#' @param directed Whether or not the directionality of the pseudotime is predicted.
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... extra information to be stored in the wrapper.
#'
#' @inherit add_trajectory return
#'
#' @keywords create_trajectory
#' 
#' @return A trajectory object
#'
#' @export
#'
#' @examples
#' library(tibble)
#' dataset <- wrap_data(cell_ids = letters)
#'
#' pseudotime <- tibble(cell_id = dataset$cell_ids, pseudotime = runif(length(dataset$cell_ids)))
#' pseudotime
#' trajectory <- add_cyclic_trajectory(dataset, pseudotime)
#'
#' # for plotting the result, install dynplot
#' #- dynplot::plot_graph(trajectory)
add_cyclic_trajectory <- function(
  dataset,
  pseudotime,
  directed = FALSE,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  assert_that(is_data_wrapper(dataset))

  pseudotime <- process_pseudotime(dataset, pseudotime)

  # scale pseudotime
  # 归一化
  if (do_scale_minmax) {
    pseudotime <- scale_minmax(pseudotime)
  } else {
    assert_that(all(0 <= pseudotime & pseudotime <= 1))
  }

  # construct milestones
  milestone_ids <- c("A", "B", "C")

  # construct milestone_network
  # 构建里程碑网络：A->B, B->C, C->A
  milestone_network <- tibble(
    from = milestone_ids,
    to = milestone_ids[c(2,3,1)],
    directed = directed,
    length = 1,
    edge_id = seq_along(milestone_ids)
  )

  # construct progressions
  # 构建细胞过程，3个分段内
  progressions <- tibble(
    time = 3 * pseudotime,
    cell_id = names(pseudotime)
  ) %>%
    mutate(edge_id = ifelse(time <= 1, 1L, ifelse(time <= 2, 2L, 3L))) %>% # 伪时间划分到3个分段内
    left_join(milestone_network, by = "edge_id") %>% # 拼接到特定里程碑边区间内
    mutate(percentage = time - (edge_id - 1)) %>% # 在里程碑边区间内部归一化
    select(cell_id, from, to, percentage)

  milestone_network <- milestone_network %>%
    select(from, to, length, directed)

  # return output
  add_trajectory(
    dataset = dataset,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    pseudotime = pseudotime,
    ...
  )
}
