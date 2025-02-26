#' Constructs a multifurcating trajectory using end state probabilities
#'
#' Constructs a multifurcating trajectory using the pseudotime values of each cell and their end state probabilities.
#' If pseudotime values are not given, will use pseudotime already present in the dataset.
#'
#' @inheritParams common_param
#' @param pseudotime A named vector of pseudo times.
#' @param end_state_probabilities A dataframe containing the *cell_id* and additional numeric columns containing the probability for every end milestone. If the tibble contains only a cell_id column, the data will be processed using `add_linear_trajectory`
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... Extras to be added to the trajectory
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
#' dataset <- wrap_data(cell_ids = letters)
#'
#' pseudotime <- runif(length(dataset$cell_ids))
#' names(pseudotime) <- dataset$cell_ids
#' pseudotime
#' end_state_probabilities <- tibble::tibble(
#'   cell_id = dataset$cell_ids,
#'   A = runif(length(dataset$cell_ids)),
#'   B = 1-A
#' )
#' end_state_probabilities
#' trajectory <- add_end_state_probabilities(dataset, end_state_probabilities, pseudotime)
#'
#' # for plotting the result, install dynplot
#' #- dynplot::plot_graph(trajectory)
add_end_state_probabilities <- function(
  dataset,
  end_state_probabilities,
  pseudotime = NULL,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  assert_that(is_data_wrapper(dataset))

  # process pseudotime
  if (!is.null(pseudotime)) {
    pseudotime <- process_pseudotime(dataset, pseudotime)
    # scale pseudotime
    if (do_scale_minmax) {
      pseudotime <- scale_minmax(pseudotime)
    } else {
      assert_that(all(0 <= pseudotime & pseudotime <= 1))
    }
    dataset <- dataset %>% add_pseudotime(pseudotime)
  } else {
    assert_that("pseudotime" %in% names(dataset))
    pseudotime <- dataset$pseudotime
  }

  # process end_state_probabilities
  assert_that("cell_id" %in% colnames(end_state_probabilities))


  if (ncol(end_state_probabilities) == 1) {
    # process as linear of no end_state_probabilities are provided
    # 只有一个终端状态，就是线性轨迹了
    dataset %>%
      add_linear_trajectory(
        pseudotime = pseudotime,
        directed = TRUE,
        do_scale_minmax = do_scale_minmax
      )
  } else {
    # construct milestone ids and milestone network
    # 多个终端状态， 构建里程碑网络
    start_milestone_id <- "milestone_begin" # 起始点是一个完全虚拟点
    end_milestone_ids <- colnames(end_state_probabilities)[colnames(end_state_probabilities) != "cell_id"] # 终端点从列名中提取
    milestone_ids <- c(start_milestone_id, end_milestone_ids)

    # 起始点作为中心的星型里程碑网络
    milestone_network <- tibble(
      from = start_milestone_id,
      to = end_milestone_ids,
      length = 1,
      directed = TRUE
    )

    # one divergence region
    # 添加发散区域，由所有里程碑节点共同构成构成
    divergence_regions <- tibble(
      milestone_id = milestone_ids,
      divergence_id = "D",
      is_start = milestone_ids == start_milestone_id
    )

    # construct progressions
    # 概率矩阵宽数据转化为长数据
    progressions <- end_state_probabilities %>%
      gather("to", "percentage", -cell_id) %>% # 忽略cell_id列，宽数据转化为长数据
      mutate(from = start_milestone_id) %>%
      group_by(cell_id) %>%
      mutate(percentage = percentage / sum(percentage) * pseudotime[cell_id]) %>%  # 缩放使其之和为1 # scale percentage so that sum = 1
      ungroup()

    # return output
    add_trajectory(
      dataset = dataset,
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      progressions = progressions,
      end_state_probabilities = end_state_probabilities,
      ...
    )
  }
}
