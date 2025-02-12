#' Calculate (column-wise) distances/similarity between two matrices
#'
#' These matrices can be dense or sparse.
#'
#' @param x A numeric matrix, dense or sparse.
#' @param y (Optional) a numeric matrix, dense or sparse, with `nrow(x) == nrow(y)`.
#' @param method Which distance method to use. Options are: `"cosine"`, `"pearson"`, `"spearman"`, `"euclidean"`, and `"manhattan"`.
#' @inheritParams proxyC::simil
#'
#' @export
#'
#' @importFrom proxyC dist
#'
#' @examples
#' ## Generate two matrices with 50 and 100 samples
#' library(Matrix)
#' x <- Matrix::rsparsematrix(50, 1000, .01)
#' y <- Matrix::rsparsematrix(100, 1000, .01)
#'
#' dist_euclidean <- calculate_distance(x, y, method = "euclidean")
#' dist_manhattan <- calculate_distance(x, y, method = "manhattan")
#' dist_spearman <- calculate_distance(x, y, method = "spearman")
#' dist_pearson <- calculate_distance(x, y, method = "pearson")
#' dist_angular <- calculate_distance(x, y, method = "cosine")
calculate_distance <- function(
  x,
  y = NULL,
  method = c("pearson", "spearman", "cosine", "euclidean", "chisquared",
             "hamming", "kullback", "manhattan", "maximum", "canberra", "minkowski"),
  margin = 1,
  diag = FALSE,
  drop0 = FALSE
) {
  method <- match.arg(method)
  input <- .process_input_matrices(x = x, y = y, margin = margin)
  x <- input$x
  y <- input$y

  dis <-
    if (method %in% c("cosine", "pearson", "spearman")) {
      # 基于相似性的距离, 相似性越高, 距离越近
      # 先计算相似性
      sim <- calculate_similarity(x = x, y = y, method = method, margin = 2, diag = diag, drop0 = drop0)
      # 相似性转化为距离, 不同的相似性转化为距离的公式不一样
      if (method == "cosine") {
        1 - 2 * acos(sim) / pi
      } else {
        1 - (sim + 1) / 2
      }
    } else {
      # 其他直接距离计算的公式
      proxyC::dist(x = x, y = y, method = method, margin = 2, diag = diag, drop0 = drop0, use_nan = FALSE)
    }

  if (is.null(y)) {
    # 只有一个矩阵计算距离矩阵,则对角线上为0
    diag(dis) <- 0
  }

  as.matrix(dis)
}

#' @rdname calculate_distance
#' @export
list_distance_methods <- function() eval(formals(calculate_distance)$method)

#' @rdname calculate_distance
#' @export
#' @importFrom proxyC simil
calculate_similarity <- function(
  x,
  y = NULL,
  margin = 1,
  method = c("spearman", "pearson", "cosine"),
  diag = FALSE,
  drop0 = FALSE
) {
  method <- match.arg(method)
  input <- .process_input_matrices(x = x, y = y, margin = margin)
  x <- input$x
  y <- input$y

  # run method
  if (method %in% c("pearson", "spearman")) {
    if (method == "spearman") {
      x <- spearman_rank_sparse(x)
      if (!is.null(y)) {
        y <- spearman_rank_sparse(y)
      }
    }
    method <- "correlation"
  }

  sim <- proxyC::simil(x = x, y = y, method = method, margin = 2, diag = diag, drop0 = drop0, use_nan = FALSE)
  # set nans to 0
  sim@x[is.nan(sim@x)] <- 0

  # fixes due to rounding errors
  if (method %in% c("pearson", "spearman", "cosine")) {
    sim@x[sim@x > 1] <- 1

    if (is.null(y)) {
      diag(sim) <- 1
    }
  }

  as.matrix(sim)
}

#' @rdname calculate_distance
#' @export
list_similarity_methods <- function() eval(formals(calculate_similarity)$method)

#' @importFrom Matrix t
#' @importFrom methods as
.process_input_matrices <- function(x, y, margin) {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) x <- as(x, "CsparseMatrix")

  if (!is.null(y)) {
    if (is.data.frame(y)) y <- as.matrix(y)
    if (is.matrix(y)) y <- as(y, "CsparseMatrix")
  }

  assert_that(
    is_sparse(x),
    is.null(y) || is_sparse(y)
  )

  # 维度转化为 特征数*样本数
  if (margin == 1) {
    if (!is.null(y)) {
      assert_that(ncol(x) == ncol(y))
      y <- Matrix::t(y)
    }
    x <- Matrix::t(x)
  } else {
    if (!is.null(y)) {
      assert_that(nrow(x) == nrow(y))
    }
  }

  list(x = x, y = y)
}

# 没什么用
.process_pairwise_matrix <- function(mat, x, y) {
  assert_that(
    ncol(x) == nrow(mat),
    ncol(y) == ncol(mat)
  )
  dimnames(mat) <- list(colnames(x), colnames(y))
  mat
}


#' These functions will be removed soon
#'
#' Use [calculate_distance()] instead.
#'
#' @inheritParams calculate_distance
#' @export
#' @rdname deprecated
# 没什么用
euclidean_distance <- function(x, y = NULL) {
  .Deprecated("calculate_distance")
  as.matrix(calculate_distance(x, y, method = "euclidean"))
}

#' @export
#' @rdname deprecated
correlation_distance <- function(x, y = NULL) {
  .Deprecated("calculate_distance")
  as.matrix(calculate_distance(x, y, method = "spearman"))
}
