#include <Rcpp.h>
using namespace Rcpp;

//' Project a set of points to to set of segments
//'
//' Finds the projection index for a matrix of points \code{x}, when
//' projected onto a set of segments defined by \code{segment_start} and \code{segment_end}.
//'
//' @param x a matrix of data points.
//' @param segment_start a matrix of segment start points.
//' @param segment_end a matrix of segment end points.
//'
//' @return A list with components
//'   \item{x_proj}{a matrix of projections of \code{x} onto the given segments.}
//'   \item{segment}{the index of the segment a point is projected on}
//'   \item{progression}{the progression of a projection along its segment}
//'   \item{distance}{the distance from each point in \code{x} to its projection in \code{x_proj}}
//'
//' @examples
//' x <- matrix(rnorm(50, 0, .5), ncol = 2)
//' segfrom <- matrix(c(0, 1, 0, -1, 1, 0, -1, 0), ncol = 2, byrow = TRUE)
//' segto <- segfrom / 10
//' fit <- project_to_segments(x, segfrom, segto)
//'
//' str(fit) # examine output
//'
//' @export
// [[Rcpp::export]]
List project_to_segments(NumericMatrix x, NumericMatrix segment_start, NumericMatrix segment_end) {
  // Determine dimensionalities
  // 维度检查
  int ncols = x.ncol();
  int npts = x.nrow();
  int nsegs = segment_start.nrow();

  if (segment_start.ncol() != ncols || segment_end.ncol() != ncols) {
    stop("the number of columns of 'x', 'segment_start' and 'segment_end' should be exactly the same");
  }
  if (segment_end.nrow() != nsegs) {
    stop("the number of rows of 'segment_start' and 'segment_end' should be exactly the same");
  }

  // calculate lengths of the segments
  // 计算每段线段长度，np.numpy很方便
  NumericVector length = no_init(nsegs);
  NumericMatrix diff = no_init(nsegs, ncols); // 线段向量
  for (int i = 0; i < nsegs; ++i) {
    // OPTIMISATION: compute length manually
    //   diff(i, _) = segment_end(i, _) - segment_start(i, _);
    //   length[i] = sum(pow(diff(i, _), 2));
    double l = 0;
    for (int k = 0; k < ncols; ++k) {
      double value = segment_end(i, k) - segment_start(i, k);
      diff(i, k) = value;
      l += value * value;
    }
    length[i] = l;
    // END OPTIMISATION
  }

  // Output objects
  // 输出对象
  NumericMatrix x_proj = no_init(npts, ncols);
  NumericVector distance = no_init(npts);
  IntegerVector segment = no_init(npts);
  NumericVector progression = no_init(npts);

  // OPTIMISATION: Allocate space for intermediate objects
  NumericVector p = no_init(ncols);
  NumericVector p_proj = no_init(ncols);
  NumericVector test_p_proj = no_init(ncols);
  // END OPTIMISATION

  // iterate over points in x
  // 遍历所有点，找到每个点最近的投影
  for (int i = 0; i < npts; ++i) {
    // OPTIMISATION: Preallocate p
    //   NumericVector p = x(i, _);
    for (int k = 0; k < ncols; ++k) {
      p[k] = x(i, k);
    }

    // store information on the closest segment
    int best_segment = -1;
    double best_progression = -1;
    double best_distance = R_PosInf;

    // iterate over the segments
    // 对于特定的点，尝试将其投影到所有线段上
    for (int segi = 0; segi < nsegs; ++segi) {

      // project p orthogonally onto the segment
      // 向量: sp(start->p), se(start->end)
      // 把点p投影到线段上: proj = cos(sp, se)*|sp| = sp*se/|se|
      // OPTIMISATION: do not compute diff1 and diff2 separately
      //   NumericVector diff1 = diff(segi, _);
      //   NumericVector diff2 = p - segment_start(segi, _);
      //   double test_progression = sum(diff1 * diff2) / length(segi);
      double test_progression = 0;
      for (int k = 0; k < ncols; ++k) {
        test_progression += diff(segi, k) * (p[k] - segment_start(segi, k));
      }
      test_progression /= length(segi);
      // END OPTIMISATION

      if (test_progression < 0) {
        test_progression = 0.0;
      }
      if (test_progression > 1) {
        test_progression = 1.0;
      }

      // calculate position of projection and the distance
      // 计算投影位置和距离
      // OPTIMISATION: compute di and n_test manually
      //   NumericVector test_p_proj = segment_start(segi, _) + test_progression * diff1;
      //   double test_distance = sum(pow(test_p_proj - p, 2.0));
      double test_distance = 0;
      for (int k = 0; k < ncols; ++k) {
        double value = segment_start(segi, k) + test_progression * diff(segi, k);
        test_p_proj(k) = value;
        test_distance += (value - p[k]) * (value - p[k]);
      }
      // END OPTIMISATION

      // if this is better than what was found earlier, store it
      // 此时投影距离最近，保存
      if (test_distance < best_distance) {
        best_distance = test_distance;
        best_segment = segi;
        best_progression = test_progression;
        for (int k = 0; k < ncols; ++k) {
          p_proj[k] = test_p_proj[k];
        }
      }
    }

    // save the best projection to the output data structures
    // 保存最好的投影位置
    for (int k = 0; k < ncols; ++k) {
      x_proj(i, k) = p_proj(k);
    }
    distance[i] = best_distance;
    segment[i] = best_segment + 1; // increase by 1 for R
    progression[i] = best_progression;
  }

  rownames(x_proj) = rownames(x);
  colnames(x_proj) = colnames(x);
  distance.attr("names") = rownames(x);
  segment.attr("names") = rownames(x);
  progression.attr("names") = rownames(x);

  // return output
  List ret;
  ret["x_proj"] = x_proj;
  ret["distance"] = distance;
  ret["segment"] = segment;
  ret["progression"] = progression;

  return ret;
}
