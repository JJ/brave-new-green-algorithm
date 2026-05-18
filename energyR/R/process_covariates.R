#' Add baseline covariate columns to interleaved energy data
#'
#' Extends the data frame with the PKG energy and time values of the baseline
#' measurements that immediately precede and follow each workload measurement.
#' The expected data layout is the same interleaved format described in
#' \code{\link{process_deltas}}: blocks of \code{block_size} rows where
#' even-indexed positions within each block are workload runs.
#'
#' These covariate columns are useful for regression models that control for
#' hardware thermal state (hysteresis) at the time of each measurement.
#'
#' @param data A data frame with at least the columns \code{PKG} and
#'   \code{seconds}.
#' @param block_size Integer. Number of rows in each interleaved block.
#'   Default: 61.
#'
#' @return A data frame identical to \code{data} with four additional columns:
#'   \describe{
#'     \item{seconds_baseline_prev}{Wall-clock time of the preceding baseline row.}
#'     \item{seconds_baseline_post}{Wall-clock time of the following baseline row.}
#'     \item{PKG_baseline_prev}{PKG energy of the preceding baseline row.}
#'     \item{PKG_baseline_post}{PKG energy of the following baseline row.}
#'   }
#'   Rows for which \code{PKG_baseline_post} is zero (i.e., pure baseline rows)
#'   are removed.
#'
#' @examples
#' set.seed(1)
#' n <- 61
#' df <- data.frame(
#'   PKG     = c(100, replicate(30, c(100 + rnorm(1, 5, 2), 100 + rnorm(1)))),
#'   seconds = c(1.0, replicate(30, c(1.0 + rnorm(1, 0.1, 0.01), 1.0 + rnorm(1, 0.01))))
#' )
#' result <- process_covariates(df)
#' head(result)
#'
#' @export
process_covariates <- function(data, block_size = 61) {
  n <- nrow(data)
  data$seconds_baseline_prev <- rep(NA_real_, n)
  data$seconds_baseline_post <- rep(NA_real_, n)
  data$PKG_baseline_prev     <- rep(NA_real_, n)
  data$PKG_baseline_post     <- rep(NA_real_, n)
  for (k in seq(from = 0, to = n - 1, by = block_size)) {
    for (i in seq(from = 2, to = block_size - 1, by = 2)) {
      index <- k + i
      if (index > n || index + 1 > n) break
      data$seconds_baseline_prev[index] <- data$seconds[index - 1]
      data$seconds_baseline_post[index] <- data$seconds[index + 1]
      data$PKG_baseline_prev[index]     <- data$PKG[index - 1]
      data$PKG_baseline_post[index]     <- data$PKG[index + 1]
    }
  }
  return(data[!is.na(data$PKG_baseline_post) & data$PKG_baseline_post != 0, ])
}
