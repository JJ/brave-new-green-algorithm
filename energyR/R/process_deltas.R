#' Process interleaved baseline/workload energy data
#'
#' Computes energy and time deltas for workload measurements that are
#' interleaved with baseline measurements. The data is assumed to be organised
#' in blocks of \code{block_size} rows where, within each block, even-indexed
#' positions (rows 2, 4, ..., \code{block_size - 1}) are workload runs and the
#' surrounding odd-indexed rows are baseline runs. The delta for each workload
#' row is computed as the workload value minus the average of the immediately
#' preceding and following baseline values.
#'
#' This layout is the standard output format produced by the BNA experiment
#' scripts in the Brave New Green Algorithm project.
#'
#' @param data A data frame with at least the columns \code{PKG} (package
#'   energy in Joules) and \code{seconds} (wall-clock time per measurement).
#' @param block_size Integer. Number of rows in each interleaved block
#'   (baseline + workload). Default: 61 (one initial baseline, 30 workload
#'   runs each surrounded by a baseline, giving 1 + 30*2 = 61 rows).
#'
#' @return A data frame identical to \code{data} with two additional columns,
#'   \code{delta_PKG} and \code{delta_seconds}, containing the computed deltas.
#'   Rows for which \code{delta_PKG} is zero (i.e., baseline rows) are removed.
#'
#' @examples
#' # Toy example: 61 rows, alternating baseline and workload
#' set.seed(42)
#' n <- 61
#' df <- data.frame(
#'   PKG     = c(100, replicate(30, c(100 + rnorm(1, 5, 2), 100 + rnorm(1)))),
#'   seconds = c(1.0, replicate(30, c(1.0 + rnorm(1, 0.1, 0.01), 1.0 + rnorm(1, 0.01))))
#' )
#' result <- process_deltas(df)
#' head(result)
#'
#' @export
process_deltas <- function(data, block_size = 61) {
  n <- nrow(data)
  data$delta_PKG <- rep(NA_real_, n)
  data$delta_seconds <- rep(NA_real_, n)
  for (k in seq(from = 0, to = n - 1, by = block_size)) {
    for (i in seq(from = 2, to = block_size - 1, by = 2)) {
      index <- k + i
      if (index > n || index + 1 > n) break
      data$delta_seconds[index] <- data$seconds[index] -
        (data$seconds[index - 1] + data$seconds[index + 1]) / 2
      data$delta_PKG[index] <- data$PKG[index] -
        (data$PKG[index - 1] + data$PKG[index + 1]) / 2
    }
  }
  return(data[!is.na(data$delta_PKG) & data$delta_PKG != 0, ])
}
