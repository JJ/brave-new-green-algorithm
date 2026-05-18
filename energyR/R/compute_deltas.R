#' Compute energy and time deltas relative to a summarised baseline
#'
#' Subtracts the per-configuration (dimension x population size) baseline
#' median energy and time from each workload row, producing \code{delta_PKG}
#' and \code{delta_seconds} columns.  This is an alternative to
#' \code{\link{process_deltas}} for experimental designs where baseline and
#' workload runs are \emph{not} interleaved but are carried out separately.
#'
#' @param baseline_summary A data frame of baseline summary statistics, as
#'   returned by \code{\link{summarize_baseline}}.  Must contain the columns
#'   \code{dimension}, \code{population_size}, \code{median_energy}, and
#'   \code{median_time}.
#' @param workload A data frame of raw workload measurements.  Must contain
#'   the columns \code{dimension}, \code{population_size}, \code{PKG}, and
#'   \code{seconds}.
#'
#' @return The \code{workload} data frame with two additional (or overwritten)
#'   columns: \code{delta_PKG} and \code{delta_seconds}.
#'
#' @examples
#' # Build a minimal baseline summary
#' baseline_summary <- data.frame(
#'   dimension       = c(3, 5),
#'   population_size = c(200, 200),
#'   median_energy   = c(100, 110),
#'   median_time     = c(1.0, 1.1)
#' )
#' # Build a minimal workload data frame
#' workload <- data.frame(
#'   dimension       = rep(c(3, 5), each = 5),
#'   population_size = 200,
#'   PKG             = c(rnorm(5, 115, 5), rnorm(5, 125, 5)),
#'   seconds         = c(rnorm(5, 1.1, 0.05), rnorm(5, 1.2, 0.05))
#' )
#' result <- compute_deltas(baseline_summary, workload)
#' head(result[, c("dimension", "PKG", "delta_PKG")])
#'
#' @export
compute_deltas <- function(baseline_summary, workload) {
  workload$delta_PKG     <- 0
  workload$delta_seconds <- 0
  dims     <- unique(workload$dimension)
  pop_sizes <- unique(workload$population_size)
  for (dim in dims) {
    for (pop_size in pop_sizes) {
      mask <- workload$dimension == dim & workload$population_size == pop_size
      n_rows <- sum(mask)
      if (n_rows == 0) next
      brow <- baseline_summary$population_size == pop_size &
        baseline_summary$dimension == dim
      if (!any(brow)) next
      workload$delta_PKG[mask] <- workload$PKG[mask] -
        rep(baseline_summary$median_energy[brow], n_rows)
      workload$delta_seconds[mask] <- workload$seconds[mask] -
        rep(baseline_summary$median_time[brow], n_rows)
    }
  }
  return(workload)
}
