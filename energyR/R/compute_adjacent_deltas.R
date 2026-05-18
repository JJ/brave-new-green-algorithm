#' Compute energy deltas for adjacent-pair (mixed) data layouts
#'
#' Handles the \emph{mixed} or \emph{inverted} experimental design used in
#' the OLA-2026 paper, where baseline and workload rows appear in adjacent
#' pairs (baseline immediately before each workload row) rather than in
#' symmetric triplets.  For every row whose \code{work} column equals
#' \code{work_name}, the energy delta is computed as the difference from the
#' immediately preceding row.
#'
#' This complements \code{\link{process_deltas}}, which handles the symmetric
#' interleaved design (baseline--workload--baseline triplets).
#'
#' @param data A data frame with at least \code{PKG}, \code{seconds}, and a
#'   \code{work} column.
#' @param work_name Character.  The value of \code{work} that identifies
#'   workload rows.
#' @param work_col Character.  Name of the column that distinguishes
#'   workload from baseline rows.  Default: \code{"work"}.
#'
#' @return The input data frame with two new columns: \code{delta_PKG} and
#'   \code{delta_seconds}, filled only for workload rows (all other rows
#'   remain \code{NA}).
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' @examples
#' df <- data.frame(
#'   PKG     = c(100, 115, 102, 118, 99, 114),
#'   seconds = c(1.0, 1.1, 1.0, 1.1, 1.0, 1.1),
#'   work    = c("base", "workload", "base", "workload", "base", "workload")
#' )
#' compute_adjacent_deltas(df, "workload")
#'
#' @export
compute_adjacent_deltas <- function(data, work_name, work_col = "work") {
  n <- nrow(data)
  data$delta_PKG     <- NA_real_
  data$delta_seconds <- NA_real_
  for (i in seq_len(n)) {
    if (i > 1 && data[[work_col]][i] == work_name) {
      data$delta_PKG[i]     <- data$PKG[i]     - data$PKG[i - 1]
      data$delta_seconds[i] <- data$seconds[i] - data$seconds[i - 1]
    }
  }
  return(data)
}
