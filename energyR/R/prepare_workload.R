#' Prepare workload data for analysis
#'
#' Runs the standard separated-run preprocessing pipeline by applying
#' \code{\link{compute_deltas}}, \code{\link{add_log_diff}}, and
#' \code{\link{add_pop_dim_label}} in one call.
#'
#' @param baseline_summary Data frame produced by \code{\link{summarize_baseline}}.
#' @param workload_data Workload data frame with workload measurements.
#' @param diff_col Character. Column used to compute log fitness differences.
#'   Default: \code{"diff_fitness"}.
#' @param log_col Character. Output column name for log differences.
#'   Default: \code{"log_diff"}.
#' @param add_label Logical. If \code{TRUE} (default), add a combined
#'   population/dimension label column.
#' @param pop_col Character. Population column used by
#'   \code{\link{add_pop_dim_label}}. Default: \code{"population_size"}.
#' @param dim_col Character. Dimension column used by
#'   \code{\link{add_pop_dim_label}}. Default: \code{"dimension"}.
#' @param label_col Character. Output label column name when
#'   \code{add_label = TRUE}. Default: \code{"pop_dim"}.
#'
#' @return A workload data frame with delta columns and optional derived columns.
#'
#' @examples
#' baseline <- data.frame(
#'   dimension = 3,
#'   population_size = 200,
#'   median_energy = 100,
#'   median_time = 1
#' )
#' workload <- data.frame(
#'   dimension = 3,
#'   population_size = 200,
#'   PKG = 115,
#'   seconds = 1.1,
#'   diff_fitness = 0.01
#' )
#' prepare_workload(baseline, workload)
#'
#' @export
prepare_workload <- function(baseline_summary,
                             workload_data,
                             diff_col = "diff_fitness",
                             log_col = "log_diff",
                             add_label = TRUE,
                             pop_col = "population_size",
                             dim_col = "dimension",
                             label_col = "pop_dim") {
  workload_data <- compute_deltas(baseline_summary, workload_data)

  if (diff_col %in% names(workload_data)) {
    workload_data <- add_log_diff(
      workload_data,
      col = diff_col,
      new_col = log_col
    )
  }

  if (isTRUE(add_label)) {
    workload_data <- add_pop_dim_label(
      workload_data,
      pop_col = pop_col,
      dim_col = dim_col,
      label_col = label_col
    )
  }

  return(workload_data)
}
