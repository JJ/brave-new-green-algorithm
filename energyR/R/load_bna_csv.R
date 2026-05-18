#' Load a BNA experiment CSV file
#'
#' Reads a CSV file produced by the Brave New Algorithm experiment scripts and
#' optionally removes columns that are irrelevant for baseline data (such as
#' algorithm-specific parameters that are only meaningful for workload runs).
#'
#' @param file_path Character.  Path to the CSV file.
#' @param drop_baseline_cols Logical.  If \code{TRUE} (default), the columns
#'   \code{alpha}, \code{max_gens}, \code{different_seeds}, \code{diff_fitness},
#'   \code{generations}, and \code{evaluations} are removed.  Set to
#'   \code{FALSE} when loading workload files that need these columns.
#'
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' # Load a baseline CSV (drops algorithm-parameter columns)
#' baseline <- load_bna_csv("data/my-baseline.csv")
#'
#' # Load a workload CSV (keep all columns)
#' workload <- load_bna_csv("data/my-workload.csv", drop_baseline_cols = FALSE)
#' }
#'
#' @export
load_bna_csv <- function(file_path, drop_baseline_cols = TRUE) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  if (drop_baseline_cols) {
    drop_cols <- c("alpha", "max_gens", "different_seeds",
                   "diff_fitness", "generations", "evaluations")
    data[, intersect(names(data), drop_cols)] <- NULL
  }
  return(data)
}

#' Standard baseline column names
#'
#' A character vector of column names that are typically present only in
#' workload (not baseline) CSV files produced by BNA experiments.  Useful
#' for dropping irrelevant columns when working with baseline data.
#'
#' @format A character vector of length 6.
#' @export
null_baseline_columns <- c(
  "alpha", "max_gens", "different_seeds",
  "diff_fitness", "generations", "evaluations"
)
