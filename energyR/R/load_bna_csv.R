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
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' Merelo Guervos, Juan Julian and Merelo-Molina, Cecilia (2025). "Analyzing
#' how the exploration/exploitation trade off in biologically-inspired
#' algorithms affects energy consumption." University of Granada.
#' \url{https://hdl.handle.net/10481/107864}
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
