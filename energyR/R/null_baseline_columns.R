#' Standard baseline column names
#'
#' A character vector of column names that are typically present only in
#' workload (not baseline) CSV files produced by BNA experiments.  Useful
#' for dropping irrelevant columns when working with baseline data.
#'
#' @format A character vector of length 6.
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' @export
null_baseline_columns <- c(
  "alpha", "max_gens", "different_seeds",
  "diff_fitness", "generations", "evaluations"
)
