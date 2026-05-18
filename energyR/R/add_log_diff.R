#' Add logarithm-of-fitness-difference column
#'
#' Appends a column containing \code{log10(diff_fitness)} to \code{data}.
#' This transformed variable is used throughout the papers (walcom, evoapps,
#' lion) as the y-axis in fitness-vs-energy scatter plots, because raw fitness
#' differences span many orders of magnitude.
#'
#' @param data A data frame containing a \code{diff_fitness} column (or the
#'   column named by \code{col}).
#' @param col Character.  Name of the source fitness-difference column.
#'   Default: \code{"diff_fitness"}.
#' @param new_col Character.  Name of the new log-transformed column.
#'   Default: \code{"log_diff"}.
#'
#' @return The input data frame with one additional numeric column.
#'
#' @references
#' Merelo-Guervos, Juan J. and Romero-López, Gustavo and Garcia-Valdez, Mario
#' (2025). "Measuring Energy Consumption of BBOB Fitness Functions." In
#' \emph{Applications of Evolutionary Computation (EvoApplications 2025)},
#' Lecture Notes in Computer Science, vol. 15613, pp. 240--254. Springer.
#' \doi{10.1007/978-3-031-90065-5_15}
#'
#' Merelo-Guervos, Juan J. and Merelo-Molina, Cecilia and Garcia-Sanchez,
#' Pablo and Garcia-Valdez, Mario (2026). "Is there a (carbon-) free lunch?
#' Energy/performance tradeoffs in population-based metaheuristics." Accepted,
#' LION 20.
#'
#' @examples
#' df <- data.frame(diff_fitness = c(0.001, 0.01, 0.1, 1))
#' add_log_diff(df)$log_diff
#'
#' @export
add_log_diff <- function(data, col = "diff_fitness", new_col = "log_diff") {
  data[[new_col]] <- log10(data[[col]])
  return(data)
}
