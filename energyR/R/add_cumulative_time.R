#' Add cumulative time column
#'
#' Adds a column containing the cumulative sum of \code{seconds} to a data
#' frame.  This is the most common preprocessing step in BNA experiment
#' scripts — nearly every timeline and energy-vs-time plot requires a
#' monotonically increasing time axis.
#'
#' @param data A data frame containing a \code{seconds} column.
#' @param col Character.  Name of the new cumulative-time column.
#'   Default: \code{"cum_seconds"}.
#'
#' @return The input data frame with one additional numeric column.
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' Merelo-Guervos, Juan J. and Romero-Lopez, Gustavo and Garcia-Valdez, Mario
#' (2025). "Time-related effects in the measurement of energy consumption in
#' evolutionary algorithms." In \emph{Europar 2025: Parallel Processing
#' Workshops}.
#'
#' @examples
#' df <- data.frame(PKG = rnorm(10, 100, 5), seconds = rep(1.5, 10))
#' df <- add_cumulative_time(df)
#' head(df$cum_seconds)
#'
#' @export
add_cumulative_time <- function(data, col = "cum_seconds") {
  data[[col]] <- cumsum(data$seconds)
  return(data)
}
