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

#' Filter out zero-energy rows
#'
#' Removes rows where the energy column equals zero.  Zero-energy readings
#' are a known measurement artefact (the RAPL counter occasionally returns
#' zero when a run is too short to register); discarding them produces
#' tighter, more reproducible distributions.  This strategy is referred to
#' as the \emph{no0} approach in the OLA-2026 paper.
#'
#' @param data A data frame.
#' @param col Character.  Name of the energy column to inspect.
#'   Default: \code{"PKG"}.
#'
#' @return A data frame with zero-energy rows removed.
#'
#' @examples
#' df <- data.frame(PKG = c(0, 100, 0, 120, 110), seconds = rep(1, 5))
#' filter_zero_energy(df)
#'
#' @export
filter_zero_energy <- function(data, col = "PKG") {
  return(data[data[[col]] != 0, , drop = FALSE])
}

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
#' @examples
#' df <- data.frame(diff_fitness = c(0.001, 0.01, 0.1, 1))
#' add_log_diff(df)$log_diff
#'
#' @export
add_log_diff <- function(data, col = "diff_fitness", new_col = "log_diff") {
  data[[new_col]] <- log10(data[[col]])
  return(data)
}
