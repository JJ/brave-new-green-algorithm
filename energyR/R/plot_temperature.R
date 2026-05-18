#' Plot temperature sensors over cumulative time
#'
#' Produces a scatter plot of two temperature sensor readings against
#' cumulative elapsed time using \pkg{ggplot2}.  Sensor 1 is shown in red
#' and sensor 2 in pink.
#'
#' @param df A data frame with at least the columns \code{cum_seconds},
#'   \code{initial_temp_1}, and \code{initial_temp_2}.
#'
#' @return The \code{\link[ggplot2]{ggplot}} object (printed as a side effect).
#'
#' @references
#' Merelo-Guervos, Juan J. and Romero-Lopez, Gustavo and Garcia-Valdez, Mario
#' (2025). "Time-related effects in the measurement of energy consumption in
#' evolutionary algorithms." In \emph{Europar 2025: Parallel Processing
#' Workshops}.
#'
#' @examples
#' \dontrun{
#' df$cum_seconds <- cumsum(df$seconds)
#' plot_temperature(df)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal
#' @export
plot_temperature <- function(df) {
  p <- ggplot(df, aes(x = cum_seconds)) +
    geom_point(color = "red",  aes(y = initial_temp_1)) +
    geom_point(color = "pink", aes(y = initial_temp_2)) +
    labs(
      title = "Temperature over time",
      x     = "Time (s)",
      y     = "Temperature"
    ) +
    theme_minimal()
  print(p)
  return(invisible(p))
}
