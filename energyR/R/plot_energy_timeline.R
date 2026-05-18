#' Plot energy consumption over cumulative time
#'
#' Produces a line or scatter plot of PKG energy against cumulative elapsed
#' time, optionally coloured and grouped by the \code{work} column.
#'
#' @param data A data frame containing at least \code{PKG} and \code{seconds}.
#'   If a \code{cum_seconds} column is absent it will be computed as
#'   \code{cumsum(seconds)}.
#' @param color_by Character.  Name of the column to use for colour mapping.
#'   Default: \code{"work"}.
#' @param geom Character.  Either \code{"point"} (default) or \code{"line"}.
#' @param title Character.  Plot title.
#'
#' @return The \code{\link[ggplot2]{ggplot}} object (printed as a side effect).
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' Merelo-Guervos, Juan J. and Romero-López, Gustavo and Garcia-Valdez, Mario
#' (2025). "Time-related effects in the measurement of energy consumption in
#' evolutionary algorithms." In \emph{Europar 2025: Parallel Processing
#' Workshops}.
#'
#' @examples
#' df <- data.frame(
#'   PKG     = cumsum(abs(rnorm(100, 100, 5))),
#'   seconds = rep(1, 100),
#'   work    = rep(c("baseline", "workload"), each = 50)
#' )
#' plot_energy_timeline(df)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal
#' @export
plot_energy_timeline <- function(data, color_by = "work",
                                  geom = c("point", "line"),
                                  title = "Energy Consumption Over Time") {
  geom <- match.arg(geom)
  if (!"cum_seconds" %in% names(data)) {
    data$cum_seconds <- cumsum(data$seconds)
  }
  color_vals <- data[[color_by]]
  p <- ggplot(data, aes(x = cum_seconds, y = PKG,
                        color = color_vals,
                        group = color_vals)) +
    labs(title = title, x = "Cumulative Time (s)", y = "PKG Energy (J)",
         color = color_by) +
    theme_minimal()
  if (geom == "point") {
    p <- p + geom_point()
  } else {
    p <- p + geom_line()
  }
  print(p)
  return(invisible(p))
}
