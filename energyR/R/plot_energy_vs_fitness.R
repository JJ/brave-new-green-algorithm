#' Scatter plot of energy vs fitness difference
#'
#' Produces a \pkg{ggplot2} scatter plot of energy (x-axis) against the
#' logarithm of the fitness difference (y-axis), with optional colouring and
#' shaping by algorithm parameters.  This is one of the most common plots
#' across all BNA papers (walcom, evoapps, lion).
#'
#' If a \code{log_diff} column is not already present in \code{data}, it is
#' computed on the fly as \code{log10(diff_fitness)}.
#'
#' @param data A data frame with at least an energy column (default
#'   \code{"delta_PKG"} or \code{"PKG"}) and either a \code{log_diff} or a
#'   \code{diff_fitness} column.
#' @param x_col Character.  Name of the energy column for the x-axis.
#'   Default: \code{"delta_PKG"}.
#' @param y_col Character.  Name of the (log) fitness column for the y-axis.
#'   If absent, \code{log10(diff_fitness)} is used.  Default: \code{"log_diff"}.
#' @param color_col Character or \code{NULL}.  Column to map to colour.
#'   Default: \code{"work"}.
#' @param alpha Numeric.  Point transparency.  Default: 0.5.
#' @param title Character.  Plot title.
#'
#' @return The \code{\link[ggplot2]{ggplot}} object (printed as a side
#'   effect).
#'
#' @references
#' Merelo-Guervos, Juan J. and Romero-Lopez, Gustavo and Garcia-Valdez, Mario
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
#' df <- data.frame(
#'   delta_PKG    = rnorm(60, 50, 10),
#'   diff_fitness = 10^(-runif(60, 1, 4)),
#'   work         = rep(c("v1", "v2"), each = 30)
#' )
#' plot_energy_vs_fitness(df)
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal scale_x_log10
#' @export
plot_energy_vs_fitness <- function(data, x_col = "delta_PKG",
                                    y_col = "log_diff",
                                    color_col = "work",
                                    alpha = 0.5,
                                    title = "Energy vs Fitness Difference") {
  if (!y_col %in% names(data)) {
    data[[y_col]] <- log10(data$diff_fitness)
  }
  x_vals     <- data[[x_col]]
  y_vals     <- data[[y_col]]
  color_vals <- if (!is.null(color_col) && color_col %in% names(data))
    data[[color_col]] else NULL
  p <- ggplot(data, aes(x = x_vals, y = y_vals)) +
    labs(title = title,
         x     = paste0(x_col, " (J)"),
         y     = "log10(diff fitness)",
         color = color_col) +
    theme_minimal()
  if (!is.null(color_vals)) {
    p <- p + geom_point(aes(color = color_vals), alpha = alpha)
  } else {
    p <- p + geom_point(alpha = alpha)
  }
  print(p)
  return(invisible(p))
}
