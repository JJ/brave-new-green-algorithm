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

#' Plot delta PKG energy distributions
#'
#' Produces a violin or box plot of \code{delta_PKG} values, faceted by
#' \code{population_dimension} label if present, and coloured by the
#' \code{work} column.  The \code{population_dimension} column can be created
#' with \code{\link{add_pop_dim_label}}.
#'
#' @param data A data frame with at least the columns \code{delta_PKG} and
#'   \code{work}.
#' @param geom Character.  Either \code{"violin"} (default) or \code{"boxplot"}.
#' @param facet_col Character or \code{NULL}.  Name of the column to facet by.
#'   Default: \code{"population_dimension"} (ignored if not present).
#' @param title Character.  Plot title.
#'
#' @return The \code{\link[ggplot2]{ggplot}} object (printed as a side effect).
#'
#' @examples
#' df <- data.frame(
#'   delta_PKG = c(rnorm(30, 10, 3), rnorm(30, 15, 3)),
#'   work      = rep(c("v1", "v2"), each = 30)
#' )
#' plot_delta_energy(df)
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot labs theme_minimal
#'   facet_wrap
#' @export
plot_delta_energy <- function(data,
                               geom      = c("violin", "boxplot"),
                               facet_col = "population_dimension",
                               title     = "Delta PKG Energy") {
  geom <- match.arg(geom)
  p <- ggplot(data, aes(x = work, y = delta_PKG,
                        color = as.factor(work))) +
    labs(title = title, x = "", y = "Delta PKG Energy (J)",
         color = "Configuration") +
    theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  if (geom == "violin") {
    p <- p + geom_violin()
  } else {
    p <- p + geom_boxplot(notch = TRUE)
  }
  if (!is.null(facet_col) && facet_col %in% names(data)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", facet_col)))
  }
  print(p)
  return(invisible(p))
}
