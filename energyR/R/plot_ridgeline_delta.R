#' Ridgeline density plot of delta PKG energy
#'
#' Produces a ridgeline density plot of \code{delta_PKG} values, one ridge
#' per unique value of \code{group_col}, with quantile lines and a vertical
#' reference line at zero.  Requires the \pkg{ggridges} package.
#'
#' @param data A data frame with at least a \code{delta_PKG} column and a
#'   grouping column.
#' @param group_col Character.  Name of the column whose values define the
#'   ridges.  Default: \code{"param_combo"}.
#' @param title Character.  Plot title.
#'
#' @return The \code{\link[ggplot2]{ggplot}} object (printed as a side
#'   effect).  Returns \code{NULL} (invisibly) if \pkg{ggridges} is not
#'   installed.
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' @examples
#' \dontrun{
#' workload <- add_pop_dim_label(workload)
#' plot_ridgeline_delta(workload, group_col = "population_dimension")
#' }
#'
#' @importFrom ggplot2 ggplot aes labs theme geom_vline
#' @export
plot_ridgeline_delta <- function(data,
                                  group_col = "param_combo",
                                  title     = "Ridgeline density of delta PKG") {
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    message("Package 'ggridges' is required for plot_ridgeline_delta(). ",
            "Install it with: install.packages('ggridges')")
    return(invisible(NULL))
  }
  p <- ggplot(data, aes(x    = delta_PKG,
                        y    = .data[[group_col]],
                        fill = .data[[group_col]])) +
    ggridges::geom_density_ridges(
      alpha          = 0.55,
      scale          = 0.85,
      quantile_lines = TRUE,
      quantiles      = c(0.25, 0.5, 0.75),
      linewidth      = 0.4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "red", linewidth = 0.8) +
    ggridges::theme_ridges() +
    theme(legend.position = "none") +
    labs(title    = title,
         subtitle = "Quantile lines at Q1, median, Q3 | Red dashed line at zero",
         x        = "delta PKG energy (J)",
         y        = NULL)
  print(p)
  return(invisible(p))
}
