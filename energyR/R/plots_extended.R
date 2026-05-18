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
         y     = "log\u2081\u2080(diff fitness)",
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

#' Plot workload runs against their per-combination baseline
#'
#' Produces the \emph{view1} visualisation introduced in the OLA-2026
#' extended document: a faceted scatter plot showing every workload PKG
#' measurement against its run index within each parameter combination, with
#' \itemize{
#'   \item a dashed orange horizontal line at the per-combination trimmed-mean
#'     baseline;
#'   \item a shaded ±1 SD band around the baseline;
#'   \item colour-coded vertical segments from each point to the baseline line
#'     (green above, pink below).
#' }
#'
#' @param workload A data frame of workload measurements.  Must include
#'   columns \code{PKG}, \code{trimmed_mean_PKG}, \code{sd_PKG}, and a facet
#'   column (default \code{"param_combo"}).  A \code{run_index} column will be
#'   added if absent.
#' @param facet_col Character.  Name of the faceting column.
#'   Default: \code{"param_combo"}.
#' @param title Character.  Plot title.
#'
#' @return The \code{\link[ggplot2]{ggplot}} object (printed as a side
#'   effect).
#'
#' @examples
#' \dontrun{
#' workload <- compute_deltas(baseline_summary, workload_raw)
#' workload$param_combo <- paste0("Dim ", workload$dimension,
#'                                 " | Pop ", workload$population_size)
#' workload$trimmed_mean_PKG <- ...  # join from baseline_summary
#' plot_workload_vs_baseline(workload)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_rect geom_segment geom_point geom_hline
#'   labs theme_minimal facet_wrap scale_colour_manual
#' @export
plot_workload_vs_baseline <- function(workload,
                                       facet_col = "param_combo",
                                       title     = "Workload runs vs. per-combination baseline") {
  if (!"run_index" %in% names(workload)) {
    workload$run_index <- stats::ave(
      seq_len(nrow(workload)),
      workload[[facet_col]],
      FUN = seq_along
    )
  }
  pal_sign <- c("TRUE" = "#3D9970", "FALSE" = "#E07A5F")
  p <- ggplot(workload, aes(x = as.numeric(run_index), y = PKG)) +
    geom_rect(
      aes(xmin = -Inf, xmax = Inf,
          ymin = trimmed_mean_PKG - sd_PKG,
          ymax = trimmed_mean_PKG + sd_PKG),
      fill = "#F28E2B", alpha = 0.10, inherit.aes = FALSE
    ) +
    geom_segment(
      aes(xend  = as.numeric(run_index),
          yend  = trimmed_mean_PKG,
          colour = (PKG - trimmed_mean_PKG) >= 0),
      alpha = 0.40, linewidth = 0.6
    ) +
    geom_point(colour = "grey20", size = 2.2, alpha = 0.80) +
    geom_hline(
      aes(yintercept = trimmed_mean_PKG),
      linetype = "dashed", colour = "#F28E2B", linewidth = 1.0
    ) +
    scale_colour_manual(
      values = pal_sign,
      labels = c("TRUE"  = "Above baseline (\u0394 PKG > 0)",
                 "FALSE" = "Below baseline (\u0394 PKG < 0)"),
      name = NULL
    ) +
    facet_wrap(stats::as.formula(paste("~", facet_col)), ncol = 2) +
    theme_minimal() +
    labs(title    = title,
         x        = "Run index within parameter combination",
         y        = "PKG energy (J)")
  print(p)
  return(invisible(p))
}

#' Ridgeline density plot of Δ PKG energy
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
                                  title     = "Ridgeline density of \u0394 PKG") {
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
         subtitle = "Quantile lines at Q1, median, Q3 \u00b7 Red dashed line at zero",
         x        = "\u0394 PKG energy (J)",
         y        = NULL)
  print(p)
  return(invisible(p))
}
