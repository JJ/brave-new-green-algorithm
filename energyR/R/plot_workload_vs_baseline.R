#' Plot workload runs against their per-combination baseline
#'
#' Produces the \emph{view1} visualisation introduced in the OLA-2026
#' extended document: a faceted scatter plot showing every workload PKG
#' measurement against its run index within each parameter combination, with
#' \itemize{
#'   \item a dashed orange horizontal line at the per-combination trimmed-mean
#'     baseline;
#'   \item a shaded +/-1 SD band around the baseline;
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
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
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
      labels = c("TRUE"  = "Above baseline (delta PKG > 0)",
                 "FALSE" = "Below baseline (delta PKG < 0)"),
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
