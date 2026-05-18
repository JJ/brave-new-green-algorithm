#' Format mean ± SD as "mean (sd)"
#'
#' Produces a compact \code{"mean (sd)"} string suitable for plain-text
#' tables or \code{kable} columns.  For LaTeX output, see
#' \code{\link{format_mean_sd_latex}}.
#'
#' @param mean_vals Numeric vector of mean values.
#' @param sd_vals   Numeric vector of standard deviation values.
#' @param digits    Integer.  Number of decimal places.  Default: 2.
#'
#' @return A character vector of the same length as \code{mean_vals}.
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' @examples
#' format_mean_sd(c(100.1, 200.5), c(5.3, 9.8))
#'
#' @export
format_mean_sd <- function(mean_vals, sd_vals, digits = 2) {
  paste0(round(mean_vals, digits), " (", round(sd_vals, digits), ")")
}
