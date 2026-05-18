#' Format mean +/- SD as a LaTeX math string
#'
#' Produces a character vector of the form \code{"$mean \\\\pm sd$"} suitable
#' for direct inclusion in LaTeX tables (e.g., via \pkg{kableExtra}).  Each
#' element looks like \code{$100.12 \\pm5.68$} in the R string, which renders
#' as \eqn{100.12 \pm 5.68} in LaTeX.
#'
#' For plain-text tables, use \code{\link{format_mean_sd}} instead.
#'
#' @param mean_vals Numeric vector of mean values.
#' @param sd_vals   Numeric vector of standard deviation values.
#' @param digits    Integer.  Number of decimal places to use for rounding.
#'   Default: 2.
#'
#' @return A character vector of the same length as \code{mean_vals}, where
#'   each element is a LaTeX math-mode string.
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' @examples
#' format_mean_sd_latex(c(100.123, 200.456), c(5.678, 9.012))
#' # [1] "$100.12 \\pm5.68$"  "$200.46 \\pm9.01$"
#'
#' @export
format_mean_sd_latex <- function(mean_vals, sd_vals, digits = 2) {
  paste0(
    "$", round(mean_vals, digits),
    " \\pm", round(sd_vals, digits),
    "$"
  )
}
