#' Add a population-size × dimension label column
#'
#' Adds a new column \code{population_dimension} (or the name specified by
#' \code{label_col}) to \code{data} containing a human-readable string of
#' the form \code{"Pop. size: 200, Dim.: 3"}.  This label is useful as an
#' x-axis tick or facet title in plots.
#'
#' @param data A data frame with at least the columns named by \code{pop_col}
#'   and \code{dim_col}.
#' @param pop_col Character.  Name of the population-size column.
#'   Default: \code{"population_size"}.
#' @param dim_col Character.  Name of the dimension column.
#'   Default: \code{"dimension"}.
#' @param label_col Character.  Name of the new label column to be added.
#'   Default: \code{"population_dimension"}.
#'
#' @return The input data frame with an additional character column named
#'   \code{label_col}.
#'
#' @examples
#' df <- data.frame(population_size = c(200, 400), dimension = c(3, 5))
#' add_pop_dim_label(df)
#'
#' @export
add_pop_dim_label <- function(data,
                               pop_col   = "population_size",
                               dim_col   = "dimension",
                               label_col = "population_dimension") {
  data[[label_col]] <- paste0(
    "Pop. size: ", data[[pop_col]],
    ", Dim.: ",    data[[dim_col]]
  )
  return(data)
}

#' Format mean ± SD as a LaTeX math string
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
