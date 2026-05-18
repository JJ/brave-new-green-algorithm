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
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
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
