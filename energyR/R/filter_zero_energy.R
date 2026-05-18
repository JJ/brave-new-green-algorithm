#' Filter out zero-energy rows
#'
#' Removes rows where the energy column equals zero.  Zero-energy readings
#' are a known measurement artefact (the RAPL counter occasionally returns
#' zero when a run is too short to register); discarding them produces
#' tighter, more reproducible distributions.  This strategy is referred to
#' as the \emph{no0} approach in the OLA-2026 paper.
#'
#' @param data A data frame.
#' @param col Character.  Name of the energy column to inspect.
#'   Default: \code{"PKG"}.
#'
#' @return A data frame with zero-energy rows removed.
#'
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' @examples
#' df <- data.frame(PKG = c(0, 100, 0, 120, 110), seconds = rep(1, 5))
#' filter_zero_energy(df)
#'
#' @export
filter_zero_energy <- function(data, col = "PKG") {
  return(data[data[[col]] != 0, , drop = FALSE])
}
