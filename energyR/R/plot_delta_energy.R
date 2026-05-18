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
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
#'
#' Merelo Guervos, Juan Julian and Merelo-Molina, Cecilia (2025). "Analyzing
#' how the exploration/exploitation trade off in biologically-inspired
#' algorithms affects energy consumption." University of Granada.
#' \url{https://hdl.handle.net/10481/107864}
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
