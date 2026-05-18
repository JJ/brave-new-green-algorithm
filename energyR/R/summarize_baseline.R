#' Summarise baseline energy measurements
#'
#' Groups a baseline data frame by \code{dimension} and \code{population_size}
#' and returns descriptive statistics for PKG energy and wall-clock time.
#' The resulting summary is suitable for passing to \code{\link{compute_deltas}}.
#'
#' @param data A data frame of raw baseline measurements containing at least
#'   the columns \code{PKG}, \code{seconds}, \code{dimension}, and
#'   \code{population_size}.
#' @param trim Numeric. Fraction (0 to 0.5) of observations to be trimmed from
#'   each end before computing means.  Default: 0.2 (20 \%).
#'
#' @return A data frame with one row per (dimension, population_size)
#'   combination and the following columns:
#'   \describe{
#'     \item{dimension}{Problem dimension.}
#'     \item{population_size}{Population size.}
#'     \item{median_energy}{Median PKG energy (J).}
#'     \item{sd_energy}{Standard deviation of PKG energy (J).}
#'     \item{trimmed_mean_energy}{Trimmed mean of PKG energy (J).}
#'     \item{median_time}{Median wall-clock time (s).}
#'     \item{sd_time}{Standard deviation of wall-clock time (s).}
#'     \item{trimmed_mean_time}{Trimmed mean of wall-clock time (s).}
#'   }
#'
#' @examples
#' df <- data.frame(
#'   PKG             = c(rnorm(30, 100, 5), rnorm(30, 110, 5)),
#'   seconds         = c(rnorm(30, 1.0, 0.05), rnorm(30, 1.1, 0.05)),
#'   dimension       = rep(c(3, 5), each = 30),
#'   population_size = 200
#' )
#' summarize_baseline(df)
#'
#' @export
summarize_baseline <- function(data, trim = 0.2) {
  configs <- unique(data[, c("dimension", "population_size"), drop = FALSE])
  result  <- do.call(rbind, lapply(seq_len(nrow(configs)), function(i) {
    d   <- configs$dimension[i]
    pop <- configs$population_size[i]
    sub <- data[data$dimension == d & data$population_size == pop, ]
    data.frame(
      dimension            = d,
      population_size      = pop,
      median_energy        = median(sub$PKG),
      sd_energy            = sd(sub$PKG),
      trimmed_mean_energy  = mean(sub$PKG, trim = trim),
      median_time          = median(sub$seconds),
      sd_time              = sd(sub$seconds),
      trimmed_mean_time    = mean(sub$seconds, trim = trim),
      stringsAsFactors     = FALSE
    )
  }))
  return(result)
}
