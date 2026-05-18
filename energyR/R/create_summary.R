#' Summarise workload delta-energy measurements
#'
#' Groups a processed workload data frame (i.e., one that already contains
#' \code{delta_PKG} and \code{delta_seconds} columns produced by
#' \code{\link{process_deltas}} or \code{\link{compute_deltas}}) by
#' \code{dimension}, \code{population_size}, and \code{max_gens}, and computes
#' descriptive statistics of the energy and time deltas.
#'
#' An \code{energy_per_evaluation} column is added prior to summarisation when
#' an \code{evaluations} column is present in \code{data}.
#'
#' @param data A data frame of processed workload measurements.  Must contain
#'   \code{delta_PKG}, \code{delta_seconds}, \code{dimension},
#'   \code{population_size}, and \code{max_gens}.  An optional \code{evaluations}
#'   column enables the per-evaluation energy statistic.
#'
#' @return A grouped summary data frame with one row per (dimension,
#'   population_size, max_gens) combination.
#'
#' @examples
#' df <- data.frame(
#'   delta_PKG       = rnorm(120, 10, 3),
#'   delta_seconds   = rnorm(120, 0.1, 0.02),
#'   PKG             = rnorm(120, 110, 5),
#'   dimension       = rep(c(3, 5), each = 60),
#'   population_size = rep(c(200, 400), times = 60),
#'   max_gens        = 10,
#'   evaluations     = sample(500:5000, 120)
#' )
#' create_summary(df)
#'
#' @export
create_summary <- function(data) {
  if ("evaluations" %in% names(data)) {
    data$energy_per_evaluation <- data$delta_PKG / data$evaluations
  } else {
    data$energy_per_evaluation <- NA_real_
  }
  configs <- unique(data[, c("dimension", "population_size", "max_gens"), drop = FALSE])
  result  <- do.call(rbind, lapply(seq_len(nrow(configs)), function(i) {
    d   <- configs$dimension[i]
    pop <- configs$population_size[i]
    mg  <- configs$max_gens[i]
    sub <- data[data$dimension == d &
                  data$population_size == pop &
                  data$max_gens == mg, ]
    data.frame(
      dimension                          = d,
      population_size                    = pop,
      max_gens                           = mg,
      mean_delta_PKG                     = mean(sub$delta_PKG),
      median_delta_PKG                   = stats::median(sub$delta_PKG),
      trimmed_mean_delta_PKG             = mean(sub$delta_PKG, trim = 0.2),
      sd_delta_PKG                       = stats::sd(sub$delta_PKG),
      iqr_delta_PKG                      = stats::IQR(sub$delta_PKG),
      trimmed_mean_energy_per_evaluation = mean(sub$energy_per_evaluation,
                                                trim = 0.2, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  return(result)
}
