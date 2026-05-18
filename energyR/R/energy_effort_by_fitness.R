#' Summarise energy effort by fitness level
#'
#' Groups data by (optionally) a \code{group_col} and by binned
#' log\eqn{_{10}} fitness levels, and reports energy statistics for each
#' bin.  This pattern is used in the PPSN-2026 paper to compare how much
#' energy different algorithm variants require to reach a given fitness level.
#'
#' @param data A data frame with at least \code{diff_fitness} and the energy
#'   column named by \code{energy_col}.
#' @param fitness_levels Integer vector of (negative) log\eqn{_{10}} fitness
#'   levels to include.  Default: \code{-1:-6}.
#' @param group_col Character or \code{NULL}.  Name of the grouping column
#'   (e.g., \code{"group"}).  Default: \code{"group"}.
#' @param energy_col Character.  Name of the energy column.
#'   Default: \code{"PKG"}.
#'
#' @return A data frame with one row per (group, fitness_level) combination
#'   containing \code{median_effort}, \code{mean_effort}, \code{min_effort},
#'   and \code{n_samples}.
#'
#' @references
#' Merelo-Guervos, Juan J. and Merelo-Molina, Cecilia and Garcia-Sanchez,
#' Pablo and Garcia-Valdez, Mario (2026). "Is there a (carbon-) free lunch?
#' Energy/performance tradeoffs in population-based metaheuristics." Accepted,
#' LION 20.
#'
#' @examples
#' df <- data.frame(
#'   diff_fitness = 10^(-runif(100, 1, 6)),
#'   PKG          = rnorm(100, 200, 30),
#'   group        = rep(c("v1", "v2"), each = 50)
#' )
#' energy_effort_by_fitness(df)
#'
#' @export
energy_effort_by_fitness <- function(data,
                                      fitness_levels = -1:-6,
                                      group_col      = "group",
                                      energy_col     = "PKG") {
  data$fitness_level <- round(log10(data$diff_fitness))
  sub <- data[data$fitness_level %in% fitness_levels, ]
  group_cols <- c(if (!is.null(group_col) && group_col %in% names(sub))
    group_col, "fitness_level")
  configs <- unique(sub[, group_cols, drop = FALSE])
  result  <- do.call(rbind, lapply(seq_len(nrow(configs)), function(i) {
    mask <- rep(TRUE, nrow(sub))
    for (gc in group_cols) mask <- mask & sub[[gc]] == configs[[gc]][i]
    rows  <- sub[mask, , drop = FALSE]
    vals  <- rows[[energy_col]]
    cbind(
      configs[i, , drop = FALSE],
      data.frame(
        median_effort = median(vals, na.rm = TRUE),
        mean_effort   = mean(vals,   na.rm = TRUE),
        min_effort    = min(vals,    na.rm = TRUE),
        n_samples     = sum(!is.na(vals)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    )
  }))
  result <- result[order(result$fitness_level, decreasing = TRUE), ]
  rownames(result) <- NULL
  return(result)
}
