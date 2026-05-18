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
#' @examples
#' format_mean_sd(c(100.1, 200.5), c(5.3, 9.8))
#'
#' @export
format_mean_sd <- function(mean_vals, sd_vals, digits = 2) {
  paste0(round(mean_vals, digits), " (", round(sd_vals, digits), ")")
}

#' Build a wide comparison table from a summary data frame
#'
#' Pivots a long-format summary data frame to a wide format suitable for
#' publication tables, placing experimental groups (\code{group_col}) across
#' columns.  Optionally merges in Wilcoxon p-values.
#'
#' This is the \code{reshape2::dcast} + \code{merge} pattern used in the
#' OLA-2026 and PPSN-2026 papers.
#'
#' @param summary_df A data frame in long format.  Must contain
#'   \code{id_cols}, \code{group_col}, and \code{value_col}.
#' @param value_col Character.  Name of the formatted-value column to spread
#'   across columns (e.g., the output of \code{\link{format_mean_sd_latex}}).
#' @param id_cols Character vector.  Names of the row-identifier columns
#'   (e.g., \code{c("population_size", "dimension")}).
#' @param group_col Character.  Name of the column whose unique values become
#'   the new column names.  Default: \code{"work"}.
#' @param wilcoxon_df Optional data frame of Wilcoxon test results (as
#'   returned by \code{\link{wilcoxon_tests}}).  When provided, the
#'   \code{p_value} column is appended and formatted as \code{"+"} (p < 0.05)
#'   or \code{"-"}.
#'
#' @return A wide data frame with one column per unique value in
#'   \code{group_col}, optionally with a \code{significant} column.
#'
#' @examples
#' summary_df <- data.frame(
#'   population_size = rep(c(200, 400), each = 2),
#'   dimension       = rep(c(3, 5), 2),
#'   work            = rep(c("v1", "v2"), each = 2),
#'   pkg_label       = paste0(round(rnorm(8, 100, 5), 1), " (3.0)")
#' )
#' pivot_comparison_table(summary_df, "pkg_label",
#'                        c("population_size", "dimension"))
#'
#' @export
pivot_comparison_table <- function(summary_df, value_col, id_cols,
                                    group_col    = "work",
                                    wilcoxon_df  = NULL) {
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package 'reshape2' is required for pivot_comparison_table(). ",
         "Install it with: install.packages('reshape2')")
  }
  formula_str <- paste(
    paste(id_cols, collapse = " + "), "~", group_col
  )
  wide <- reshape2::dcast(
    summary_df,
    stats::as.formula(formula_str),
    value.var = value_col
  )
  if (!is.null(wilcoxon_df) && "p_value" %in% names(wilcoxon_df)) {
    merge_cols <- intersect(id_cols, names(wilcoxon_df))
    if (length(merge_cols) > 0) {
      wide <- merge(wide,
                    wilcoxon_df[, c(merge_cols, "p_value"), drop = FALSE],
                    by = merge_cols, all.x = TRUE)
      wide$significant <- ifelse(
        !is.na(wide$p_value) & as.numeric(wide$p_value) < 0.05, "+", "-"
      )
      wide$p_value <- NULL
    }
  }
  return(wide)
}

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
