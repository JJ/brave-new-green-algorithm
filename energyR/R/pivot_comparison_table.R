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
#' @references
#' Merelo, JJ and Merelo Molina, Cecilia (2026). "Best practices in measuring
#' energy consumption in population-based metaheuristics." In \emph{Proceedings
#' OLA'26 International Conference on Optimization and Learning}, pp. 183--194.
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
