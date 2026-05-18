#' Extract top ANOVA terms by variance explained
#'
#' Given the \code{\link[stats]{anova}} result of a fitted model, extracts
#' the \emph{n} most influential terms (by F value) that are statistically
#' significant (\code{p < alpha}), and adds a column showing the percentage
#' of variance each term explains among the significant terms.
#'
#' This pattern is used repeatedly in the PPSN-2026 and ICSME-2026 papers to
#' produce compact ANOVA summary tables.
#'
#' @param anova_result An object of class \code{"anova"} as returned by
#'   \code{\link[stats]{anova}}.
#' @param n Integer.  Maximum number of terms to return.  Default: 10.
#' @param alpha Numeric.  Significance threshold for the p-value.
#'   Default: 0.05.
#' @param f_col Character or \code{NULL}.  Name of the F-statistic column in
#'   the anova result.  When \code{NULL} (default), the function auto-detects
#'   the column by looking for \code{"F"} then \code{"F value"}, which handles
#'   both \code{glm} (\code{test = "F"}) and \code{lm} anova tables.
#' @param p_col Character.  Name of the p-value column.
#'   Default: \code{"Pr(>F)"}.
#'
#' @return A data frame with columns \code{term}, \code{percentage_variance},
#'   and any other columns present in \code{anova_result}.  Rows are sorted
#'   by \code{percentage_variance} descending.
#'
#' @examples
#' model <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' top_anova_terms(anova(model))
#'
#' @export
top_anova_terms <- function(anova_result, n = 10, alpha = 0.05,
                             f_col = NULL, p_col = "Pr(>F)") {
  tbl <- as.data.frame(anova_result)
  tbl$term <- rownames(tbl)
  # Auto-detect F column: glm uses "F", lm uses "F value"
  if (is.null(f_col)) {
    f_col <- if ("F" %in% names(tbl)) "F" else if ("F value" %in% names(tbl)) "F value" else
      stop("Cannot detect F-statistic column; supply f_col explicitly.")
  }
  f_vals <- tbl[[f_col]]
  p_vals <- tbl[[p_col]]
  mask   <- !is.na(f_vals) & !is.na(p_vals) & p_vals < alpha
  tbl    <- tbl[mask, , drop = FALSE]
  if (nrow(tbl) == 0) return(tbl)
  tbl    <- tbl[order(-tbl[[f_col]]), , drop = FALSE]
  tbl    <- head(tbl, n)
  tbl$percentage_variance <- tbl[[f_col]] / sum(tbl[[f_col]], na.rm = TRUE) * 100
  rownames(tbl) <- NULL
  tbl[, c("term", "percentage_variance",
          setdiff(names(tbl), c("term", "percentage_variance")))]
}
