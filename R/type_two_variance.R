# Order-independent variance decomposition for a gaussian glm/lm energy model.
#
# Replaces the old "F / sum(F) over the top 10 significant terms" figure.
# That figure was the share of each term's *mean square* among the selected
# terms only (all F values share the same residual mean square, so it cancels):
# it always summed to 100% whatever the model R^2, ignored the residual and the
# non-selected terms, and it came from sequential (Type I) ANOVA, so terms
# entered first (e.g. initial_temp) absorbed any variance they shared with
# terms entered later. It is a ranking of terms under one particular ordering,
# not a fraction of the variance of the response.
#
# This helper computes, for every term, from Type II sums of squares
# (car::Anova), which do not depend on the order of the terms in the formula:
#   variance_share_percentage = 100 * SS_term / SS_total
#       Percentage of the TOTAL variance of the response attributable to the
#       term after adjusting for all other terms that do not contain it. For
#       terms not contained in a higher-order interaction this is exactly the
#       drop in R^2 when the term is removed (squared semi-partial
#       correlation), i.e. variance the term explains uniquely. Shares do not
#       add up to R^2 because variance shared among correlated terms is not
#       credited to any of them.
#   partial_eta_squared = SS_term / (SS_term + SS_residual)
#       Fraction of the variance left unexplained by the other terms that this
#       term explains. Standard effect size; does not add up to anything.
# The model R^2 is attached as attribute "r_squared" so captions can report it.
#
# Rows are sorted by sum of squares, descending; filtering by p-value and
# keeping the top n is left to the caller.
type_two_variance <- function(model) {
  # A gaussian glm is the same model as lm; refitting as lm makes car::Anova
  # return sums of squares (for glm objects it returns likelihood-ratio tests).
  linear_model <- lm(formula(model), data = model.frame(model))
  type_two_table <- car::Anova(linear_model, type = 2)

  response <- model.response(model.frame(linear_model))
  total_sum_squares <- sum((response - mean(response))^2)
  residual_sum_squares <- type_two_table["Residuals", "Sum Sq"]

  term_rows <- rownames(type_two_table) != "Residuals"
  variance_table <- data.frame(
    term = rownames(type_two_table)[term_rows],
    sum_squares = type_two_table[term_rows, "Sum Sq"],
    p_value = type_two_table[term_rows, "Pr(>F)"],
    stringsAsFactors = FALSE
  )
  variance_table$variance_share_percentage <-
    100 * variance_table$sum_squares / total_sum_squares
  variance_table$partial_eta_squared <-
    variance_table$sum_squares / (variance_table$sum_squares + residual_sum_squares)

  variance_table <- variance_table[order(-variance_table$sum_squares), ]
  rownames(variance_table) <- NULL
  attr(variance_table, "r_squared") <- summary(linear_model)$r.squared
  variance_table
}

# Table for the paper: the top n significant terms plus the rows needed for
# the percentage column to add up to 100%, so the reader can see where the rest
# of the variance goes:
#   "Other terms (on their own)" = Type II shares of all terms not listed
#   "Shared among terms"         = 100 * R^2 minus the sum of all Type II
#       shares: variance that the model explains but that cannot be credited to
#       any single term because correlated terms (e.g. both baselines and
#       running time) explain it jointly. With strongly correlated covariates
#       this can be the largest row. It can be slightly negative when terms
#       suppress each other.
#   "Not explained (residual)"   = 100 * (1 - R^2)
# Partial eta squared is left empty (NA) for these rows: it has no meaning for
# them.
variance_table_with_totals <- function(variance_table, n = 10, alpha = 0.05) {
  r_squared_percentage <- 100 * attr(variance_table, "r_squared")
  significant_terms <- variance_table[variance_table$p_value < alpha, ]
  top_terms <- head(significant_terms, n)
  listed_share <- sum(top_terms$variance_share_percentage)
  all_terms_share <- sum(variance_table$variance_share_percentage)

  summary_rows <- data.frame(
    term = c("Other terms (on their own)", "Shared among terms",
             "Not explained (residual)", "Total"),
    variance_share_percentage = c(all_terms_share - listed_share,
                                  r_squared_percentage - all_terms_share,
                                  100 - r_squared_percentage,
                                  100),
    partial_eta_squared = NA_real_,
    stringsAsFactors = FALSE
  )
  rbind(top_terms[, c("term", "variance_share_percentage", "partial_eta_squared")],
        summary_rows)
}

# Value of one column for one term, NA when the term is not in the table, so
# that \Sexpr{} in the paper does not silently print nothing.
variance_of_term <- function(variance_table, term_name,
                             column = "variance_share_percentage") {
  value <- variance_table[variance_table$term == term_name, column]
  if (length(value) == 0) NA_real_ else value
}
