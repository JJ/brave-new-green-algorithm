#' Run Wilcoxon rank-sum tests across parameter combinations
#'
#' For each unique combination of \code{dimension} and \code{population_size}
#' in \code{data}, performs a two-sample Wilcoxon rank-sum test between the
#' two \code{work} groups present.  Only combinations that have \emph{exactly}
#' two distinct \code{work} values are tested; others are silently skipped.
#'
#' @param data A data frame containing at least the columns \code{dimension},
#'   \code{population_size}, and \code{work}, plus the column named by
#'   \code{column_name}.
#' @param column_name Character.  Name of the numeric column to test.
#'   Default: \code{"PKG"}.
#'
#' @return A data frame with one row per tested (dimension, population_size)
#'   combination and the following columns:
#'   \describe{
#'     \item{population_size}{Population size.}
#'     \item{dimension}{Problem dimension.}
#'     \item{work1}{Label of the first work group.}
#'     \item{work2}{Label of the second work group.}
#'     \item{p_value}{P-value from \code{\link[stats]{wilcox.test}}.}
#'   }
#'
#' @examples
#' df <- data.frame(
#'   PKG             = c(rnorm(20, 100, 5), rnorm(20, 115, 5)),
#'   dimension       = 3,
#'   population_size = 200,
#'   work            = rep(c("baseline", "workload"), each = 20)
#' )
#' wilcoxon_tests(df)
#'
#' @importFrom stats wilcox.test
#' @export
wilcoxon_tests <- function(data, column_name = "PKG") {
  results <- data.frame(
    population_size = integer(0),
    dimension       = integer(0),
    work1           = character(0),
    work2           = character(0),
    p_value         = numeric(0),
    stringsAsFactors = FALSE
  )
  for (pop_size in unique(data$population_size)) {
    for (dim in unique(data$dimension)) {
      subset_data <- data[data$population_size == pop_size &
                            data$dimension == dim, ]
      work_values <- unique(subset_data$work)
      if (length(work_values) != 2) next
      g1 <- subset_data[subset_data$work == work_values[1], column_name, drop = TRUE]
      g2 <- subset_data[subset_data$work == work_values[2], column_name, drop = TRUE]
      test_result <- wilcox.test(g1, g2)
      results <- rbind(results, data.frame(
        population_size  = pop_size,
        dimension        = dim,
        work1            = work_values[1],
        work2            = work_values[2],
        p_value          = test_result$p.value,
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results)
}
