test_that("format_mean_sd produces correct plain-text format", {
  result <- format_mean_sd(c(100.123, 200.456), c(5.678, 9.012))
  expect_equal(result[1], "100.12 (5.68)")
  expect_equal(result[2], "200.46 (9.01)")
})

test_that("format_mean_sd respects digits parameter", {
  result <- format_mean_sd(100.123, 5.678, digits = 1)
  expect_equal(result, "100.1 (5.7)")
})

test_that("energy_effort_by_fitness returns correct structure", {
  set.seed(42)
  df <- data.frame(
    diff_fitness = 10^(-runif(100, 1, 5)),
    PKG          = rnorm(100, 200, 30),
    group        = rep(c("v1", "v2"), each = 50)
  )
  result <- energy_effort_by_fitness(df)
  expect_true("fitness_level" %in% names(result))
  expect_true("median_effort" %in% names(result))
  expect_true("n_samples"     %in% names(result))
  # All fitness_levels should be within requested range
  expect_true(all(result$fitness_level %in% -1:-6))
})

test_that("energy_effort_by_fitness works with custom fitness levels", {
  df <- data.frame(
    diff_fitness = 10^(-runif(60, 1, 4)),
    PKG          = rnorm(60, 150, 20),
    group        = "only_one"
  )
  result <- energy_effort_by_fitness(df, fitness_levels = -1:-3)
  expect_true(all(result$fitness_level %in% -1:-3))
})

test_that("pivot_comparison_table produces wide format", {
  skip_if_not_installed("reshape2")
  summary_df <- data.frame(
    population_size = rep(c(200, 400), each = 2),
    dimension       = rep(c(3, 5), 2),
    work            = c("v1", "v1", "v2", "v2"),
    pkg_label       = c("100 (5)", "110 (6)", "105 (4)", "115 (7)")
  )
  result <- pivot_comparison_table(
    summary_df, "pkg_label", c("population_size", "dimension")
  )
  expect_true("v1" %in% names(result))
  expect_true("v2" %in% names(result))
  expect_equal(nrow(result), 2)
})
