test_that("add_cumulative_time adds cum_seconds column", {
  df <- data.frame(PKG = rnorm(5, 100, 5), seconds = c(1, 2, 1.5, 2, 1))
  result <- add_cumulative_time(df)
  expect_true("cum_seconds" %in% names(result))
  expect_equal(result$cum_seconds, cumsum(df$seconds))
})

test_that("add_cumulative_time respects custom col name", {
  df <- data.frame(seconds = rep(1, 3))
  result <- add_cumulative_time(df, col = "t")
  expect_true("t" %in% names(result))
})

test_that("filter_zero_energy removes zero-PKG rows", {
  df <- data.frame(PKG = c(0, 100, 0, 120), seconds = rep(1, 4))
  result <- filter_zero_energy(df)
  expect_equal(nrow(result), 2)
  expect_true(all(result$PKG > 0))
})

test_that("filter_zero_energy works with custom column", {
  df <- data.frame(energy = c(0, 50, 100), PKG = c(100, 0, 100))
  result <- filter_zero_energy(df, col = "energy")
  expect_equal(nrow(result), 2)
})

test_that("add_log_diff adds log_diff column", {
  df <- data.frame(diff_fitness = c(0.001, 0.01, 0.1, 1))
  result <- add_log_diff(df)
  expect_true("log_diff" %in% names(result))
  expect_equal(result$log_diff, log10(df$diff_fitness))
})

test_that("add_log_diff respects custom column names", {
  df <- data.frame(fitness = c(0.01, 0.1))
  result <- add_log_diff(df, col = "fitness", new_col = "lf")
  expect_true("lf" %in% names(result))
})

test_that("prepare_workload runs the default preprocessing pipeline", {
  baseline_summary <- data.frame(
    dimension = c(3, 5),
    population_size = c(200, 200),
    median_energy = c(100, 110),
    median_time = c(1.0, 1.1)
  )

  workload <- data.frame(
    dimension = c(3, 5),
    population_size = c(200, 200),
    PKG = c(115, 130),
    seconds = c(1.15, 1.25),
    diff_fitness = c(0.01, 0.1)
  )

  result <- prepare_workload(baseline_summary, workload)

  expect_true(all(c("delta_PKG", "delta_seconds", "log_diff", "pop_dim") %in% names(result)))
  expect_equal(result$delta_PKG, c(15, 20))
})

test_that("prepare_workload skips log and labels when requested", {
  baseline_summary <- data.frame(
    dimension = 3,
    population_size = 200,
    median_energy = 100,
    median_time = 1.0
  )

  workload <- data.frame(
    dimension = 3,
    population_size = 200,
    PKG = 115,
    seconds = 1.15
  )

  result <- prepare_workload(
    baseline_summary,
    workload,
    add_label = FALSE
  )

  expect_true(all(c("delta_PKG", "delta_seconds") %in% names(result)))
  expect_false("log_diff" %in% names(result))
  expect_false("pop_dim" %in% names(result))
})
