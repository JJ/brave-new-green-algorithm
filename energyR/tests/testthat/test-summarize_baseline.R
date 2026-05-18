test_that("summarize_baseline returns one row per dimension x population combo", {
  df <- data.frame(
    PKG             = c(rnorm(30, 100, 5), rnorm(30, 110, 5)),
    seconds         = c(rnorm(30, 1.0, 0.05), rnorm(30, 1.1, 0.05)),
    dimension       = rep(c(3, 5), each = 30),
    population_size = 200
  )
  result <- summarize_baseline(df)
  expect_equal(nrow(result), 2)
  expect_true("median_energy" %in% names(result))
  expect_true("trimmed_mean_energy" %in% names(result))
  expect_true("median_time" %in% names(result))
})

test_that("summarize_baseline respects trim parameter", {
  df <- data.frame(
    PKG             = c(rep(100, 28), 1000, 1000),   # two extreme outliers
    seconds         = rep(1, 30),
    dimension       = 3,
    population_size = 200
  )
  result_trim    <- summarize_baseline(df, trim = 0.2)
  result_notrim  <- summarize_baseline(df, trim = 0.0)

  # Trimmed mean should be lower than untrimmed mean (outliers excluded)
  expect_lt(result_trim$trimmed_mean_energy, result_notrim$trimmed_mean_energy)
})
