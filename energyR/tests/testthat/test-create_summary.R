test_that("create_summary returns expected columns", {
  df <- data.frame(
    delta_PKG       = rnorm(60, 10, 3),
    delta_seconds   = rnorm(60, 0.1, 0.02),
    PKG             = rnorm(60, 110, 5),
    dimension       = rep(c(3, 5), each = 30),
    population_size = 200,
    max_gens        = 10,
    evaluations     = sample(500:5000, 60)
  )
  result <- create_summary(df)

  expect_true("mean_delta_PKG" %in% names(result))
  expect_true("median_delta_PKG" %in% names(result))
  expect_true("trimmed_mean_delta_PKG" %in% names(result))
  expect_true("sd_delta_PKG" %in% names(result))
  expect_equal(nrow(result), 2)  # 2 dimensions x 1 pop x 1 max_gens
})

test_that("create_summary works without evaluations column", {
  df <- data.frame(
    delta_PKG       = rnorm(30, 10, 3),
    delta_seconds   = rnorm(30, 0.1, 0.02),
    PKG             = rnorm(30, 110, 5),
    dimension       = 3,
    population_size = 200,
    max_gens        = 10
  )
  expect_no_error(create_summary(df))
})
