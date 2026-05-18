test_that("process_covariates adds correct covariate columns", {
  set.seed(5)
  block <- data.frame(PKG = rep(100, 61), seconds = rep(1, 61))
  for (i in seq(2, 60, by = 2)) {
    block$PKG[i]     <- 115
    block$seconds[i] <- 1.15
  }

  result <- process_covariates(block)

  expect_true("PKG_baseline_prev" %in% names(result))
  expect_true("PKG_baseline_post" %in% names(result))
  expect_true("seconds_baseline_prev" %in% names(result))
  expect_true("seconds_baseline_post" %in% names(result))

  # All baseline covariate values should be 100 (the baseline PKG)
  expect_true(all(result$PKG_baseline_prev == 100))
  expect_true(all(result$PKG_baseline_post == 100))
})
