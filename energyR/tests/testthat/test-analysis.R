test_that("compute_adjacent_deltas fills deltas for workload rows only", {
  df <- data.frame(
    PKG     = c(100, 115, 102, 118, 99, 114),
    seconds = c(1.0, 1.1, 1.0, 1.1, 1.0, 1.1),
    work    = c("base", "workload", "base", "workload", "base", "workload")
  )
  result <- compute_adjacent_deltas(df, "workload")

  # Workload rows: indices 2, 4, 6 (1-based)
  expect_equal(result$delta_PKG[2],  115 - 100)
  expect_equal(result$delta_PKG[4],  118 - 102)
  expect_equal(result$delta_PKG[6],  114 - 99)

  # Baseline rows should be NA
  expect_true(is.na(result$delta_PKG[1]))
  expect_true(is.na(result$delta_PKG[3]))
  expect_true(is.na(result$delta_PKG[5]))
})

test_that("top_anova_terms returns correct structure", {
  model  <- lm(mpg ~ cyl * hp + wt, data = mtcars)
  result <- top_anova_terms(anova(model))

  expect_true("term" %in% names(result))
  expect_true("percentage_variance" %in% names(result))
  expect_true(nrow(result) <= 10)
  expect_true(all(result$percentage_variance > 0))
  # percentages should sum to ~100
  expect_equal(sum(result$percentage_variance), 100, tolerance = 1e-6)
})

test_that("top_anova_terms respects n parameter", {
  model  <- lm(mpg ~ cyl + hp + wt + disp + gear, data = mtcars)
  result <- top_anova_terms(anova(model), n = 2)
  expect_true(nrow(result) <= 2)
})

test_that("top_anova_terms returns empty df when no significant terms", {
  model  <- lm(mpg ~ carb, data = mtcars)
  result <- top_anova_terms(anova(model), alpha = 1e-10)
  expect_equal(nrow(result), 0)
})
