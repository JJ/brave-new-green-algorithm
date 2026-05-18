test_that("wilcoxon_tests returns correct structure", {
  df <- data.frame(
    PKG             = c(rnorm(20, 100, 5), rnorm(20, 115, 5)),
    dimension       = 3,
    population_size = 200,
    work            = rep(c("baseline", "workload"), each = 20)
  )
  result <- wilcoxon_tests(df)

  expect_equal(nrow(result), 1)
  expect_true("p_value" %in% names(result))
  expect_true("work1" %in% names(result))
  expect_true("work2" %in% names(result))
})

test_that("wilcoxon_tests handles multiple parameter combinations", {
  df <- do.call(rbind, lapply(c(3, 5), function(dim) {
    do.call(rbind, lapply(c(200, 400), function(pop) {
      data.frame(
        PKG             = c(rnorm(20, 100, 5), rnorm(20, 115, 5)),
        dimension       = dim,
        population_size = pop,
        work            = rep(c("baseline", "workload"), each = 20)
      )
    }))
  }))
  result <- wilcoxon_tests(df)
  expect_equal(nrow(result), 4)  # 2 dims x 2 pop sizes
})

test_that("wilcoxon_tests skips combinations with != 2 work groups", {
  df <- data.frame(
    PKG             = rnorm(20, 100, 5),
    dimension       = 3,
    population_size = 200,
    work            = "only_one_group"
  )
  result <- wilcoxon_tests(df)
  expect_equal(nrow(result), 0)
})

test_that("wilcoxon_tests works with a custom column name", {
  df <- data.frame(
    delta_PKG       = c(rnorm(20, 10, 2), rnorm(20, 20, 2)),
    dimension       = 3,
    population_size = 200,
    work            = rep(c("v1", "v2"), each = 20)
  )
  result <- wilcoxon_tests(df, column_name = "delta_PKG")
  expect_equal(nrow(result), 1)
  expect_true(result$p_value < 0.05)  # Should be significantly different
})
