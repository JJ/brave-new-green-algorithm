test_that("compute_deltas subtracts correct baseline medians", {
  baseline_summary <- data.frame(
    dimension       = c(3, 5),
    population_size = c(200, 200),
    median_energy   = c(100, 110),
    median_time     = c(1.0, 1.1)
  )
  workload <- data.frame(
    dimension       = rep(c(3, 5), each = 5),
    population_size = 200,
    PKG             = c(rep(115, 5), rep(130, 5)),
    seconds         = c(rep(1.15, 5), rep(1.25, 5)),
    max_gens        = 10
  )
  result <- compute_deltas(baseline_summary, workload)

  expect_equal(result$delta_PKG[result$dimension == 3],   rep(15, 5))
  expect_equal(result$delta_PKG[result$dimension == 5],   rep(20, 5))
  expect_equal(result$delta_seconds[result$dimension == 3], rep(0.15, 5),
               tolerance = 1e-9)
})

test_that("compute_deltas ignores missing dimension-pop combos gracefully", {
  baseline_summary <- data.frame(
    dimension = 3, population_size = 200,
    median_energy = 100, median_time = 1.0
  )
  workload <- data.frame(
    dimension = c(3, 7),  # 7 not in baseline
    population_size = 200,
    PKG = c(110, 120),
    seconds = c(1.1, 1.2)
  )
  # Should not error
  expect_no_error(compute_deltas(baseline_summary, workload))
})
