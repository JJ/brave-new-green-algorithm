test_that("process_deltas computes correct deltas", {
  set.seed(42)
  # Build a single 61-row block with known structure
  block <- data.frame(
    PKG     = rep(NA_real_, 61),
    seconds = rep(NA_real_, 61)
  )
  block$PKG[1]     <- 100
  block$seconds[1] <- 1.0
  for (i in seq(2, 60, by = 2)) {
    block$PKG[i]         <- 120   # workload
    block$seconds[i]     <- 1.2
    block$PKG[i + 1]     <- 100   # baseline
    block$seconds[i + 1] <- 1.0
  }

  result <- process_deltas(block)

  # Should have 30 rows (workload rows only)
  expect_equal(nrow(result), 30)

  # All delta_PKG values should be 120 - (100 + 100)/2 = 20
  expect_true(all(abs(result$delta_PKG - 20) < 1e-9))

  # All delta_seconds values should be 1.2 - (1.0 + 1.0)/2 = 0.2
  expect_true(all(abs(result$delta_seconds - 0.2) < 1e-9))
})

test_that("process_deltas handles multiple blocks", {
  set.seed(1)
  make_block <- function() {
    b <- data.frame(PKG = rep(100, 61), seconds = rep(1, 61))
    for (i in seq(2, 60, by = 2)) {
      b$PKG[i]     <- 110
      b$seconds[i] <- 1.1
    }
    b
  }
  two_blocks <- rbind(make_block(), make_block())
  result <- process_deltas(two_blocks)
  expect_equal(nrow(result), 60)
})
