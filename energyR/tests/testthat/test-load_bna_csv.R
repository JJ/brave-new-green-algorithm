test_that("load_bna_csv drops baseline columns by default", {
  # Create a temp CSV with baseline columns
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(
    PKG             = c(100, 110),
    seconds         = c(1.0, 1.1),
    dimension       = c(3, 5),
    population_size = 200,
    work            = "baseline",
    alpha           = 0.1,
    max_gens        = 25,
    different_seeds = 1,
    diff_fitness    = 0.01,
    generations     = 100,
    evaluations     = 1000
  )
  write.csv(df, tmp, row.names = FALSE)

  result <- load_bna_csv(tmp)
  expect_false("alpha" %in% names(result))
  expect_false("max_gens" %in% names(result))
  expect_true("PKG" %in% names(result))
})

test_that("load_bna_csv keeps all columns when drop_baseline_cols = FALSE", {
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(
    PKG      = 100,
    seconds  = 1.0,
    work     = "workload",
    max_gens = 25
  )
  write.csv(df, tmp, row.names = FALSE)
  result <- load_bna_csv(tmp, drop_baseline_cols = FALSE)
  expect_true("max_gens" %in% names(result))
})
