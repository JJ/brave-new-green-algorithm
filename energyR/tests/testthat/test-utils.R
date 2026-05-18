test_that("add_pop_dim_label adds correct column", {
  df <- data.frame(population_size = c(200, 400), dimension = c(3, 5))
  result <- add_pop_dim_label(df)

  expect_true("population_dimension" %in% names(result))
  expect_equal(result$population_dimension[1], "Pop. size: 200, Dim.: 3")
  expect_equal(result$population_dimension[2], "Pop. size: 400, Dim.: 5")
})

test_that("add_pop_dim_label respects custom column names", {
  df <- data.frame(pop = 200, dim = 3)
  result <- add_pop_dim_label(df, pop_col = "pop", dim_col = "dim",
                               label_col = "my_label")
  expect_true("my_label" %in% names(result))
})

test_that("format_mean_sd_latex returns expected string", {
  result <- format_mean_sd_latex(100.123, 5.678)
  expect_equal(result, "$100.12 \\pm5.68$")
})

test_that("format_mean_sd_latex vectorises correctly", {
  result <- format_mean_sd_latex(c(100, 200), c(5, 10))
  expect_length(result, 2)
  expect_true(all(startsWith(result, "$")))
  expect_true(all(endsWith(result, "$")))
})

test_that("null_baseline_columns is a character vector", {
  expect_type(null_baseline_columns, "character")
  expect_true("alpha" %in% null_baseline_columns)
  expect_true("max_gens" %in% null_baseline_columns)
})
