# Tests for chi_calc
test_that("chi_calc performs basic calculations correctly", {
  test_data <- setup_test_data()

  result <- chi_calc(
    ph.data = test_data$my.analytic,
    ph.instructions = test_data$my.instructions,
    ci = 0.90,
    rate = FALSE,
    source_name = "test",
    source_date = Sys.Date()
  )

  expect_s3_class(result, "data.table")
  expect_true(all(c("result", "lower_bound", "upper_bound") %in% names(result)))
  expect_true(all(result$result >= 0 & result$result <= 1))  # For proportions
  expect_type(result$result, "double")
  expect_type(result$numerator, "double")
  expect_type(result$denominator, "double")
  expect_type(result$indicator_key, "character")
})
