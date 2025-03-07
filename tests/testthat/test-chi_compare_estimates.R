# Tests for chi_compare_estimates
test_that("chi_compare_estimates identifies differences correctly", {
  test_data <- setup_test_data()

  comparison <- chi_compare_estimates(
    OLD = test_data$my.estimate_old,
    NEW = test_data$my.estimate,
    OLD.year = "2022",
    NEW.year = "2023",
    META = test_data$my.metadata
  )

  expect_true("absolute.diff" %in% names(comparison))
  expect_true("relative.diff" %in% names(comparison))
  expect_equal(nrow(comparison[notable == 1]), 1) # Only South KC should have a notable difference
})
