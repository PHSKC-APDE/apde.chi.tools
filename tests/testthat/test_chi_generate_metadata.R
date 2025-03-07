# Tests for chi_generate_metadata
test_that("chi_generate_metadata handles valid inputs", {
  test_data <- setup_test_data()

  expect_error(chi_generate_metadata(), "meta.old must be provided")
  expect_error(chi_generate_metadata(meta.old = test_data$my.metadata), "est.current must be provided")
})
