# Tests for chi_generate_analysis_set
test_that("chi_generate_analysis_set validates inputs", {
  expect_error(chi_generate_analysis_set(), "data_source parameter must be provided")
  expect_error(chi_generate_analysis_set(123), "data_source must be a single character string")
})
