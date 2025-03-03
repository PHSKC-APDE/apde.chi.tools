# Tests for chi_generate_analysis_set
test_that("chi_generate_analysis_set validates inputs", {
  expect_error(chi_generate_analysis_set(), "Either data_source parameter or CHIestimates parameter must be provided")
  expect_error(chi_generate_analysis_set(data_source = 123), "data_source must be a single character string")
  expect_error(chi_generate_analysis_set(CHIestimates = 123), "CHIestimates must be a data.table or data.frame")
})
