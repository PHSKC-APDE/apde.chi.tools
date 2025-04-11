# Tests for chi_generate_analysis_set
test_that("chi_generate_analysis_set validates inputs", {
  expect_error(chi_generate_analysis_set(), "Either data_source parameter or CHIestimates parameter must be provided")
  expect_error(chi_generate_analysis_set(data_source = 123), "data_source must be a single character string")
  expect_error(chi_generate_analysis_set(CHIestimates = 123), "CHIestimates must be a data.table or data.frame")
})

test_that("chi_generate_analysis_set generates expected output", {
  TestData <- setup_test_data()
  TestData$my.generic_data
  TestData$my.analysis_set_twosets
  test_analysis_instructions_results <- apde.chi.tools::chi_generate_tro_shell(TestData$my.analysis_set_twosets, end.year = 2023, year.span = 3, trend.span = 3, trend.periods = 5)
  test_chi_calc_output <- apde.chi.tools::chi_calc(ph.data =  TestData$my.generic_data, ph.instructions = test_analysis_instructions_results, source_name =  "test",source_date = as.Date("2025-05-10"), ci = .95)


})
