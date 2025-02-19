# Tests for chi_qa_tro
test_that("chi_qa_tro validates data structure", {
  test_data <- setup_test_data()

  result <- chi_calc(
    ph.data = test_data$my.analytic,
    ph.instructions = test_data$my.instructions,
    rate = FALSE,
    source_name = "test",
    source_date = Sys.Date()
  )

  expect_error(chi_qa_tro(), 'argument "CHIestimates" is missing, with no default')
  expect_type(suppressWarnings(chi_qa_tro(CHIestimates = result, CHImetadata = test_data$my.metadata, verbose = FALSE)), "double")
  expect_error(suppressWarnings(chi_qa_tro(result, test_data$my.metadata, verbose = FALSE)), NA)
})
