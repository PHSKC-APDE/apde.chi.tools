# Tests for chi_qa_tro
test_that("chi_qa_tro validates data structure", {
  test_data <- setup_test_data()

  # Should return 0 and give warning about missing values
  expect_warning(
    result <- chi_qa_tro(
      CHIestimates = test_data$my.estimate,
      CHImetadata = test_data$my.metadata,
      acs = F,
      verbose = F
    ),
    "100% missing"
  )

  expect_equal(result, 0)

  # Should give warning about missing cat1 column, but also an error b/c of nested rads function
  expect_warning(
    expect_error(
      chi_qa_tro(
        CHIestimates = test_data$my.estimate[, cat1 := NULL],
        CHImetadata = test_data$my.metadata,
        acs = F,
        verbose = T
      ),
      "Validation of TSQL data types necessitates exactly one TSQL datatype per column name"
    ),
    "You are missing the following critical columns\\(s\\) in CHIestimates: cat1"
  )

  # Test missing argument case
  expect_error(chi_qa_tro(verbose = F), 'argument "CHIestimates" is missing, with no default')
})
