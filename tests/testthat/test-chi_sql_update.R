# Tests for chi_sql_update
test_that("chi_sql_update validates inputs", {
  test_data <- setup_test_data()

  expect_warning(
    chi_sql_update(
      CHIestimates = test_data$my.estimate,
      CHImetadata = test_data$my.metadata,
      table_name = 'JustTesting',
      server = 'development',
      replace_table = FALSE
    ),
    "Validation may be flawed for the following variables because they are 100% missing"
  )

  expect_error(chi_sql_update(),
               "The results table to push to SQL \\(CHIestimates\\) is missing")
  expect_error(suppressWarnings(chi_sql_update(CHIestimates = test_data$my.estimate)),
               "The metadata table to push to SQL \\(CHImetadata\\) is missing")
  expect_error(suppressWarnings(chi_sql_update(CHIestimates = test_data$my.estimate,
                                               CHImetadata = test_data$my.metadata)),
               "The table_name argument is missing")
})
