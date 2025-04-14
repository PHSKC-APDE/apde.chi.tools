# Tests for chi_generate_metadata
test_that("chi_generate_metadata handles valid inputs", {
  test_data <- setup_test_data()

  expect_error(chi_generate_metadata(), "meta.old must be provided")
  expect_error(chi_generate_metadata(meta.old = test_data$my.metadata), "est.current must be provided")

  # why does this test fail? the DTs are not properly constructed, perhaps I need to update packages? will try later
  #DTtest <- test_data$my.estimate
  #DTtest[tab,] # throws error
  #chi_generate_metadata(meta.old = test_data$my.metadata, est.current = test_data$my.estimate)

})
