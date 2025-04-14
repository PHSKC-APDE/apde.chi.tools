# Tests for chi_calc
test_that("chi_calc performs basic calculations correctly", {
  test_data <- setup_test_data()

  set <-  test_data$my.analysis_set_twosets
  instruction <- apde.chi.tools::chi_generate_tro_shell(ph.analysis_set = set, end.year = 2023, year.span = 5, trend.span = 3, trend.periods = 3)
  DTgeneric <- test_data$my.generic_data
  #make a denominator come out as 0
  DTgeneric$indicator
  DTgeneric[chi_race_7 == "White" , indicator1 := "never"]
  result <- chi_calc(
    ph.data = DTgeneric,
    ph.instructions = instruction,
    ci = 0.90,
    rate = FALSE,
    source_name = "test",
    source_date = Sys.Date(),
    small_num_suppress =  F
  )


  expect_s3_class(result, "data.table")
  expect_true(all(c("result", "lower_bound", "upper_bound") %in% names(result)))
  expect_true(all(result$result >= 0 & result$result <= 1))  # For proportions
  expect_type(result$result, "double")
  expect_type(result$numerator, "double")
  expect_type(result$denominator, "double")
  expect_type(result$indicator_key, "character")
  expect_true(all(!is.na(result[numerator == 0 & denominator != 0,se])))
})
