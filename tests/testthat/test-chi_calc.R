# Tests for chi_calc
test_that("chi_calc produces consistent results whether running sequentially or in parallel", {
  # Set up environment
    test_data <- setup_test_data()

    set <-  test_data$my.analysis_set_twosets
    instruction <- apde.chi.tools::chi_generate_tro_shell(ph.analysis_set = set, end.year = 2023, year.span = 5, trend.span = 3, trend.periods = 3)
    DTgeneric <- test_data$my.generic_data
    #make a denominator come out as 0
    DTgeneric$indicator
    DTgeneric[race4 == "White" , indicator1 := "never"]


  # Create a core testing function
  run_core_test <- function(current_plan_name) {
    message(paste("\n--- Running chi_calc with plan:", current_plan_name, "---"))
    result <- suppressWarnings(chi_calc(  # suppressWarnings because may get warning that future was built under different version
      ph.data = DTgeneric,
      ph.instructions = instruction,
      ci = 0.90,
      rate = FALSE,
      source_name = "test",
      source_date = Sys.Date(),
      small_num_suppress =  F
    ))

    expect_s3_class(result, "data.table")
    expect_true(all(c("result", "lower_bound", "upper_bound") %in% names(result)))
    expect_true(all(result$result >= 0 & result$result <= 1))  # For proportions
    expect_type(result$result, "double")
    expect_type(result$numerator, "double")
    expect_type(result$denominator, "double")
    expect_type(result$indicator_key, "character")
    expect_true(all(!is.na(result[numerator == 0 & denominator != 0,se])))
  }

  # Run with PARALLEL (Multisession)
  plan(multisession, workers = 2)
  run_core_test("multisession (2 workers)")

  # Run with SEQUENTIAL
  # This switches the plan and acts as our "cleanup" step
  plan(sequential)
  run_core_test("sequential")
})

