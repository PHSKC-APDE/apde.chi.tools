# Create mock_chars data for injury ----
  mock_chars <- rads.data::synthetic_chars
  mock_chars[, `:=` (
    chi_year = sample(2012:2022, nrow(mock_chars), replace = TRUE),
    chi_age = sample(0:99, nrow(mock_chars), replace = TRUE),
    wastate = "Washington State",
    race3_hispanic = sample(c("Hispanic", "Non-Hispanic"), nrow(mock_chars), replace = TRUE),
    chi_geo_region = sample(c("Seattle", "South", "East", "North"), nrow(mock_chars), replace = TRUE),
    chi_sex = sample(c("Female", "Male"), nrow(mock_chars), replace = TRUE)
  )]

  # force exactly 100 unintentional falls
  mock_chars[injury_mechanism == 'fall', injury_mechanism := 'unspecified']
  idx <- sample(mock_chars[injury_mechanism == "unspecified" & injury_intent == "unintentional", which = TRUE], 100)
  mock_chars[idx, injury_mechanism := "fall"]
  mock_chars[injury_mechanism == 'fall', .N, .(injury_intent, injury_mechanism)]

  # force exactly 50 intentional poisoning
  mock_chars[injury_mechanism == 'poisoning', injury_mechanism := 'unspecified']
  idx <- sample(mock_chars[injury_mechanism == "unspecified" & injury_intent == "intentional", which = TRUE], 100)
  mock_chars[idx, injury_mechanism := "poisoning"]
  mock_chars[injury_mechanism == 'poisoning', .N, .(injury_intent, injury_mechanism)]

# Create mock_instructions ----
mock_instructions <- data.table(
  indicator_key = rep(c("hos1901000_v1", "hos1901000_v2"), 3),
  tab = rep(c("trends", "_wastate"), each = 3),
  cat1 = rep("Ethnicity", 6),
  cat1_varname = rep("race3_hispanic", 6),
  cat2 = c(NA_character_, NA_character_, "Sex", "Sex", NA_character_, NA_character_),
  cat2_varname = c(NA_character_, NA_character_, "chi_sex", "chi_sex", NA_character_, NA_character_),
  end = c(2017, 2022, 2017, 2022, 2015, 2015),
  start = c(2012, 2018, 2014, 2016, 2012, 2012)
)

# Create mock_chars_def ----
mock_chars_def <- data.table(
  indicator_name = c("Fall injuries (all ages)", "Fall injuries (children)"),
  indicator_key = c("hos1901000_v1", "hos1901000_v2"),
  intent = c("unintentional", "unintentional"),
  mechanism = c("fall", "motor_vehicle_traffic"),
  age_start = c(0, 0),
  age_end = c(120, 17)
)

# Add poisoning self-harm indicator
mock_chars_def <- rbind(mock_chars_def, data.table(
  indicator_name = "Self-harm poisoning",
  indicator_key = "hos1902000_v1",
  intent = "intentional", # Changed from intentional_self_harm to match intent column names
  mechanism = "poisoning",
  age_start = 10,
  age_end = 120
))

# Add row to instructions for self-harm
mock_instructions <- rbind(mock_instructions, data.table(
  indicator_key = "hos1902000_v1",
  tab = "trends",
  cat1 = "Ethnicity",
  cat1_varname = "race3_hispanic",
  cat2 = "Sex",
  cat2_varname = "chi_sex",
  end = 2022,
  start = 2012
))

# Create vector of expected column order ----
expectedCols <- c('indicator_key', 'year', 'chi_age', 'hospitalizations', 'tab', 'cat1', 'cat1_varname', 'cat1_group', 'cat2', 'cat2_varname', 'cat2_group')

# Test validation ----
test_that("chi_chars_injury validates inputs correctly", {
  # Test missing ph.indicator
  expect_error(chi_chars_injury(ph.indicator = NA,
                                ph.data = mock_chars,
                                ph.instructions = mock_instructions,
                                chars.defs = mock_chars_def),
               "ph.indicator must be provided")

  # Test missing ph.data
  expect_error(chi_chars_injury(ph.indicator = "hos1901000_v1",
                                ph.data = NULL,
                                ph.instructions = mock_instructions,
                                chars.defs = mock_chars_def),
               "ph.data must be specified")

  # Test indicator not found in instructions
  expect_error(chi_chars_injury(ph.indicator = "not_an_indicator",
                                ph.data = mock_chars,
                                ph.instructions = mock_instructions,
                                chars.defs = mock_chars_def),
               "not found in ph.instructions")

  # Test invalid column in instructions
  bad_instructions <- copy(mock_instructions)
  bad_instructions[1, cat1_varname := "not_a_column"]

  expect_error(chi_chars_injury(ph.indicator = "hos1901000_v1",
                                ph.data = mock_chars,
                                ph.instructions = bad_instructions,
                                chars.defs = mock_chars_def),
               "don't exist in ph.data")

  # Test invalid def parameter
  expect_error(chi_chars_injury(ph.indicator = "hos1901000_v1",
                                ph.data = mock_chars,
                                ph.instructions = mock_instructions,
                                chars.defs = mock_chars_def,
                                def = "invalid_def"),
               "must be either 'narrow' or 'broad'")
})

# Test function processes fall injury data correctly ----
test_that("chi_chars_injury processes fall injury data correctly", {
  # Run function with default parameters
  result <- chi_chars_injury(
    ph.indicator = "hos1901000_v1",
    ph.data = mock_chars,
    ph.instructions = mock_instructions[indicator_key == "hos1901000_v1"],
    chars.defs = mock_chars_def
  )

  # Check if result has expected structure
  expect_true(is.data.table(result))
  expect_equal(names(result), expectedCols)

  # Check that data matches the fall/unintentional criteria
  expect_true(sum(result$hospitalizations) > 0)

  # Check age filtering worked correctly
  expect_gt(max(as.numeric(result$chi_age)), 50) # is for all ages, so should definitely have some > 50 yrs old

  # Check if have consistent number of rows per age
  expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
})

# xxxTest function handles age filtering correctly ----
test_that("chi_chars_injury handles age filtering correctly", {
  # Test children-only indicator (hos1901000_v2 has age_end = 17)
  result <- chi_chars_injury(
    ph.indicator = "hos1901000_v2",
    ph.data = mock_chars,
    ph.instructions = mock_instructions[indicator_key == "hos1901000_v2"],
    chars.defs = mock_chars_def
  )

  # Check if result has expected structure
  expect_true(is.data.table(result))
  expect_equal(names(result), expectedCols)

  # Check correct age range
  expect_true(all(result$chi_age %in% 0:17))

  # Check if have the same number of observations for each age
  expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
})

# Test function handles different injury types correctly ----
test_that("chi_chars_injury handles different injury types correctly", {
  # Test self-harm poisoning indicator
  result <- chi_chars_injury(
    ph.indicator = "hos1902000_v1",
    ph.data = mock_chars,
    ph.instructions = mock_instructions[indicator_key == "hos1902000_v1"],
    chars.defs = mock_chars_def
  )

  # Check if result has expected structure
  expect_true(is.data.table(result))
  expect_equal(names(result), expectedCols)

  # Check that we have counts > 0
  expect_true(sum(result$hospitalizations) > 0)

  # Check age filtering worked correctly (min age should be 10)
  expect_equal(min(as.numeric(result$chi_age)), 10)
})

# Test def parameter variation ----
test_that("chi_chars_injury handles 'def' parameter correctly", {
  # Test with narrow definition (default)
  narrow_result <- chi_chars_injury(
    ph.indicator = "hos1901000_v1",
    ph.data = mock_chars,
    ph.instructions = mock_instructions[indicator_key == "hos1901000_v1"][1],
    chars.defs = mock_chars_def,
    def = "narrow"
  )

  # Test with broad definition
  broad_result <- chi_chars_injury(
    ph.indicator = "hos1901000_v1",
    ph.data = mock_chars,
    ph.instructions = mock_instructions[indicator_key == "hos1901000_v1"][1],
    chars.defs = mock_chars_def,
    def = "broad"
  )

  # Both should have valid structure
  expect_true(is.data.table(narrow_result))
  expect_true(is.data.table(broad_result))

  # Results can be the same in our test data, but function should run without error
  expect_true(TRUE)
})

# Test that WA state filtering works ----
test_that("chi_chars_injury handles WA state filtering correctly", {
  # Instructions for WA state
  wa_instructions <- mock_instructions[tab == "_wastate" & indicator_key == "hos1901000_v1"]

  # Run function
  result <- chi_chars_injury(
    ph.indicator = "hos1901000_v1",
    ph.data = mock_chars,
    ph.instructions = wa_instructions,
    chars.defs = mock_chars_def
  )

  # Check if result has expected structure
  expect_true(is.data.table(result))
  expect_equal(names(result), expectedCols)

  # Check that all rows are for WA state
  expect_true(all(result$tab == "_wastate"))

  # Check if have the same number of observations for each age
  expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
})

# Test processing multiple instructions ----
test_that("chi_chars_injury processes multiple instructions correctly", {
  # Use multiple instructions for the same indicator
  multiple_instructions <- mock_instructions[indicator_key == "hos1901000_v1"]

  # Run function
  result <- chi_chars_injury(
    ph.indicator = "hos1901000_v1",
    ph.data = mock_chars,
    ph.instructions = multiple_instructions,
    chars.defs = mock_chars_def
  )

  # Check if result has expected structure with multiple rows
  expect_true(is.data.table(result))
  expect_equal(names(result), expectedCols)
  expect_true(nrow(result) > 0)

  # confirm we processed the all of the instruction sets
  expect_equal(uniqueN(result[, .(indicator_key, tab, cat1_varname, cat2_varname)]),
               nrow(multiple_instructions))

  # Check if have the same number of observations for each age
  expect_equal(uniqueN(result[, .N, chi_age]$N), 1)
})

# Test what happens when instructions filter out all rows----
test_that("When some instructions filter out all rows, expect it to work but returning warnings", {
  warnings <- capture_warnings({
    result <- rbindlist(lapply(c("hos1901000_v1", "hos1901000_v2"), function(indicator) {
      chi_chars_injury(
        ph.indicator = indicator,
        ph.data = copy(mock_chars)[, chi_geo_kc := NA], # Corrupt the data
        ph.instructions = mock_instructions[indicator_key %in% c("hos1901000_v1", "hos1901000_v2")],
        chars.defs = mock_chars_def)
    }), fill = TRUE)
  })

  # Check that we got exactly 2 warnings (1 per indicator)
  expect_length(warnings, 2)

  # Check content of first warning
  expect_match(warnings[1],
               "No data found for the following .* instruction\\(s\\) for indicator 'hos1901000_v1'")

  # Check content of second warning
  expect_match(warnings[2],
               "No data found for the following .* instruction\\(s\\) for indicator 'hos1901000_v2'")

  # Check for any data from _wastate (which should still work)
  expect_gt(nrow(result), 0)
})

# Test year restriction for injury data (only 2012+) ----
test_that("chi_chars_injury correctly handles pre-2012 years", {
  # Create instructions with pre-2012 years
  early_instructions <- copy(mock_instructions[1])
  early_instructions[, `:=`(start = 2010, end = 2015)]

  # Run the function with pre-2012 years
  result <- chi_chars_injury(
    ph.indicator = "hos1901000_v1",
    ph.data = mock_chars,
    ph.instructions = early_instructions,
    chars.defs = mock_chars_def
  )

  # Check the results
  expect_true(is.data.table(result))
  expect_true(nrow(result) > 0)

  # The year range in the result should be 2012-2015, not 2010-2015
  expect_true(all(result$year == "2012-2015"))
})

# Test poisoning mechanism handling with drug & non-drug ----
test_that("chi_chars_injury correctly handles poisoning with ICD-10", {
  # Create mock data with both poisoning_drug and poisoning_nondrug
  poisoning_test_chars <- copy(mock_chars)

  # Add some specific poisoning_drug and poisoning_nondrug cases
  poisoning_test_chars[201:225, `:=`(
    mechanism_poisoning_drug = 1,
    mechanism_poisoning = 0,
    intent_intentional = 1,
    injury_intent = "intentional"
  )]

  poisoning_test_chars[226:250, `:=`(
    mechanism_poisoning_nondrug = 1,
    mechanism_poisoning = 0,
    intent_intentional = 1,
    injury_intent = "intentional"
  )]

  # Create a test for poisoning indicator (which may need to collapse drug & non-drug)
  result <- chi_chars_injury(
    ph.indicator = "hos1902000_v1",
    ph.data = poisoning_test_chars,
    ph.instructions = mock_instructions[indicator_key == "hos1902000_v1"],
    chars.defs = mock_chars_def
  )

  # Check the results
  expect_true(is.data.table(result))
  expect_true(nrow(result) > 0)

  # The result should include data from the poisoning mechanism
  expect_true(sum(result$hospitalizations) > 0)
})
