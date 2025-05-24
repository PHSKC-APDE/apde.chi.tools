# Helper function to create test data (not in helper.R because very specific to this function)
create_test_data <- function() {
  cat1_age6 <- data.table(cat1 = 'Age', cat1_varname = 'age6',
                          cat1_group = c('<18', '18-24', '25-44', '45-64', '65-74', '75+'),
                          mykey = 1)
  cat2_sex <- data.table(cat2 = 'Gender', cat2_varname = 'chi_sex',
                         cat2_group = c('Female', 'Male'), mykey = 1)
  cat1_sex <- setnames(copy(cat2_sex), gsub('cat2', 'cat1', names(cat2_sex)))
  cat2_yage4 <- data.table(cat2 = 'Age', cat2_varname = 'yage4',
                           cat2_group = c('0-4', '10-14', '15-17', '5-9'), mykey = 1)
  cat1_geo <- data.table(cat1 = 'King County', cat1_varname = 'chi_geo_kc',
                         cat1_group = 'King County', mykey = 1)
  chi_ages <- data.table(chi_age = 0:100, mykey = 1)

  test.counts <- rbind(
    merge(cat1_age6, cat2_sex, allow.cartesian = TRUE),
    merge(cat1_sex, cat2_yage4, allow.cartesian = TRUE),
    merge(cat1_geo, cat2_sex, allow.cartesian = TRUE))

  test.counts <- merge(test.counts, chi_ages, allow.cartesian = TRUE)[, mykey := NULL]

  test.counts[, `:=` (indicator_key = 'indicator1',
                      year = '2020',
                      tab = 'mytab',
                      count = 1)]

  return(test.counts)
}

test_that("Input validation works correctly", {
  # Test NULL input
  expect_error(chi_drop_illogical_ages(NULL), "ph.data must be provided")

  # Test missing columns
  incomplete_data <- data.table(cat1 = "Age", cat1_group = "<18")
  expect_error(chi_drop_illogical_ages(incomplete_data), "ph.data is missing required columns")

  # Test incorrect column classes
  bad_class_data <- create_test_data()

  bad_class_data[, chi_age := as.character(chi_age)]
  expect_no_error(chi_drop_illogical_ages(bad_class_data)) # should be fine because can convert losslessly to integer

  bad_class_data[, cat1_group := NULL][, cat1_group := as.Date('2024-01-01')]
  expect_error(chi_drop_illogical_ages(bad_class_data), "should be of class 'character' or 'factor', but is 'Date'")
})

test_that("Age filtering works correctly for cat1 Age groups", {
  # Create test data
  test.data <- create_test_data()

  # Filter data
  filtered.data <- chi_drop_illogical_ages(test.data)

  # Test age group '<18'
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "<18"]
  expect_true(all(age_group_rows$chi_age < 18))

  # Test age group '18-24'
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "18-24"]
  expect_true(all(age_group_rows$chi_age >= 18 & age_group_rows$chi_age <= 24))

  # Test age group '25-44'
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "25-44"]
  expect_true(all(age_group_rows$chi_age >= 25 & age_group_rows$chi_age <= 44))

  # Test age group '45-64'
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "45-64"]
  expect_true(all(age_group_rows$chi_age >= 45 & age_group_rows$chi_age <= 64))

  # Test age group '65-74'
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "65-74"]
  expect_true(all(age_group_rows$chi_age >= 65 & age_group_rows$chi_age <= 74))

  # Test age group '75+'
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "75+"]
  expect_true(all(age_group_rows$chi_age >= 75))

  # King County has no age, so should be lossless
  expect_equal(nrow(test.data[cat1 == 'King County']), nrow(filtered.data[cat1 == 'King County']))
})

test_that("Age filtering works correctly for cat2 Age groups", {
  # Create test data
  test.data <- create_test_data()

  # Filter data
  filtered.data <- chi_drop_illogical_ages(test.data)

  # Test age group '0-4'
  age_group_rows <- filtered.data[cat2 == "Age" & cat2_group == "0-4"]
  expect_true(all(age_group_rows$chi_age >= 0 & age_group_rows$chi_age <= 4))

  # Test age group '5-9'
  age_group_rows <- filtered.data[cat2 == "Age" & cat2_group == "5-9"]
  expect_true(all(age_group_rows$chi_age >= 5 & age_group_rows$chi_age <= 9))

  # Test age group '10-14'
  age_group_rows <- filtered.data[cat2 == "Age" & cat2_group == "10-14"]
  expect_true(all(age_group_rows$chi_age >= 10 & age_group_rows$chi_age <= 14))

  # Test age group '15-17'
  age_group_rows <- filtered.data[cat2 == "Age" & cat2_group == "15-17"]
  expect_true(all(age_group_rows$chi_age >= 15 & age_group_rows$chi_age <= 17))

  # Test when there is no age (KC x Gender), all rows preserved
  expect_equal(nrow(test.data[cat1 != 'Age' & cat2 != 'Age'][, .N, .(cat1, cat2)]),
               nrow(filtered.data[cat1 != 'Age' & cat2 != 'Age'][, .N, .(cat1, cat2)]))
})

test_that("Count sums decrease after filtering", {
  # Create test data
  test.data <- create_test_data()

  # Sum of counts before filtering
  original_count_sum <- sum(test.data$count)

  # Filter data
  filtered.data <- chi_drop_illogical_ages(test.data)

  # Sum of counts after filtering
  filtered_count_sum <- sum(filtered.data$count)

  # There should be fewer rows after filtering
  expect_true(nrow(filtered.data) < nrow(test.data))

  # Sum of counts should decrease
  expect_true(filtered_count_sum < original_count_sum)

  # Calculate exact expected count
  # For each age group, count rows that should be filtered out
  expected_filtered_rows <- test.data[
    (cat1 == "Age" & cat1_group == "<18" & chi_age >= 18) |
      (cat1 == "Age" & cat1_group == "18-24" & (chi_age < 18 | chi_age > 24)) |
      (cat1 == "Age" & cat1_group == "25-44" & (chi_age < 25 | chi_age > 44)) |
      (cat1 == "Age" & cat1_group == "45-64" & (chi_age < 45 | chi_age > 64)) |
      (cat1 == "Age" & cat1_group == "65-74" & (chi_age < 65 | chi_age > 74)) |
      (cat1 == "Age" & cat1_group == "75+" & chi_age < 75) |
      (cat2 == "Age" & cat2_group == "0-4" & (chi_age < 0 | chi_age > 4)) |
      (cat2 == "Age" & cat2_group == "5-9" & (chi_age < 5 | chi_age > 9)) |
      (cat2 == "Age" & cat2_group == "10-14" & (chi_age < 10 | chi_age > 14)) |
      (cat2 == "Age" & cat2_group == "15-17" & (chi_age < 15 | chi_age > 17))
  ]

  # Count rows that should remain
  rows_with_age_categories <- test.data[(cat1 == "Age") | (cat2 == "Age")]
  non_age_categories_rows <- test.data[(cat1 != "Age") & (cat2 != "Age")]

  expected_age_rows_after_filtering <- nrow(rows_with_age_categories) - nrow(expected_filtered_rows)
  expected_total_rows <- expected_age_rows_after_filtering + nrow(non_age_categories_rows)

  # Check if we have the expected number of rows after filtering
  expect_equal(nrow(filtered.data), expected_total_rows)
})

test_that("Non-standard agevar name works fine", {
  # Create test data
  test.data <- create_test_data()

  # Copy chi_age to new column
  test.data[, myagevar := chi_age]

  # Filter
  alt <- chi_drop_illogical_ages(test.data, agevar = 'myagevar')
  std <- chi_drop_illogical_ages(test.data)

  # Compare output
  expect_identical(alt, std)

})

test_that("Age groups with no matching ages", {
  test.data <- create_test_data()

  # Subset to young
  young_only_data <- test.data[chi_age <= 10]

  # Filter data
  filtered.data <- chi_drop_illogical_ages(young_only_data)

  # Old groups should be completed filtered out
  expect_equal(nrow(filtered.data[cat1 == "Age" & cat1_group == "75+"]), 0)
  expect_equal(nrow(filtered.data[cat1 == "Age" & cat1_group == "65-74"]), 0)
  expect_equal(nrow(filtered.data[cat1 == "Age" & cat1_group == "45-64"]), 0)

  # Youngest group shoudl still exist
  expect_true(nrow(filtered.data[cat1 == "Age" & cat1_group == "<18"]) > 0)
})

test_that("Boundary ages are handled correctly", {
  test.data <- create_test_data()

  # Filter data
  filtered.data <- chi_drop_illogical_ages(test.data)

  # Age 18 should be in "18-24" but NOT in "<18"
  expect_true(18 %in% filtered.data[cat1 == "Age" & cat1_group == "18-24"]$chi_age)
  expect_false(18 %in% filtered.data[cat1 == "Age" & cat1_group == "<18"]$chi_age)

  # Age 17 should be in "<18" and "15-17" but NOT in "18-24"
  expect_true(17 %in% filtered.data[cat1 == "Age" & cat1_group == "<18"]$chi_age)
  expect_true(17 %in% filtered.data[cat2 == "Age" & cat2_group == "15-17"]$chi_age)

  # Age 75 should be in "75+" but NOT in "65-74"
  expect_true(75 %in% filtered.data[cat1 == "Age" & cat1_group == "75+"]$chi_age)
  expect_false(75 %in% filtered.data[cat1 == "Age" & cat1_group == "65-74"]$chi_age)

  # Age 74 should be in "65-74" but NOT in "75+"
  expect_true(74 %in% filtered.data[cat1 == "Age" & cat1_group == "65-74"]$chi_age)
  expect_false(74 %in% filtered.data[cat1 == "Age" & cat1_group == "75+"]$chi_age)
})

test_that("Age filtering works when there is a non-standard cat#_group value and cat# == 'Age", {
  # Create the data
  test.data <- create_test_data()
  test.data[cat1_group == '<18', cat1_group := '<=18']

  # Check for warning
  expect_warning(chi_drop_illogical_ages(test.data), "Age: <=18")

  # Filter the data
  filtered.data <- suppressWarnings(chi_drop_illogical_ages(test.data))

  # Test age group '<=18' (should not have filtered)
  expect_equal(nrow(test.data[cat1_group == '<=18']),
               nrow(filtered.data[cat1_group == '<=18']))

  # Test age group '18-24' (should have filtered)
  age_group_rows <- filtered.data[cat1 == "Age" & cat1_group == "18-24"]
  expect_true(all(age_group_rows$chi_age >= 18 & age_group_rows$chi_age <= 24))

})
