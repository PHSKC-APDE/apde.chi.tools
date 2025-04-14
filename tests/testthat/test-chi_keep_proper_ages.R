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
  expect_error(chi_keep_proper_ages(NULL), "ph.data must be provided")

  # Test missing columns
  incomplete_data <- data.table(cat1 = "Age", cat1_group = "<18")
  expect_error(chi_keep_proper_ages(incomplete_data), "ph.data is missing required columns")

  # Test incorrect column classes
  bad_class_data <- create_test_data()
  bad_class_data[, chi_age := as.character(chi_age)]
  expect_error(chi_keep_proper_ages(bad_class_data), "should be of class 'numeric or integer'")
})

test_that("Age filtering works correctly for cat1 Age groups", {
  # Create test data
  test.data <- create_test_data()

  # Filter data
  filtered_data <- chi_keep_proper_ages(test.data)

  # Test age group '<18'
  age_group_rows <- filtered_data[cat1 == "Age" & cat1_group == "<18"]
  expect_true(all(age_group_rows$chi_age < 18))

  # Test age group '18-24'
  age_group_rows <- filtered_data[cat1 == "Age" & cat1_group == "18-24"]
  expect_true(all(age_group_rows$chi_age >= 18 & age_group_rows$chi_age <= 24))

  # Test age group '25-44'
  age_group_rows <- filtered_data[cat1 == "Age" & cat1_group == "25-44"]
  expect_true(all(age_group_rows$chi_age >= 25 & age_group_rows$chi_age <= 44))

  # Test age group '45-64'
  age_group_rows <- filtered_data[cat1 == "Age" & cat1_group == "45-64"]
  expect_true(all(age_group_rows$chi_age >= 45 & age_group_rows$chi_age <= 64))

  # Test age group '65-74'
  age_group_rows <- filtered_data[cat1 == "Age" & cat1_group == "65-74"]
  expect_true(all(age_group_rows$chi_age >= 65 & age_group_rows$chi_age <= 74))

  # Test age group '75+'
  age_group_rows <- filtered_data[cat1 == "Age" & cat1_group == "75+"]
  expect_true(all(age_group_rows$chi_age >= 75))
})

test_that("Age filtering works correctly for cat2 Age groups", {
  # Create test data
  test.data <- create_test_data()

  # Filter data
  filtered_data <- chi_keep_proper_ages(test.data)

  # Test age group '0-4'
  age_group_rows <- filtered_data[cat2 == "Age" & cat2_group == "0-4"]
  expect_true(all(age_group_rows$chi_age >= 0 & age_group_rows$chi_age <= 4))

  # Test age group '5-9'
  age_group_rows <- filtered_data[cat2 == "Age" & cat2_group == "5-9"]
  expect_true(all(age_group_rows$chi_age >= 5 & age_group_rows$chi_age <= 9))

  # Test age group '10-14'
  age_group_rows <- filtered_data[cat2 == "Age" & cat2_group == "10-14"]
  expect_true(all(age_group_rows$chi_age >= 10 & age_group_rows$chi_age <= 14))

  # Test age group '15-17'
  age_group_rows <- filtered_data[cat2 == "Age" & cat2_group == "15-17"]
  expect_true(all(age_group_rows$chi_age >= 15 & age_group_rows$chi_age <= 17))
})

test_that("Non-Age categories are not filtered", {
  # Create test data
  test.data <- create_test_data()

  # Filter data
  filtered_data <- chi_keep_proper_ages(test.data)

  # Test for geographic category (completely unrelated to Age)
  geo_rows_original <- test.data[cat1 == "King County"]
  geo_rows_filtered <- filtered_data[cat1 == "King County"]

  # For completely non-Age categories, all rows should remain
  # (note: filtering only applies when cat1 or cat2 is "Age")
  expect_equal(nrow(geo_rows_original), nrow(geo_rows_filtered))

  # Check that all unique chi_age values remain for King County rows
  geo_original_ages <- sort(unique(geo_rows_original$chi_age))
  geo_filtered_ages <- sort(unique(geo_rows_filtered$chi_age))

  expect_equal(geo_original_ages, geo_filtered_ages)

  # Also check that Gender categories not paired with Age are preserved
  gender_only_original <- test.data[cat1 == "Gender" & cat2 != "Age"]
  gender_only_filtered <- filtered_data[cat1 == "Gender" & cat2 != "Age"]

  expect_equal(nrow(gender_only_original), nrow(gender_only_filtered))
})

test_that("Count sums decrease after filtering", {
  # Create test data
  test.data <- create_test_data()

  # Sum of counts before filtering
  original_count_sum <- sum(test.data$count)

  # Filter data
  filtered_data <- chi_keep_proper_ages(test.data)

  # Sum of counts after filtering
  filtered_count_sum <- sum(filtered_data$count)

  # There should be fewer rows after filtering
  expect_true(nrow(filtered_data) < nrow(test.data))

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
  expect_equal(nrow(filtered_data), expected_total_rows)
})

