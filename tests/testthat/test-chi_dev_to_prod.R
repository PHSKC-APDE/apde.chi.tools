# Tests for chi_dev_to_prod
library(testthat)

# Test validation ----
test_that("table_name = NULL throws error", {
  expect_error(chi_dev_to_prod(table_name = NULL), "`table_name` is required")
})

test_that("table_name must be a single string", {
  expect_error(chi_dev_to_prod(table_name = c("birth", "death")), "single character string")
  expect_error(chi_dev_to_prod(table_name = 123), "single character string")
})

test_that("exclude_keys rejects non-character input", {
  expect_error(chi_dev_to_prod(table_name = "birth", exclude_keys = 1:3), "character vector or NULL")
})

test_that("exclude_keys rejects NA or empty strings", {
  expect_error(chi_dev_to_prod(table_name = "birth", exclude_keys = c("a", NA)), "NA or empty")
  expect_error(chi_dev_to_prod(table_name = "birth", exclude_keys = c("a", "")), "NA or empty")
  expect_error(chi_dev_to_prod(table_name = "birth", exclude_keys = c("a", "  ")), "NA or empty")
})

test_that("exact_match must be a single logical", {
  expect_error(chi_dev_to_prod(table_name = "birth", exact_match = "yes"), "single logical")
  expect_error(chi_dev_to_prod(table_name = "birth", exact_match = c(TRUE, FALSE)), "single logical")
})

test_that("confirm must be a single logical", {
  expect_error(chi_dev_to_prod(table_name = "birth", confirm = "y"), "single logical")
  expect_error(chi_dev_to_prod(table_name = "birth", confirm = c(TRUE, TRUE)), "single logical")
})

# Test filtering logic (maybe not useful because dependent on the logic remaining the same in the function) ----
test_that("grepl exclusion logic works as expected", {
  all_keys <- c("uninsured_adult", "uninsured_child", "poverty", "housing")
  pattern  <- paste(c("uninsure"), collapse = "|")
  excluded <- all_keys[grepl(pattern, all_keys)]
  expect_equal(sort(excluded), c("uninsured_adult", "uninsured_child"))
  expect_equal(sort(setdiff(all_keys, excluded)), c("housing", "poverty"))
})

test_that("exact match exclusion logic works as expected", {
  all_keys     <- c("uninsured_adult", "uninsured_child", "poverty")
  exclude_keys <- c("uninsured_adult", "poverty", "not_present")
  excluded     <- intersect(exclude_keys, all_keys)
  expect_equal(sort(excluded), c("poverty", "uninsured_adult"))
  expect_equal(setdiff(all_keys, excluded), "uninsured_child")
})

# No actual function testing because mocking odbc connections is miserable ----
# I manually tested the functions extensively with the acs results and metadata
# acs is a good test case because the uninsured is separate from the rest of the
# indicator keys
