library('data.table')
library('testthat')

# create test data ----
set.seed(98104)
dt <- suppressWarnings(data.table::data.table(chi_year = 2022,
                                              indicator = "home ownership",
                                              team = sample(as.vector(outer(letters, 1:15, paste0)), rep = T),
                                              color = c("red", "blue", "yellow", "green"),
                                              numerator = sample(1:200, 1000, rep = TRUE)))
dt[, denominator := sample(500:1000, 1000, rep = TRUE)]
dt[, result := numerator / denominator]
dt[, se := sqrt(result/100)] # not a real formula!
dt[, lower_bound := result - (1.96 * se)]
dt[, upper_bound := result + (1.96 * se)]
dt[, rse := 100*se / result]
setorder(dt, indicator, team, color, numerator)
dt[, counter := 1:.N, c("indicator", "team", "color")]
dt <- dt[counter == 1][, counter := NULL]

# test defaults ----
dt1 <- chi_suppress_results(dt)
test_that('Check that defaults work as expected',{
  expect_equal(nrow(dt1[suppression=="^"]), nrow(dt[numerator %in% 1:9 | denominator %in% 1:9]))
  expect_equal(nrow(dt1[suppression=="^"]), nrow(dt1[is.na(result)]))
  expect_equal(nrow(dt1[suppression=="^"]), nrow(dt1[is.na(se)]))
  expect_equal(nrow(dt1[suppression=="^"]), nrow(dt1[is.na(lower_bound)]))
  expect_equal(nrow(dt1[suppression=="^"]), nrow(dt1[is.na(rse)]))
  expect_equal(nrow(dt1[caution=="!"]), nrow(dt[!(numerator %in% 1:9 | denominator %in% 1:9) & (rse >=30 | numerator == 0)]))
})

# test suppression range ----
dt2 <- chi_suppress_results(dt, suppress_range = c(0,10), secondary = FALSE)
test_that('Check that the suppression_range argument works',{
  expect_equal(nrow(dt2[suppression=="^"]), nrow(dt[numerator <= 10 | denominator <= 10]))
  expect_equal(nrow(dt2[suppression=="^"]), nrow(dt2[is.na(result)]))
  expect_equal(nrow(dt2[suppression=="^"]), nrow(dt2[is.na(se)]))
  expect_equal(nrow(dt2[suppression=="^"]), nrow(dt2[is.na(lower_bound)]))
  expect_equal(nrow(dt2[suppression=="^"]), nrow(dt2[is.na(rse)]))
  expect_equal(nrow(dt2[caution=="!"]), nrow(dt[!(numerator %in% 0:10 | denominator %in% 0:10) & (rse >=30)]))
})

# test secondary suppression ----
dt3 <- chi_suppress_results(dt, suppress_range = c(0,10),
                            secondary = TRUE,
                            secondary_ids = c("indicator", "team"))
#ugly manual method to apply secondary suppression for comparison
sec.suppress3 <- copy(dt2) # build off results from initial / primary suppression
sec.suppress3[, max.grp.rows := .N, .(indicator, team)] # num of rows per set of secondary_ids
sec.suppress3[, group := .GRP, by = .(indicator, team)] # create group id for each set of secondary_ids
supp.ids <- unique(sec.suppress3[suppression=="^"]$group) # get group ids where there was initial suppression
sec.suppress3[, suppressed.group := F]
sec.suppress3[group %in% supp.ids, suppressed.group := T] # identify groups with initial suppression in table
sec.suppress3[group %in% supp.ids & is.na(suppression), unsuppressed := .N, .(indicator, team)] # rows unsuppressed per group
suppressWarnings(sec.suppress3[, unsuppressed := max(unsuppressed, na.rm = T), .(indicator, team)]) # fill in NA for rows unsuppressed
sec.suppress3[is.na(suppression) & unsuppressed == max.grp.rows - 1, secondary.suppression := T] # identify groups that need secondary suppression (groups with exactly one suppressed row)
setorder(sec.suppress3, group, numerator, na.last = T) # sort from smallest to largest numerator by group
sec.suppress3[secondary.suppression == T, order := 1:.N, group] # identify 1st row (smallest numerator) of each group needing secondary suppression
sec.suppress3[order==1, suppression := "^"] # mark the specific rows to have secondary suppression
sec.suppress3[suppression == "^", c("numerator", "denominator", "result", "se", "lower_bound", "upper_bound", "rse", "caution") := NA]
sec.suppress3[, c("max.grp.rows", "group", "suppressed.group", "unsuppressed", "secondary.suppression", "order") := NULL]

test_that('Check that secondary suppression works',{
  expect_equal(nrow(dt3[suppression=="^"]), nrow(sec.suppress3[suppression=="^"]))
  expect_equal(nrow(dt3[suppression=="^"]), nrow(dt3[is.na(result)]))
  expect_equal(nrow(dt3[suppression=="^"]), nrow(dt3[is.na(se)]))
  expect_equal(nrow(dt3[suppression=="^"]), nrow(dt3[is.na(lower_bound)]))
  expect_equal(nrow(dt3[suppression=="^"]), nrow(dt3[is.na(rse)]))
})

# test NA handling in numerator and demominator ----
dt_na <- copy(dt)
dt_na[1:10, numerator := NA]
dt_na[11:20, denominator := NA]
dt_na_result <- chi_suppress_results(dt_na)
test_that('Check NA handling in numerator and denominator', {
  # Check whether NAs are properly handled and not incorrectly flagged
  expect_equal(sum(is.na(dt_na_result[1:20]$suppression)), 20)
})

# test missing numerator column ----
dt_no_num <- copy(dt)[, numerator := NULL]
test_that('Check error when numerator column is missing', {
  expect_error(chi_suppress_results(dt_no_num), "Required column 'numerator' is missing")
})

# test secondary suppression group ----
dt_single <- data.table(
  chi_year = 2022,
  indicator = c("A", "A", "B"),
  team = c("team1", "team2", "team1"),
  numerator = c(5, 15, 20),
  denominator = c(100, 100, 100)
)
dt_single[, result := numerator / denominator]
dt_single[, rse := 20]

dt_single_result <- suppressWarnings(chi_suppress_results(dt_single,
                                                          secondary = TRUE,
                                                          secondary_ids = c("indicator")))

test_that('Check secondary suppression with single row group', {
  # Group A should be entirely suppressed and
  # Group B shoudl have one unsuppressed row
  expect_equal(nrow(dt_single_result[indicator == "A" & suppression=='^']), 2)
  expect_equal(nrow(dt_single_result[indicator == "B" & is.na(suppression)]), 1)
})

# test secondary suppression with secondary_exclude ----
dt4 <- chi_suppress_results(dt, suppress_range = c(0,10),
                            secondary = TRUE,
                            secondary_ids = c("indicator", "team"),
                            secondary_exclude = !team %in% c('a10', 'a11'))

#ugly manual method to apply secondary suppression for testing
exclusion4 <- copy(dt2)[team %in% c('a10', 'a11')] # partition off part excluded from secondary suppression
sec.suppress4 <- copy(dt2)[!team %in% c('a10', 'a11')] # build off results from initial / primary suppression
sec.suppress4[, max.grp.rows := .N, .(indicator, team)] # num of rows per set of secondary_ids
sec.suppress4[, group := .GRP, by = .(indicator, team)] # create group id for each set of secondary_ids
supp.ids <- unique(sec.suppress4[suppression=="^"]$group) # get group ids where there was initial suppression
sec.suppress4[, suppressed.group := F]
sec.suppress4[group %in% supp.ids, suppressed.group := T] # identify groups with initial suppression in table
sec.suppress4[group %in% supp.ids & is.na(suppression), unsuppressed := .N, .(indicator, team)] # rows unsuppressed per group
suppressWarnings(sec.suppress4[, unsuppressed := max(unsuppressed, na.rm = T), .(indicator, team)]) # fill in NA for rows unsuppressed
sec.suppress4[is.na(suppression) & unsuppressed == max.grp.rows - 1, secondary.suppression := T] # identify groups that need secondary suppression (groups with exactly one suppressed row)
setorder(sec.suppress4, group, numerator, na.last = T) # sort from smallest to largest numerator by group
sec.suppress4[secondary.suppression == T, order := 1:.N, group] # identify 1st row (smallest numerator) of each group needing secondary suppression
sec.suppress4[order==1, suppression := "^"] # mark the specific rows to have secondary suppression
sec.suppress4[suppression == "^", c("numerator", "denominator", "result", "se", "lower_bound", "upper_bound", "rse", "caution") := NA]
sec.suppress4[, c("max.grp.rows", "group", "suppressed.group", "unsuppressed", "secondary.suppression", "order") := NULL]
sec.suppress4 <- rbind(sec.suppress4, exclusion4)

test_that('Check that secondary suppression with exclusion works',{
  expect_equal(nrow(dt4[suppression=="^"]), nrow(sec.suppress4[suppression=="^"]))
  expect_equal(nrow(dt4[suppression=="^"]), nrow(dt4[is.na(result)]))
  expect_equal(nrow(dt4[suppression=="^"]), nrow(dt4[is.na(se)]))
  expect_equal(nrow(dt4[suppression=="^"]), nrow(dt4[is.na(lower_bound)]))
  expect_equal(nrow(dt4[suppression=="^"]), nrow(dt4[is.na(rse)]))
})

# test flag_only ----
dt5 <- chi_suppress_results(dt, flag_only = TRUE)
test_that('Check that flag_only works',{
  expect_equal(nrow(dt5[suppression=="^"]), nrow(dt[numerator <= 9 | denominator <= 9]))
  expect_equal(0, nrow(dt5[is.na(result)]))
  expect_equal(0, nrow(dt5[is.na(se)]))
  expect_equal(0, nrow(dt5[is.na(lower_bound)]))
  expect_equal(0, nrow(dt5[is.na(rse)]))
  expect_equal(nrow(dt5[caution=="!"]), nrow(dt[rse >=30 | numerator == 0]))
})

# test secondary_exclude when character and unquoted expression ----
test_that('Check that the same results are returned whether or not quoted',{
  expect_warning(dt6 <- chi_suppress_results(dt, secondary_exclude = "team %like% '^a|^b|^c|^d'"))
  dt7 <- chi_suppress_results(dt, secondary_exclude = team %like% '^a|^b|^c|^d')
  expect_identical(dt6, dt7)
})

# test custom column names ----
dt_custom <- copy(dt)
setnames(dt_custom,
         old = c("numerator", "denominator", "result", "rse"),
         new = c("num", "denom", "value", "rel_error"))

test_that('Check that custom column names work correctly', {
  dt_custom_result <- chi_suppress_results(
    dt_custom,
    numerator_col = "num",
    denominator_col = "denom",
    rse_col = "rel_error",
    columns_to_suppress = c("value", "se", "lower_bound", "upper_bound", "num", "denom")
  )

  expect_equal(nrow(dt_custom_result[suppression=="^"]),
               nrow(dt_custom[num <= 9 | denom <= 9]))
  expect_equal(nrow(dt_custom_result[caution=="!"]),
               nrow(dt_custom[rel_error >= 30 | num == 0]))
})

# test missing columns handling ----
dt_missing <- copy(dt)[, rse := NULL]

test_that("Check that missing rse column is handled properly", {
  warnings <- character()
  dt_missing_result <- withCallingHandlers( # needed because will produce two warnings
    chi_suppress_results(dt_missing),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")  # prevents printing the warning
    }
  )

  expect_true(any(grepl("Column 'rse', the value of `rse_col`, is missing", warnings)))
  expect_true(any(grepl("columns specified in 'columns_to_suppress' are missing", warnings)))
  expect_false("caution" %in% names(dt_missing_result))
})

# test non-existent columns in columns_to_suppress ----
test_that('Check handling of non-existent columns in columns_to_suppress', {
  expect_warning(
    dt_cols <- chi_suppress_results(
      dt,
      columns_to_suppress = c("result", "non_existent_column", "another_missing")
    ),
    "The following columns specified in 'columns_to_suppress' are missing from the dataset"
  )

  # Should only suppress existing columns
  expect_true(all(is.na(dt_cols[suppression=="^"]$result)))
})

# test existing suppression column ----
dt_exist <- copy(dt)
dt_exist[, suppression := "old"]

test_that('Check that existing suppression column is overwritten with warning', {
  expect_warning(
    dt_exist_result <- chi_suppress_results(dt_exist),
    "Existing 'suppression' column will be overwritten"
  )

  # Should have overwritten the old suppression column
  expect_false(nrow(dt_exist_result[suppression == "old"]) > 0)
})
