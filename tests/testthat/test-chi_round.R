# Tests for chi_round

# Create test data ----
  # One row of unrounded estimates. Values are chosen so each rounding standard
  # gives a distinct, easy to verify answer.
  make_estimates <- function(n = 1) {
    data.table::data.table(
      indicator_key = paste0("indicator", seq_len(n)),
      result      = rep(12.345678, n),
      lower_bound = rep(10.555555, n),
      upper_bound = rep(14.444444, n),
      se          = rep(1.23456789, n),
      rse         = rep(9.87654321, n)
    )
  }

# Rounding standards for each result_type ----
  test_that("proportions are rounded to 3 (result & CI), 4 (se), and 3 (rse) decimals", {
    out <- chi_round(make_estimates(), result_type = "proportion")
    expect_equal(out$result, 12.346)
    expect_equal(out$lower_bound, 10.556)
    expect_equal(out$upper_bound, 14.444)
    expect_equal(out$se, 1.2346)
    expect_equal(out$rse, 9.877)
  })

  test_that("rates are rounded to 1 (result & CI), 2 (se), and 3 (rse) decimals", {
    out <- chi_round(make_estimates(), result_type = "rate")
    expect_equal(out$result, 12.3)
    expect_equal(out$lower_bound, 10.6)
    expect_equal(out$upper_bound, 14.4)
    expect_equal(out$se, 1.23)
    expect_equal(out$rse, 9.877)
  })

  test_that("counts, dollars, & integers are rounded to 0 (result & CI), 1 (se), and 3 (rse) decimals", {
    for (type in c("count", "dollars", "integer")) {
      out <- chi_round(make_estimates(), result_type = type)
      expect_equal(out$result, 12)
      expect_equal(out$lower_bound, 11)
      expect_equal(out$upper_bound, 14)
      expect_equal(out$se, 1.2)
      expect_equal(out$rse, 9.877)
    }
  })

  test_that("halves are rounded up (rads::round2), not to the nearest even number (base::round)", {
    dt <- data.table::data.table(result = c(4.5, 5.5, 0.5))
    out <- chi_round(dt, result_type = "count")
    expect_equal(out$result, c(5, 6, 1))
  })

  test_that("NA values remain NA and other columns are unchanged", {
    dt <- make_estimates(2)
    dt[1, se := NA_real_]
    dt[, other_col := c(1.23456, 6.54321)]
    out <- chi_round(dt, result_type = "proportion")
    expect_true(is.na(out[1]$se))
    expect_equal(out[2]$se, 1.2346)
    expect_identical(out$other_col, dt$other_col)
    expect_identical(out$indicator_key, dt$indicator_key)
  })

  test_that("only the standard columns that exist are rounded", {
    dt <- data.table::data.table(result = 0.123456)
    out <- chi_round(dt, result_type = "proportion")
    expect_equal(out$result, 0.123)
    expect_identical(names(out), "result")
  })

# How result_type is determined ----
  test_that("a result_type column in ph.data is used row by row", {
    dt <- make_estimates(3)
    dt[, result_type := c("proportion", "rate", "dollars")]
    out <- chi_round(dt)
    expect_equal(out$result, c(12.346, 12.3, 12))
    expect_equal(out$se, c(1.2346, 1.23, 1.2))
    expect_equal(out$rse, rep(9.877, 3))
  })

  test_that("metadata is used to look up result_type by indicator_key", {
    dt <- make_estimates(3)
    dt <- rbind(dt, dt[1]) # indicator1 appears twice
    meta <- data.table::data.table(
      indicator_key = c("indicator1", "indicator2", "indicator3"),
      result_type = c("rate", "proportion", "count"),
      other_info = "ignored"
    )
    out <- chi_round(dt, metadata = meta)
    expect_equal(out$result, c(12.3, 12.346, 12, 12.3))
    # metadata columns are not added to the output
    expect_identical(names(out), names(dt))
  })

  test_that("the result_type argument overrides a result_type column, with a warning if they differ", {
    dt <- make_estimates(2)
    dt[, result_type := c("proportion", "rate")]
    expect_warning(out <- chi_round(dt, result_type = "count"), "differ from the result_type argument")
    expect_equal(out$result, c(12, 12))
    # the result_type column itself is not changed
    expect_identical(out$result_type, c("proportion", "rate"))

    # no warning when they agree
    dt[, result_type := "count"]
    expect_no_warning(chi_round(dt, result_type = "count"))
  })

  test_that("the result_type argument is used even when metadata is provided", {
    meta <- data.table::data.table(indicator_key = "indicator1", result_type = "rate")
    out <- chi_round(make_estimates(), result_type = "proportion", metadata = meta)
    expect_equal(out$result, 12.346)
  })

  test_that("result_type values are not case sensitive", {
    out <- chi_round(make_estimates(), result_type = " Rate ")
    expect_equal(out$result, 12.3)
  })

# The input data is not modified ----
  test_that("ph.data is not modified by reference and data.frames are accepted", {
    dt <- make_estimates()
    dt_before <- data.table::copy(dt)
    out <- chi_round(dt, result_type = "proportion")
    expect_identical(dt, dt_before)

    df <- as.data.frame(make_estimates())
    out_df <- chi_round(df, result_type = "proportion")
    expect_true(data.table::is.data.table(out_df))
    expect_false(data.table::is.data.table(df))
    expect_equal(out_df$result, 12.346)
  })

  test_that("a table with zero rows is returned without error", {
    out <- chi_round(make_estimates()[0], result_type = "proportion")
    expect_equal(nrow(out), 0)
  })

# Errors ----
  test_that("invalid ph.data gives an error", {
    expect_error(chi_round(), "must be a data.table or data.frame")
    expect_error(chi_round(list(result = 1), result_type = "rate"), "must be a data.table or data.frame")
    expect_error(chi_round(data.table::data.table(x = 1), result_type = "rate"), "at least one of these columns")
    expect_error(chi_round(data.table::data.table(result = "1"), result_type = "rate"), "must be numeric")
  })

  test_that("invalid result_type argument gives an error", {
    expect_error(chi_round(make_estimates(), result_type = "percent"), "must be one of")
    expect_error(chi_round(make_estimates(), result_type = c("rate", "proportion")), "single character value")
    expect_error(chi_round(make_estimates(), result_type = 1), "single character value")
  })

  test_that("an error is given when result_type cannot be determined", {
    expect_error(chi_round(make_estimates()), "Cannot determine the result_type")
  })

  test_that("missing or unrecognized result_type values give an error that names the indicator_key", {
    dt <- make_estimates(3)
    dt[, result_type := c("proportion", NA, "percent")]
    expect_error(chi_round(dt), "2 row\\(s\\) have a missing or unrecognized result_type.*indicator2, indicator3")

    # indicator_key missing from metadata
    meta <- data.table::data.table(indicator_key = c("indicator1", "indicator2"),
                                   result_type = c("rate", "rate"))
    expect_error(chi_round(make_estimates(3), metadata = meta), "indicator_key: indicator3")
  })

  test_that("invalid metadata gives an error", {
    expect_error(chi_round(make_estimates(), metadata = list(a = 1)), "metadata must be a data.table")
    expect_error(chi_round(make_estimates(), metadata = data.table::data.table(indicator_key = "indicator1")),
                 "must contain the columns indicator_key and result_type")
    expect_error(chi_round(make_estimates()[, !"indicator_key"],
                           metadata = data.table::data.table(indicator_key = "indicator1", result_type = "rate")),
                 "must contain an indicator_key column")
    expect_error(chi_round(make_estimates(),
                           metadata = data.table::data.table(indicator_key = c("indicator1", "indicator1"),
                                                             result_type = c("rate", "proportion"))),
                 "more than one result_type")
  })

# Consistency with chi_qa_tro() ----
  test_that("chi_round() output passes the rounding checks used by chi_qa_tro()", {
    dt <- make_estimates(5)
    dt[, result_type := c("proportion", "rate", "count", "dollars", "integer")]
    out <- chi_round(dt)
    # same checks as in chi_qa_tro()
    expect_true(all(out[result_type == "proportion", result == rads::round2(result, 3) & se == rads::round2(se, 4)]))
    expect_true(all(out[result_type == "rate", result == rads::round2(result, 1) & se == rads::round2(se, 2)]))
    expect_true(all(out[result_type %in% c("count", "dollars", "integer"), result == rads::round2(result, 0) & se == rads::round2(se, 1)]))
    expect_true(all(out[, rse == rads::round2(rse, 3)]))
  })
