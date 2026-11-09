# chi_round ----
#' Round CHI Estimates According to CHI Standards
#'
#' @description
#' Rounds the `result`, `lower_bound`, `upper_bound`, `se`, and `rse` columns
#' of CHI estimates according to the CHI rounding standards. The number of
#' decimal places depends on the `result_type` of each row (e.g., proportions
#' and rates are rounded differently).
#'
#' All rounding uses [rads::round2()], which rounds halves away from zero
#' (e.g., 4.5 becomes 5), as public health expects.
#'
#' This function replaces the deprecated `rads::digits()`.
#'
#' @details
#' **Rounding standards (number of decimal places)**
#'
#' | result_type             | result, lower_bound, upper_bound | se | rse |
#' |:------------------------|:--------------------------------:|:--:|:---:|
#' | proportion              | 3                                | 4  | 3   |
#' | rate                    | 1                                | 2  | 3   |
#' | count, dollars, integer | 0                                | 1  | 3   |
#'
#' These are the same standards that [chi_qa_tro()] checks.
#'
#' **How the `result_type` of each row is determined**
#'
#' Because a compiled CHI table can contain many `indicator_key` values, each
#' measured differently (e.g., proportions, rates, and dollars in a single
#' table), the `result_type` is identified for each row, in this order:
#' 1. If the `result_type` argument is specified, it is used for every row.
#'    This overrides any `result_type` column in `ph.data` (with a warning if
#'    they disagree).
#' 2. Otherwise, if `ph.data` has a `result_type` column, each row is rounded
#'    according to its own `result_type`.
#' 3. Otherwise, if `metadata` is provided, each row's `result_type` is looked
#'    up in `metadata` by `indicator_key`.
#' 4. Otherwise, the function stops with an error.
#'
#' The function never guesses the `result_type`!
#'
#' Only the five standard columns are rounded, and only those present in
#' `ph.data`. All other columns are returned unchanged.
#'
#' @param ph.data A data.table or data.frame of CHI estimates. Must contain at
#' least one of `result`, `lower_bound`, `upper_bound`, `se`, or `rse`.
#' @param result_type Optional character vector of length one. One of
#' `"proportion"`, `"rate"`, `"count"`, `"dollars"`, or `"integer"`. When
#' specified, it is applied to every row of `ph.data`.
#'
#' Default: `result_type = NULL`, which means the `result_type` is taken from
#' `ph.data` or `metadata` (see Details).
#' @param metadata Optional data.table or data.frame of CHI metadata, with
#' (at least) the columns `indicator_key` and `result_type`. It is only used
#' when `result_type = NULL` and `ph.data` does not have a `result_type`
#' column. When used, `ph.data` must have an `indicator_key` column.
#'
#' Default: `metadata = NULL`
#'
#' @return Returns a rounded copy of `ph.data` as a data.table. The input data
#' is not modified.
#'
#' @seealso
#' [chi_calc()] which uses this function to round its estimates
#'
#' [chi_qa_tro()] which checks that estimates are rounded correctly
#'
#' [rads::round2()] for the underlying `rads` rounding function
#'
#' @examples
#' \dontrun{
#' # One result_type for the entire table
#' props <- data.table::data.table(
#'   indicator_key = "indicator1",
#'   result = 0.123456,
#'   lower_bound = 0.100049,
#'   upper_bound = 0.150051,
#'   se = 0.0123456,
#'   rse = 10.00049
#' )
#' chi_round(ph.data = props, result_type = "proportion")
#'
#' # A compiled table with different result_types, using metadata
#' compiled <- data.table::data.table(
#'   indicator_key = c("indicator1", "indicator2", "indicator3"),
#'   result = c(0.123456, 123.456, 98765.4321),
#'   se = c(0.0123456, 12.3456, 987.654321)
#' )
#' my_metadata <- data.table::data.table(
#'   indicator_key = c("indicator1", "indicator2", "indicator3"),
#'   result_type = c("proportion", "rate", "dollars")
#' )
#' chi_round(ph.data = compiled, metadata = my_metadata)
#' }
#'
#' @export
#'
chi_round <- function(ph.data,
                      result_type = NULL,
                      metadata = NULL) {

  # Rounding standards ----
    # Number of decimal places for each result_type. Edit here if CHI standards change.
    rounding_rules <- data.table::data.table(
      result_type     = c("proportion", "rate", "count", "dollars", "integer"),
      digits_estimate = c(3,            1,      0,       0,         0), # result, lower_bound, upper_bound
      digits_se       = c(4,            2,      1,       1,         1),
      digits_rse      = c(3,            3,      3,       3,         3)
    )
    valid_types <- rounding_rules$result_type

  # Validate ph.data ----
    if (missing(ph.data) || !is.data.frame(ph.data)) {
      stop("\n\U1F6D1 ph.data must be a data.table or data.frame")
    }

    # Work on a copy so the user's data is not modified by reference
    ph.data <- data.table::setDT(data.table::copy(ph.data))

    # Identify which of the standard columns are present
    estimate_cols <- intersect(c("result", "lower_bound", "upper_bound"), names(ph.data))
    round_cols <- intersect(c(estimate_cols, "se", "rse"), names(ph.data))

    if (length(round_cols) == 0) {
      stop("\n\U1F6D1 ph.data must contain at least one of these columns: ",
           "result, lower_bound, upper_bound, se, rse")
    }

    # All columns to be rounded must be numeric
    not_numeric <- round_cols[!vapply(ph.data[, round_cols, with = FALSE], is.numeric, logical(1))]
    if (length(not_numeric) > 0) {
      stop("\n\U1F6D1 The following columns must be numeric: ", paste(not_numeric, collapse = ", "))
    }

    # Nothing to round in an empty table
    if (nrow(ph.data) == 0) {
      return(ph.data)
    }

  # Determine the result_type of every row ----
    # row_types is a character vector with one value per row of ph.data
    if (!is.null(result_type)) {
      # Option 1: the result_type argument applies to every row
      if (!is.character(result_type) || length(result_type) != 1 || is.na(result_type)) {
        stop("\n\U1F6D1 result_type must be a single character value, e.g., 'proportion'")
      }
      result_type <- tolower(trimws(result_type))
      if (!result_type %in% valid_types) {
        stop("\n\U1F6D1 result_type must be one of: ", paste(valid_types, collapse = ", "))
      }

      # Warn if it disagrees with an existing result_type column
      if ("result_type" %in% names(ph.data)) {
        existing_types <- unique(tolower(trimws(stats::na.omit(ph.data$result_type))))
        if (any(existing_types != result_type)) {
          warning("\n\u26A0\ufe0f ph.data has a result_type column with values (",
                  paste(existing_types, collapse = ", "),
                  ") that differ from the result_type argument ('", result_type, "').\n",
                  "Every row has been rounded as '", result_type, "'.\n",
                  "To round each row by its own result_type, set result_type = NULL.")
        }
      }

      row_types <- rep(result_type, nrow(ph.data))

    } else if ("result_type" %in% names(ph.data)) {
      # Option 2: use the result_type column in ph.data
      row_types <- tolower(trimws(ph.data$result_type))

    } else if (!is.null(metadata)) {
      # Option 3: look up the result_type in metadata by indicator_key
      if (!is.data.frame(metadata)) {
        stop("\n\U1F6D1 metadata must be a data.table or data.frame")
      }
      if (!all(c("indicator_key", "result_type") %in% names(metadata))) {
        stop("\n\U1F6D1 metadata must contain the columns indicator_key and result_type")
      }
      if (!"indicator_key" %in% names(ph.data)) {
        stop("\n\U1F6D1 ph.data must contain an indicator_key column to use metadata")
      }

      # Keep one row per indicator_key / result_type
      meta_lookup <- unique(data.table::as.data.table(metadata)[, list(indicator_key, result_type)])

      # Each indicator_key must have exactly one result_type
      dup_keys <- meta_lookup[duplicated(indicator_key)]$indicator_key
      if (length(dup_keys) > 0) {
        stop("\n\U1F6D1 metadata has more than one result_type for these indicator_key values: ",
             paste(unique(dup_keys), collapse = ", "),
             "\nEach indicator_key should correspond to exactly one unique result_type.")
      }

      # Find the corresponding metadata row for each indicator_key.
      meta_rows <- match(ph.data$indicator_key, meta_lookup$indicator_key)

      # Fetch and standardize the corresponding result_type.
      row_types <- tolower(trimws(meta_lookup$result_type[meta_rows]))


    } else {
      # Option 4: no way to know the result_type
      stop("\n\U1F6D1 Cannot determine the result_type. Please do one of the following:\n",
           "  - specify the result_type argument (e.g., result_type = 'proportion')\n",
           "  - add a result_type column to ph.data\n",
           "  - provide metadata with indicator_key and result_type columns")
    }

  # Confirm every row has a valid result_type ----
    bad_rows <- which(is.na(row_types) | !row_types %in% valid_types)
    if (length(bad_rows) > 0) {
      # Identify the problem rows as helpfully as possible
      if ("indicator_key" %in% names(ph.data)) {
        problem_ids <- paste0("indicator_key: ", paste(unique(ph.data$indicator_key[bad_rows]), collapse = ", "))
      } else {
        problem_ids <- paste0("row numbers: ", paste(utils::head(bad_rows, 20), collapse = ", "),
                              if (length(bad_rows) > 20) ", ..." else "")
      }
      stop("\n\U1F6D1 ", length(bad_rows), " row(s) have a missing or unrecognized result_type (",
           problem_ids, ").\n",
           "Valid result_type values are: ", paste(valid_types, collapse = ", "))
    }

  # Round ----
    # Look up the rounding rule for every row of ph.data (one rule row per data row).
    # rads::round2() accepts a vector of digits (`n`), so each value is rounded
    # according to the result_type of its own row.
    row_rules <- rounding_rules[match(row_types, rounding_rules$result_type)]

    # result, lower_bound, & upper_bound (only those present)
    if (length(estimate_cols) > 0) {
      ph.data[, (estimate_cols) := lapply(.SD, rads::round2, n = row_rules$digits_estimate), # this works because `n` is vectorized
              .SDcols = estimate_cols]
    }

    # se & rse (only if present)
    if ("se" %in% names(ph.data))  ph.data[, se  := rads::round2(se,  n = row_rules$digits_se)]
    if ("rse" %in% names(ph.data)) ph.data[, rse := rads::round2(rse, n = row_rules$digits_rse)]

  # Return ----
    return(ph.data)
}
