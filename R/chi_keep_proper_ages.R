# chi_keep_proper_ages ----
#' Keep data for appropriate ages only
#'
#' @description
#' Internal function to filter data based on age groups in cat1_group and cat2_group,
#' preserving only the rows where chi_age falls within the implied age ranges of
#' any age groups in the data.
#'
#' @param ph.data A data.table containing the data to be filtered. Should have the
#' following columns: cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group,
#' indicator_key, year, chi_age, tab -- all of the appropriate classes.
#'
#' @return A filtered data.table with only the rows where chi_age is compatible
#' with the age ranges implied by age groups.
#'
#' @details
#' When \code{cat# == 'Age'}, the function parses \code{cat#_group} labels (e.g.,
#' "<18", "18-24", "75+") to determine minimum and maximum ages, then filters the
#' data accordingly. It handles age groups in both cat1 and cat2 categories.
#'
#' @importFrom data.table is.data.table as.data.table rbindlist setnames data.table
#'
#' @examples
#' library(data.table)
#'
#' # Make test data with all combinations of categories and age
#' test_data <- CJ(
#'   cat1 = "Gender",
#'   cat1_varname = "chi_sex",
#'   cat1_group = c("Female", "Male"),
#'   cat2 = "Age",
#'   cat2_varname = "yage4",
#'   cat2_group = c("0-4", "5-9", "10-14"),
#'   chi_age = 1:20,
#'   unique = TRUE
#' )
#'
#' # Add extra required columns
#' test_data[, `:=`(
#'   indicator_key = "indicator1",
#'   year = "2023",
#'   tab = "mytab",
#'   count = 1
#' )]
#'
#' # Example usage
#' filtered_data <- chi_keep_proper_ages(test_data)
#' filtered_data[cat2_group == "10-14", unique(chi_age)]
#'
#' @export
chi_keep_proper_ages <- function(ph.data = NULL) {
  # Validate inputs
  if (is.null(ph.data)) {
    stop("\n\U1F6D1 ph.data must be provided")
  }

  if (!data.table::is.data.table(ph.data)) {
    ph.data <- data.table::as.data.table(ph.data)
  }

  # Check for required columns
  required_columns <- c("cat2_group", "cat1_group", "indicator_key", "year",
                        "chi_age", "tab", "cat1", "cat1_varname",
                        "cat2", "cat2_varname")

  missing_columns <- required_columns[!required_columns %in% names(ph.data)]
  if (length(missing_columns) > 0) {
    stop(paste0("\n\U1F6D1 ph.data is missing required columns: ",
                paste(missing_columns, collapse = ", ")))
  }

  # Check expected column classes
  expected_classes <- list(
    cat1 = c("character", "factor"),
    cat1_varname = c("character", "factor"),
    cat1_group = c("character", "factor"),
    cat2 = c("character", "factor"),
    cat2_varname = c("character", "factor"),
    cat2_group = c("character", "factor"),
    indicator_key = c("character", "factor"),
    year = "character",
    chi_age = c("numeric", "integer"),
    tab = c("character", "factor")
  )

  for (col_name in names(expected_classes)) {
    expected <- expected_classes[[col_name]]
    valid_class <- FALSE

    # Check if column inherits from any of the expected classes
    for (expected_class in expected) {
      if (inherits(ph.data[[col_name]], expected_class)) {
        valid_class <- TRUE
        break # once a valid class is established, no need to keep checking for that col_name
      }
    }

    if (isFALSE(valid_class)) {
      expected_str <- paste(expected, collapse = " or ")
      actual_class <- class(ph.data[[col_name]])[1]  # Get primary class
      stop(paste0("\n\U1F6D1 Column '", col_name, "' should be of class '",
                  expected_str, "', but is '", actual_class, "'"))
    }
  }

  # Make function used in lapply below to get min and max age from age group
  parse_age_group <- function(group) {

    group <- gsub(" ", "", group)
    if (grepl("^<[0-9]+$", group)) {
      return(list(c(NA, as.numeric(sub("<", "", group)) - 1)))
    } else if (grepl("^[0-9]+-[0-9]+$", group)) {
      return(list(as.numeric(unlist(strsplit(group, "-")))))
    } else if (grepl("^[0-9]+\\+$", group)) {
      return(list(c(as.numeric(sub("\\+", "", group)), NA)))
    } else {
      return(list(c(NA, NA)))
    }
  }


  # Create reference table of age groups and corresponding min_age and max_age
  all_groups <- unique(c(ph.data$cat1_group[ph.data$cat1 == "Age"],
                         ph.data$cat2_group[ph.data$cat2 == "Age"]))
  all_bounds <- rbindlist(lapply(all_groups, function(mygroup) {
    bounds <- parse_age_group(mygroup)[[1]]
    data.table(age_group = mygroup, min_age = bounds[1], max_age = bounds[2])
  }))

  # Merge cat1 bounds
  setnames(all_bounds, c("age_group", "min_age", "max_age"), c("cat1_group", "min1", "max1"))
  ph.data <- merge(ph.data, all_bounds, by = "cat1_group", all.x = TRUE, sort = FALSE)

  # Merge cat2 bounds
  setnames(all_bounds, c("cat1_group", "min1", "max1"), c("cat2_group", "min2", "max2"))
  ph.data <- merge(ph.data, all_bounds, by = "cat2_group", all.x = TRUE, sort = FALSE)

  # Create filter to identify rows to drop
  ph.data[, keepme := TRUE]

  ph.data[cat1 == "Age", keepme := keepme &
            (is.na(min1) | chi_age >= min1) & (is.na(max1) | chi_age <= max1)]
  ph.data[cat2 == "Age", keepme := keepme &
            (is.na(min2) | chi_age >= min2) & (is.na(max2) | chi_age <= max2)]

  # Clean up and return
  return(ph.data[keepme == TRUE][, c("keepme", "min1", "max1", "min2", "max2") := NULL])
}
