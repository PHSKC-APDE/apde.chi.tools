# chi_drop_illogical_ages ----
#' Filter Out Age Values That Don't Match Their Corresponding Age Group Categories
#'
#' @description
#' This function filters a data.table to remove rows with incompatible / illogical
#' age values. It compares the single year age value (specified by \code{agevar})
#' against age ranges defined in \code{cat1_group} and \code{cat2_group} columns,
#' keeping only those rows where:
#' \itemize{
#'   \item The categorical variable is not age-related, OR
#'   \item The age value falls within the range specified by the corresponding age group
#' }
#'
#' Age groups are expected to be in formats like "10-17", "<5", or "45+", which
#' the function automatically parses into numeric ranges. When neither \code{cat1}
#' nor \code{cat2} contains age categories, the function returns the data unchanged.
#'
#' @param ph.data A data.table or data.frame containing category and age data to
#' be filtered. Must contain the following columns: \code{cat1}, \code{cat1_group},
#' \code{cat2}, \code{cat2_group}, and the age variable specified by \code{agevar}.
#' @param agevar Character string specifying the name of the age variable column,
#' which must be encoded as an integer.
#'
#' Default: \code{agevar = 'chi_age'}
#'
#' @return A filtered data.table with only logically consistent age values
#'
#' @details
#' The function interprets special formats in age group strings:
#' \itemize{
#'   \item "0" is treated as age "0-0" (age zero)
#'   \item "<1" is treated as age "0-0" (age zero)
#'   \item "<N" is converted to "0-(N-1)" (ages 0 through N-1)
#'   \item "N+" is converted to "N-120" (ages N through 120)
#'   \item "N-M" ranges are used as-is (ages N through M)
#' }
#'
#' Input validation ensures proper column types and structure before processing.
#' Age categories are identified by checking if \code{cat1} or \code{cat2} values
#' match "age" or "* age" (case-insensitive).
#'
#' If invalid age group formats are detected, a warning is issued showing the
#' specific problematic combinations. Age validation is skipped for those specific
#' combinations while continuing to process other valid age categories normally.
#'
#' @seealso
#' \code{\link{chi_count_by_age}} and \code{\link{chi_get_proper_pop}} - This
#' function is typically applied to tables resulting from merging the output of
#' these two functions.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Create sample data with age inconsistencies
#' test_data <- data.table(
#'   cat1 = "Gender",
#'   cat1_group = c("Female", "Male"),
#'   cat2 = "Age",
#'   cat2_group = c("<5", "5-9", "10-14", "15-25", "25+"),
#'   chi_age = sample(1:100, 1000, replace = T),
#'   count = 1
#' )
#'
#' # Filter to keep only consistent rows
#' filtered_data <- chi_drop_illogical_ages(ph.data = test_data,
#'                                          agevar = 'chi_age')
#'
#' # Confirm data was filtered correctly
#' unique(test_data[, .(min_age = min(chi_age), max_age = max(chi_age)),
#'                  cat2_group])
#' unique(filtered_data[, .(min_age = min(chi_age), max_age = max(chi_age)),
#'                  cat2_group])
#' }
#'
#' @importFrom data.table copy := fcase between setDT is.data.table as.data.table
#' @importFrom rads lossless_convert
#' @export
#'
chi_drop_illogical_ages <- function(ph.data,
                                    agevar = 'chi_age') {

  # Validate inputs ----
  if (is.null(ph.data)) {
    stop("\n\U1F6D1 ph.data must be provided")
  }

  if (!is.data.frame(ph.data)) {
    stop("\n\U1F6D1 ph.data must be a data.frame or data.table")
  }

  # Convert to data.table if needed
  if (!data.table::is.data.table(ph.data)) {
    setDT(ph.data)
  } else {
    # Make copy to avoid modifying original by reference
    ph.data <- data.table::copy(ph.data)
  }

  # Check if required columns exist
  required_columns <- c(agevar, "cat1", "cat1_group", "cat2", "cat2_group")
  missing_columns <- required_columns[!required_columns %in% names(ph.data)]

  if (length(missing_columns) > 0) {
    stop(paste0("\n\U1F6D1 ph.data is missing required columns: ",
                paste(missing_columns, collapse = ", ")))
  }

  # Check expected column classes
  for (col_name in c("cat1", "cat1_group", "cat2", "cat2_group")) {
    if (!inherits(ph.data[[col_name]], c("character", "factor"))) {
      actual_class <- class(ph.data[[col_name]])[1]
      stop(paste0("\n\U1F6D1 Column '", col_name, "' should be of class 'character' or 'factor'",
                  ", but is '", actual_class, "'"))
    }
  }

  # Ensure agevar column contains integer values
  if (!is.integer(ph.data[[agevar]])) {
    ph.data[, c(agevar) := rads::lossless_convert(get(agevar), 'integer')]
    if (!is.integer(ph.data[[agevar]])) {
      stop(paste0("\n\U1F6D1 '", agevar, "' could not be converted to an integer"))
    }
  }

  # Return without changes if no age data in table ----
  has_age_cat1 <- any(grepl('^age$| age$', unique(ph.data[['cat1']]), ignore.case = TRUE))
  has_age_cat2 <- any(grepl('^age$| age$', unique(ph.data[['cat2']]), ignore.case = TRUE))

  if (!any(c(has_age_cat1, has_age_cat2))) {
    return(ph.data)
  }

  # Process age filtering ----
  # Loop for cat1 and cat2
  for (catnum in c("cat1", "cat2")) {
    # Get column names for this category
    catgroup <- paste0(catnum, "_group")
    temp_catgroup <- paste0(catgroup, "_temp")

    # Check for age-related rows and validate their group formats
    age_related_rows <- ph.data[grepl('^age$| age$', get(catnum), ignore.case = TRUE)]

    if (nrow(age_related_rows) > 0) {
      # Find rows with invalid age group formats
      invalid_rows <- age_related_rows[!grepl("^(0|<1)$|^<[0-9]+$|^[0-9]+\\+$|^[0-9]+-[0-9]+$", get(catgroup))]

      if (nrow(invalid_rows) > 0) {
        # Show warning with specific problematic combinations
        invalid_summary <- unique(invalid_rows[, list(category = get(catnum), group = get(catgroup))])
        warning(paste0("\u26A0\ufe0f Invalid age group formats detected in '", catnum, "':\n",
                       paste0("  ", invalid_summary$category, ": ", invalid_summary$group, collapse = "\n"),
                       "\nValid formats are: '0', '<1', '<N', 'N+', 'N-M'. \n",
                       "Age filtering will be skipped for these specific combinations."))
      }
    }

    # Create a standardized version of the age group
    ph.data[, (temp_catgroup) := NA_character_]

    ph.data[grepl('^age$| age$', get(catnum), ignore.case = TRUE) & get(catgroup) %in% c('0', '<1'),
            (temp_catgroup) := '0-0']

    ph.data[grepl('^age$| age$', get(catnum), ignore.case = TRUE) & grepl("^<[0-9]+$", get(catgroup)),
            (temp_catgroup) := paste0("0-", as.numeric(gsub("<", "", get(catgroup))) - 1)]

    ph.data[grepl('^age$| age$', get(catnum), ignore.case = TRUE) & grepl("^[0-9]+\\+$", get(catgroup)),
            (temp_catgroup) := gsub("\\+", "-120", get(catgroup))]

    ph.data[grepl('^age$| age$', get(catnum), ignore.case = TRUE) & grepl("^[0-9]+-[0-9]+$", get(catgroup)),
            (temp_catgroup) := as.character(get(catgroup))]


    # Extract min and max age (only for rows with valid temp_catgroup)
    ph.data[, c("min_age", "max_age") := list(NA_real_, NA_real_)]
    ph.data[!is.na(get(temp_catgroup)), min_age := as.numeric(gsub("-.*", "", get(temp_catgroup)))] # keep part before hyphen
    ph.data[!is.na(get(temp_catgroup)), max_age := as.numeric(gsub(".*-", "", get(temp_catgroup)))] # keep part after hyphen

    # Keep rows where either:
    # 1. The cat is not age-related, OR
    # 2. The cat is age-related AND (group format is invalid OR age value is within range)
    ph.data <- ph.data[!grepl('^age$| age$', get(catnum), ignore.case = TRUE) |
                         (grepl('^age$| age$', get(catnum), ignore.case = TRUE) &
                            (is.na(get(temp_catgroup)) |
                               data.table::between(get(agevar), min_age, max_age, incbounds = TRUE)))]

    # Clean up temporary columns
    ph.data[, c(temp_catgroup, "min_age", "max_age") := NULL]
  }

  return(ph.data)
}
