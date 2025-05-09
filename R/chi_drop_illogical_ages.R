#' Filter Out Age Values That Don't Match Their Corresponding Age Group Categories
#'
#' @description
#' This function filters a data.table to remove rows with inconsistent age values.
#' It compares the single year age value (specified by \code{agevar}) against age
#' ranges defined in \code{cat1_group} and \code{cat2_group} columns, keeping only
#' those rows where:
#' \itemize{
#'   \item The categorical variable is not age-related, OR
#'   \item The age value falls within the range specified by the corresponding age group
#' }
#'
#' Age groups are expected to be in formats like "10-17", "<5", or "45+", which
#' the function automatically parses into numeric ranges.
#'
#' @param ph.data A data.table or data.frame containing category and age data to be filtered.
#' @param agevar Character string specifying the name of the age variable column.
#'
#' Default: \code{agevar = 'chi_age'}
#'
#' @return A filtered data.table with only logically consistent age values
#'
#' @details
#' The function interprets special formats in age group strings:
#' \itemize{
#'   \item "<1" is treated as age "0-0" (age zero)
#'   \item "<N" is converted to "0-N" (ages 0 through N)
#'   \item "N+" is converted to "N-120" (ages N through 120)
#' }
#'
#' @seealso
#' \code{\link{chi_count_by_age}} and \code{\link{chi_get_proper_pop}} - This function
#' is typically applied to tables resulting from merging the output of these two functions.
#'
#' @examples
#' \dontrun{
#' # Assuming infmort_combo is a data.frame or data.table with age, cat1,
#' # cat1_group, cat2, cat2_group columns:
#'
#' filtered_data <- chi_drop_illogical_ages(ph.data = infmort_combo, agevar = 'age')
#' }
#'
#' @importFrom data.table copy := fcase between
#' @export
#'
chi_drop_illogical_ages <- function(ph.data, agevar = 'chi_age') {
  # Validate ph.data
  if (!is.data.frame(ph.data)) {
    stop("'ph.data' must be a data.frame or data.table")
  }

  # Convert to data.table if needed
    setDT(ph.data)

  # Check if required columns exist
  critical_cols <- c(agevar, "cat1", "cat1_group", "cat2", "cat2_group")
  missing_cols <- setdiff(critical_cols, names(ph.data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  # Ensure agevar column contains numeric values
  if (!is.numeric(ph.data[[agevar]])) {
    warning(paste("'", agevar, "' column is not numeric. Attempting to convert...", sep = ""))
    tryCatch({
      ph.data[, (agevar) := as.numeric(get(agevar))]
    }, error = function(e) {
      stop(paste("'", agevar, "' could not be converted to numeric. Error: ", e$message, sep = ""))
    })
  }

  # Loop for cat1 and cat2
  for (catnum in c("cat1", "cat2")) {
    # Get column names for this category
    catgroup <- paste0(catnum, "_group")
    temp_catgroup <- paste0(catgroup, "_temp")

    # Create a standardized version of the age group
    ph.data[, (temp_catgroup) := data.table::fcase(
      get(catgroup) == '<1' & grepl(' age$|^age$', get(catnum), ignore.case = T), '0-0',

      grepl("<", get(catgroup)) &grepl(' age$|^age$', get(catnum), ignore.case = T), gsub("<", "0-", get(catgroup)),

      grepl("\\+", get(catgroup))& grepl(' age$|^age$', get(catnum), ignore.case = T), gsub("\\+", "-120", get(catgroup)),

      grepl('-', get(catgroup))& grepl(' age$|^age$', get(catnum), ignore.case = T), as.character(get(catgroup)))]

    # Extract min and max age
    ph.data[, "min_age" := as.numeric(gsub("-.*", "", get(temp_catgroup)))]
    ph.data[, "max_age" := as.numeric(gsub(".*-", "", get(temp_catgroup)))]

    # Keep rows where either:
    # 1. The cat is not age-related, OR
    # 2. The cat is age-related AND age value is within the min-max range
    ph.data <- ph.data[!grepl(' age$|^age$', get(catnum), ignore.case = T) |
                         data.table::between(get(agevar), min_age, max_age)]

    # Clean up temporary columns
    ph.data[, c(temp_catgroup, "min_age", "max_age") := NULL]
  }

  return(ph.data)
}
