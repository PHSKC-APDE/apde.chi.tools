#' CHI Generate Metadata
#'
#' @description
#' Function to generate metadata table combining existing metadata and latest estimates.
#'
#' @param meta.old Name of a data.table with the prior year's metadata
#' @param est.current current year's tableau ready output with completed estimates
#' @param allowed_missing A list of lists specifying allowed missing metadata values. Each inner list
#'        should have \code{indicator_key} and \code{column} elements.
#' @param warn_only Logical, if TRUE, only warns about unexpected missing values instead of stopping
#'
#' @return Returns a table of metadata
#'
#' @seealso
#' \code{\link{chi_calc}} for generating estimates
#'
#' \code{\link{chi_qa_tro}} for validating metadata
#'
#' @importFrom data.table setDT copy :=
#' @importFrom rads substrRight
#' @importFrom utils tail
#' @export
#'
chi_generate_metadata <- function(meta.old = NULL,
                                  est.current = NULL,
                                  allowed_missing = list(
                                    list(indicator_key = "medinc", column = "latest_year_count")
                                    # Add more exceptions here as needed
                                  ),
                                  warn_only = FALSE){
  # Input validation ----
  if (is.null(meta.old)) stop("\n\U1F6D1 meta.old must be provided")
  if (!is.data.frame(meta.old)) stop("\n\U1F6D1 meta.old must be a data.frame or data.table")
  if (is.null(est.current)) stop("\n\U1F6D1 est.current must be provided")
  if (!is.data.frame(est.current)) stop("\n\U1F6D1 est.current must be a data.frame or data.table")
  # Convert to data.table if needed ----
  if (!is.data.table(meta.old)) setDT(meta.old)
  if (!is.data.table(est.current)) setDT(est.current)
  # get new metadata ----
  meta.new <- unique(est.current[tab == "metadata",
                                 list(indicator_key,
                                      latest_yearx = as.integer(year),
                                      latest_year_resultx = result,
                                      run_datex = run_date,
                                      latest_year_countx = as.integer(numerator),
                                      latest_year_kc_popx = as.integer(denominator))])
  # merge new metadata onto old metadata ----
  meta.new <- merge(meta.old, meta.new, by = c("indicator_key"), all = T)
  # update with newest data ----
  # only replace old data when there is new data because may stop calculating indicators, in which case, would want to keep old data
  meta.new[!is.na(latest_yearx), latest_year := as.numeric(latest_yearx)]
  meta.new[!is.na(latest_year_resultx), latest_year_result := latest_year_resultx]
  meta.new[, run_date := as.Date(gsub("-", "", run_datex), "%Y%m%d")]
  meta.new[!is.na(latest_year_countx), latest_year_count := latest_year_countx]
  meta.new[!is.na(latest_year_kc_popx), latest_year_kc_pop := latest_year_kc_popx]
  meta.new[, c("latest_yearx", "latest_year_resultx", "run_datex", "latest_year_countx", "latest_year_kc_popx") := NULL]

  # Update valid_years & keep up to 10 years ----
    meta.new[, valid_years := {
      # Get current years as vector
      years_vector <- as.integer(strsplit(valid_years, " ")[[1]])

      # Add latest_year if it's not already there
      if (!is.na(latest_year) && !latest_year %in% years_vector) {
        years_vector <- c(years_vector, latest_year)
      }

      # Sort and keep only most recent 10 years if there are more than 10
      years_vector <- sort(unique(years_vector))
      if (length(years_vector) > 10) {
        years_vector <- tail(years_vector, 10)
      }

      # Convert back to space-separated string
      paste(years_vector, collapse = " ")
    }, by = indicator_key]

  # Ensure there are no missing important metadata cells (with exceptions) ----
  unexpected_missing <- data.frame()

  for(col in names(meta.new)) {
    na_rows <- which(is.na(meta.new[[col]]))

    if(length(na_rows) > 0) {
      for(row in na_rows) {
        ind_key <- meta.new$indicator_key[row]

        # Check if this is an allowed exception
        is_allowed <- any(sapply(allowed_missing, function(exc) {
          exc$indicator_key == ind_key && exc$column == col
        }))

        if(!is_allowed) {
          unexpected_missing <- rbind(unexpected_missing, data.frame(
            indicator_key = ind_key,
            column = col,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  if(nrow(unexpected_missing) > 0) {
    msg <- paste0("Unexpected missing metadata values:\n",
                  paste(apply(unexpected_missing, 1, function(row) {
                    paste0("Indicator '", row[1], "' is missing value for '", row[2], "'")
                  }), collapse = "\n"))

    if(warn_only) {
      warning(paste0('\U00026A0 ', msg))
    } else {
      stop(paste0('\U1F6D1 ', msg))
    }
  }

  # order metadata table ----
  setorder(meta.new, indicator_key)
  # return table ----
  return(meta.new)
}
