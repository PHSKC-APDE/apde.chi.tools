# chi_chars_injury ----
#' Generate CHI CHARS counts for injury indicators
#'
#' @description
#' Generate hospitalization counts from Comprehensive Hospital Abstract Reporting
#' System (CHARS) data for injury indicators including falls, poisoning,
#' self-harm, and other injury mechanisms for CHI. This function processes
#' instructions for specific indicators and summarizes CHARS data accordingly.
#'
#' @param ph.indicator A character string of length 1. The indicator key to process,
#' which must exist in the chars.defs data table.
#' @param ph.data A data.table containing the CHARS data to be processed.
#' @param myinstructions A data.table containing processing instructions for each indicator.
#'        Default is the output from \code{\link{chi_generate_tro_shell}}.
#' @param chars.defs A data.table containing definitions for each indicator. It
#' should have the following columns: `indicator_name`, `indicator_key`, `intent`,
#' `mechanism`, `age_start`, and `age_end`.
#' @param def A character string indicating which injury definition to use.
#'        Default is \code{def = 'narrow'}.
#'
#' @return A data.table containing the summarized CHARS injury hospitalization data for the
#'         specified indicator, stratified by the requested demographic variables.
#'
#' @details
#' This function processes instructions for a specific indicator sequentially.
#'
#' Note that injury data is only available for 2012 and later years. Unlike chi_chars_ccs(),
#' this function doesn't need to handle the ICD-9 to ICD-10 transition explicitly as the
#' mechanism and intent columns have been standardized across coding systems.
#'
#' @examples
#' \dontrun{
#' # Example of how to run with future_lapply for memory efficiency
#' library(future)
#' library(future.apply)
#'
#' plan(multisession, workers = future::availableCores() - 1)
#'
#' countsINJURY <- rbindlist(future_lapply(VectorOfIndicators, function(indicator) {
#'   chi_chars_injury(
#'     ph.indicator = indicator,
#'     ph.data = chars,
#'     myinstructions = myinstructions,
#'     chars.defs = chars.defs,
#'     def = 'narrow')
#' }, future.seed = TRUE))
#'
#' plan(sequential)
#'
#' }
#'
#' @seealso
#' \code{\link[rads]{get_data_chars}}, which provides creates ph.data
#'
#' \code{\link[rads]{chars_injury_matrix_count}}, which is the engine used by this function
#'
#' \code{\link{chi_generate_tro_shell}}, which creates myinstructions
#'
#' @import data.table
#' @import rads
#' @importFrom utils capture.output
#' @export
chi_chars_injury <- function(ph.indicator = NA,
                             ph.data = NULL,
                             myinstructions = NULL,
                             chars.defs = NULL,
                             def = 'narrow') {
  # Input validation -----
  if (is.na(ph.indicator)) {
    stop("\n\U1F6D1 ph.indicator must be provided.")
  }

  for (arg_name in c("ph.data", "myinstructions", "chars.defs")) {
    arg_value <- get(arg_name)
    if (is.null(arg_value)) {
      stop(paste0("\n\U1F6D1 ", arg_name, " must be specified."))
    }
    if (is.data.frame(arg_value)) {
      setDT(arg_value)
    } else if (!is.data.frame(arg_value)) {
      stop(paste0("\n\U1F6D1 ", arg_name, " must be a data.table or data.frame."))
    }
  }

  # Ensure indicator exists
  instructions <- myinstructions[indicator_key == ph.indicator]

  if (nrow(instructions) == 0) {
    stop(paste0("\n\U1F6D1 ph.indicator '", ph.indicator, "' not found in myinstructions."))
  }

  if (!ph.indicator %in% chars.defs$indicator_key) {
    stop(paste0("\n\U1F6D1 ph.indicator '", ph.indicator, "' not found in chars.defs."))
  }


  # Ensure cat1_varname and cat2_varname exist in ph.data
  unique_cat_vars <- unique(c(
    instructions$cat1_varname[!is.na(instructions$cat1_varname)],
    instructions$cat2_varname[!is.na(instructions$cat2_varname)]
  ))

  missing_cols <- unique_cat_vars[!unique_cat_vars %in% names(ph.data)]
  if (length(missing_cols) > 0) {
    stop(paste0("\n\U1F47F The following columns referenced in instructions don't exist in ph.data: ",
                paste(missing_cols, collapse = ", ")))
  }

  # Extract indicator parameters
  indicator_def <- chars.defs[indicator_key == ph.indicator]
  age_start <- indicator_def$age_start
  age_end <- indicator_def$age_end
  if (!is.numeric(age_start) || !is.numeric(age_end)) {
    stop("\n\U1F6D1 age_start and age_end must be numeric.")
  }

  # Verify that this is an injury indicator (those should use chi_chars_injury)
  has_mechanism <- "mechanism" %in% names(indicator_def) && !is.na(indicator_def$mechanism)
  has_intent <- "intent" %in% names(indicator_def) && !is.na(indicator_def$intent)

  if (!has_mechanism && !has_intent) {
    stop(paste0("\n\U1F6D1 Indicator '", ph.indicator, "' does not appear to be an injury indicator. ",
                "You must use chi_chars_ccs() instead."))
  }

  # Check for required parameters
  if (!all(c("intent", "mechanism") %in% names(indicator_def))) {
    stop("\n\U1F6D1 chars.defs must contain columns 'intent' and 'mechanism' for injury indicators.")
  }

  # Validate injury-specific parameters
  if (!is.character(def) || length(def) != 1 || !def %in% c('narrow', 'broad')) {
    stop("\n\U1F6D1 'def' must be either 'narrow' or 'broad'.")
  }

  # Process sequentially ----
  message('\U023F3 Processing calculations... this may take a while')

  # Process each row
  results_list <- lapply(
    X = seq_len(nrow(instructions)),
    FUN = function(i) {
      # Progress meter
      current_row <- instructions[i]
      row_info <- paste(
        ph.indicator,
        current_row$tab,
        current_row$cat1,
        current_row$cat1_varname,
        current_row$cat2,
        current_row$cat2_varname,
        current_row$end,
        current_row$start,
        sep = "|"
      )
      message(paste0("myinstructions row ", i, "/", nrow(instructions), ": \n  - ", row_info))

      # Get current instruction parameters
      current_start <- as.integer(current_row$start)
      current_end <- as.integer(current_row$end)

      # For injury data, check if the year range is valid (only available from 2012+)
      if (current_start < 2012) {
        message(paste0("  - \u26A0\ufe0f Warning: Injury data only available from 2012. Adjusting start year from ",
                       current_start, " to 2012."))
        current_start <- 2012
      }

      # If start year is now after end year, skip this row
      if (current_start > current_end) {
        message(paste0("  - \u26A0\ufe0f Skipping: After year adjustment, start year ", current_start,
                       " is after end year ", current_end))
        return(NULL)
      }

      # Determine geographical filter based on tab
      is_wastate <- current_row$tab == "_wastate"

      # Filter data for appropriate years, geography, and age
      if (is_wastate) {
        data_subset <- data.table::copy(ph.data[wastate == 'Washington State' &
                                                  chi_year >= current_start &
                                                  chi_year <= current_end &
                                                  chi_age >= age_start &
                                                  chi_age <= age_end])
      } else {
        data_subset <- data.table::copy(ph.data[chi_geo_kc == 'King County' &
                                                  chi_year >= current_start &
                                                  chi_year <= current_end &
                                                  chi_age >= age_start &
                                                  chi_age <= age_end])
      }

      # Return NULL if no data after filtering
      if (nrow(data_subset) == 0) {
        return(NULL)
      }

      # Add year information
      data_subset[, year_range := paste0(current_start, "-", current_end)]

      # Generate group_by argument
      if (is.na(current_row$cat2_varname)) {
        group_by_arg <- current_row$cat1_varname
      } else {
        group_by_arg <- c(current_row$cat1_varname, current_row$cat2_varname)
      }
      group_by_arg <- c('chi_age', unique(group_by_arg))

      # Process data with rads::chars_injury_matrix_count
      result <- rads::chars_injury_matrix_count(
        ph.data = data_subset,
        intent = indicator_def$intent,
        mechanism = indicator_def$mechanism,
        group_by = group_by_arg,
        def = def,
        kingco = FALSE  # Already filtered data above
      )

      # Return NULL if no results
      if (nrow(result) == 0) {
        return(NULL)
      }

      # For ICD10cm, poisoning is split into drug & non-drug, collapse them if needed
      if (!is.null(indicator_def$mechanism) && !is.na(indicator_def$mechanism) &&
          indicator_def$mechanism == 'poisoning') {
        result <- result[, list(mechanism = 'poisoning', hospitalizations = sum(hospitalizations)),
                         by = setdiff(names(result), c('mechanism', 'hospitalizations'))]
      }

      # Remove mechanism and intent columns
      if ("mechanism" %in% names(result)) {
        result[, mechanism := NULL]
      }
      if ("intent" %in% names(result)) {
        result[, intent := NULL]
      }

      # Format the results
      # Add metadata columns
      result[, year := paste0(current_start, "-", current_end)]
      result[, tab := current_row$tab]
      result[, cat1 := current_row$cat1]
      result[, cat2 := current_row$cat2]
      result[, cat1_varname := gsub("_hispanic", "", current_row$cat1_varname)]
      result[, cat2_varname := gsub("_hispanic", "", current_row$cat2_varname)]

      # Rename category columns
      data.table::setnames(result, current_row$cat1_varname, "cat1_group")

      if (!is.na(current_row$cat2_varname)) {
        data.table::setnames(result, current_row$cat2_varname, "cat2_group", skip_absent = TRUE)
      }

      # Filter and format
      result <- result[!is.na(cat1_group)]  # Drop when group not identified
      result[cat2_varname == 'chi_geo_kc', cat2_group := 'King County']
      result <- result[is.na(cat2_varname) | (!is.na(cat2_varname) & !is.na(cat2_group))]
      result[, ph.indicator := ph.indicator]

      return(result)
    }
  )

  # Filter out NULL results
  results_list <- results_list[!sapply(results_list, is.null)]

  # Combine results if any exist
  if (length(results_list) > 0) {
    result <- data.table::rbindlist(results_list, fill = TRUE, use.names = TRUE)
  } else {
    warning("\u26A0\ufe0f No results found for indicator: ", ph.indicator)
    result <- data.table::data.table()
  }

  # Ensure we have complete data (for all ages within the specified range) ----
  # This creates a template with all possible combinations to avoid gaps when needing to age standardize the results
  template <- unique(result[, list(tab,
                                cat1, cat1_varname, cat1_group,
                                cat2, cat2_varname, cat2_group,
                                indicator_key = ph.indicator, year)])

  template <- template[, list(chi_age = seq(age_start, age_end)), by = names(template)]

  result <- merge(template,
                  result[, chi_age := as.numeric(chi_age)],
                  by = intersect(names(template), names(result)),
                  all = TRUE)

  result[is.na(hospitalizations), hospitalizations := 0]

  result <- result[, list(indicator_key, year, chi_age = as.integer(chi_age), hospitalizations, tab, cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group)]

  setorder(result, tab, year, cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group, chi_age, hospitalizations)

  # Identify instructions that caused all data to be filtered out ----
  # this helps diagnose data quality issues, either in myinstructions or in ph.data
  if (nrow(result) > 0) {
    # Get combinations actually used in results
    result_combos <- unique(result[, list(indicator_key, tab, cat1, cat1_varname, cat2, cat2_varname,
                                          start = as.numeric(substr(year, 1, 4)),
                                          end = as.numeric(substr(year, nchar(year) - 3, nchar(year))) )])

    # Handle the special case for race3_hispanic
    result_combos[cat1_varname == 'race3' & cat1 == 'Ethnicity', cat1_varname := 'race3_hispanic']

    # Use fsetdiff to find instructions that didn't produce results
    unused_instructions <- fsetdiff(
      setcolorder(instructions[, start := pmax(2012, start)], names(result_combos)),
      result_combos
    )

    # Sort for warning display
    setorder(unused_instructions, indicator_key, tab, cat1, cat1_varname, cat2, cat2_varname, start, end)
  } else {
    unused_instructions <- copy(instructions)
  }

  # Generate warnings and attach to result
  if (nrow(unused_instructions) > 0) {
    # Capture the formatted output directly
    empty_table <- paste(
      utils::capture.output(
        print(unused_instructions,
              row.names = FALSE,
              class = FALSE,
              printClassOfColumns = FALSE)
      ),
      collapse = "\n"
    )

    warning(paste0(
      "\n\u26A0\ufe0f No data found for the following ", nrow(unused_instructions),
      " instruction(s) for indicator '", ph.indicator, "':\n",
      empty_table
    ))
  }

  # Return data.table ----
  return(result)
}
