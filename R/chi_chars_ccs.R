# chi_chars_ccs ----
#' Generate CHI CHARS counts by CCS category
#'
#' @description
#' Generate hospitalization counts from Comprehensive Hospital Abstract Reporting
#' System (CHARS) data based on Clinical Classification Software (CCS) categories
#' for CHI. This function processes instructions for specific indicators and
#' summarizes CHARS data accordingly. It automatically handles the ICD-9 to ICD-10
#' transition that occurred in 2016.
#'
#' @param ph.indicator A character string of length 1. The indicator key to process,
#' which must exist in the chars.defs data table.
#' @param ph.data A data.table containing the CHARS data to be processed.
#' @param myinstructions A data.table containing processing instructions for each indicator.
#'        Default is the output from chi_generate_tro_shell().
#' @param chars.defs A data.table containing definitions for each indicator. It
#' should have the following columns: `indicator_name`, `indicator_key`, `intent`,
#' `mechanism`, `superlevel`, `broad`, `midlevel`, `detailed`, `age_start`, and
#' `age_end`.
#'
#' @return A data.table containing the summarized CHARS hospitalization data for the
#'         specified indicator, stratified by the requested demographic variables.
#'
#' @details
#' The function processes multiple instructions for the same indicator sequentially.
#'
#' The function automatically detects whether data spans the ICD-9 to ICD-10 transition in 2016
#' and processes each part with the appropriate ICD version. Results are then combined seamlessly.
#'
#' @examples
#' \dontrun{
#' # Example of how to run with future_lapply for memory efficiency
#' library(future)
#' library(future.apply)
#'
#' plan(multisession, workers = future::availableCores() - 1)
#'
#' countsCCS <- rbindlist(future_lapply(VectorOfIndicators, function(indicator) {
#'   chi_chars_ccs(
#'     ph.indicator = indicator,
#'     ph.data = chars,
#'     myinstructions = myinstructions,
#'     chars.defs = chars.defs)
#' }, future.seed = TRUE))
#'
#' plan(sequential)
#'
#' }
#'
#' @seealso
#' \code{\link[rads]{get_data_chars}}, which provides creates ph.data
#'
#' \code{\link[rads]{chars_icd_ccs_count}}, which is the engine used by this function
#'
#' \code{\link{chi_generate_tro_shell}}, which creates myinstructions
#'
#' @import data.table
#' @import rads
#' @export
chi_chars_ccs <- function(ph.indicator = NA,
                          ph.data = NULL,
                          myinstructions = NULL,
                          chars.defs = NULL) {
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

  # Verify that this is not an injury indicator (those should use chi_chars_injury)
  has_mechanism <- "mechanism" %in% names(indicator_def) && !is.na(indicator_def$mechanism)
  has_intent <- "intent" %in% names(indicator_def) && !is.na(indicator_def$intent)

  if (has_mechanism || has_intent) {
    stop(paste0("\n\U1F6D1 Indicator '", ph.indicator, "' appears to be an injury indicator. ",
                "You must using chi_chars_injury() instead."))
  }

  # Check for required parameters
  if (!all(c("superlevel", "broad", "midlevel", 'detailed') %in% names(indicator_def))) {
    stop("\n\U1F6D1 chars.defs must contain columns 'superlevel', 'broad', 'midlevel', and 'detailed' for CCS indicators.")
  }

  # Import CM reference table ----
  message("Loading ICD-CM reference table (this will happen only once per ph.indicator)")
  if (any(instructions$start < 2016, na.rm = TRUE)){ICDCM9_table <- rads::chars_icd_ccs(icdcm_version = 9)}
  ICDCM10_table <- rads::chars_icd_ccs(icdcm_version = 10)

  # Helper function to process ICD data (will be passed to lapply) ----
  process_icd_data <- function(icdcm_version,
                               current_start,
                               current_end,
                               current_row,
                               age_start,
                               age_end,
                               ph_data,
                               ICDCM_table,
                               indicator_def,
                               indicator_key) {
    # Determine geographical filter based on tab ----
    is_wastate <- current_row$tab == "_wastate"

    # Filter data for appropriate years and geography ----
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

    if (nrow(data_subset) == 0) {
      return(NULL)
    }

    # Add year information ----
    data_subset[, year_range := paste0(current_start, "-", current_end)]

    # Get CCS table based on ICD version ----
    ICDCM_table<- if (icdcm_version == 9) ICDCM9_table else ICDCM10_table

    # Generate arguments for chars_icd_ccs_count ----
    superlevel_arg <- if (!is.na(indicator_def$superlevel)) indicator_def$superlevel else NULL
    broad_arg <- if (!is.na(indicator_def$broad)) indicator_def$broad else NULL
    midlevel_arg <- if (!is.na(indicator_def$midlevel)) indicator_def$midlevel else NULL
    detailed_arg <- if (!is.na(indicator_def$detailed)) indicator_def$detailed else NULL

    # Generate group_by argument ----
    if (is.na(current_row$cat2_varname)) {
      group_by_arg <- current_row$cat1_varname
    } else {
      group_by_arg <- c(current_row$cat1_varname, current_row$cat2_varname)
    }
    group_by_arg <- c('chi_age', unique(group_by_arg))

    # Process data with rads::chars_icd_ccs_count----
    result <- rads::chars_icd_ccs_count(
      ph.data = data_subset,
      icdcm_version = icdcm_version,
      CMtable= ICDCM_table,
      icdcm = NULL,
      superlevel = superlevel_arg,
      broad = broad_arg,
      midlevel = midlevel_arg,
      detailed = detailed_arg,
      icdcol = 'diag1',
      group_by = group_by_arg,
      kingco = FALSE  # Already filtered data above, so always FALSE
    )

    # Format the results ----
    if (nrow(result) > 0) {
      # Add metadata columns
      result[, icd_version := icdcm_version]
      result[, original_year_range := paste0(current_row$start, "-", current_row$end)]
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
      result <- result[!is.na(cat1_group)]
      result[cat2_varname == 'chi_geo_kc', cat2_group := 'King County']
      result <- result[is.na(cat2_varname) | (!is.na(cat2_varname) & !is.na(cat2_group))]
      result[, ph.indicator := ph.indicator]

    }

    return(result)
  }

  # Process sequentially ----
  message('\U023F3 Processing calculations... this may take a while')

  # Process each row
  results_list <- lapply(
    X = seq_len(nrow(instructions)),
    FUN = function(i) {
      # Progress meter ----
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

      # Determine if we need ICD-9, ICD-10, or both ----
      current_start <- as.integer(instructions[i, start])
      current_end <- as.integer(instructions[i, end])
      need_icd9 <- current_start < 2016  # Includes any year before 2016
      need_icd10 <- current_end >= 2016  # Includes any year from 2016 onwards

      # Results for this instruction row ----
      row_results <- list()

      # Process with ICD-9 if needed ----
      if (need_icd9) {
        message(paste0("  - Processing ICD-9 portion (", current_start, "-", min(current_end, 2015), ")"))
        row_results$icd9 <- process_icd_data(
          icdcm_version = 9,
          current_start = current_start,
          current_end = min(current_end, 2015),
          current_row = instructions[i,],
          age_start = age_start,
          age_end = age_end,
          ph_data = ph.data,
          ICDCM_table= ICDCM9_table,
          indicator_def = indicator_def,
          indicator_key = ph.indicator
        )
      }

      # Process with ICD-10 if needed ----
      if (need_icd10) {
        message(paste0("  - Processing ICD-10 portion (", max(current_start, 2016), "-", current_end, ")"))
        row_results$icd10 <- process_icd_data(
          icdcm_version = 10,
          current_start = max(current_start, 2016),
          current_end = current_end,
          current_row = instructions[i,],
          age_start = age_start,
          age_end = age_end,
          ph_data = ph.data,
          ICDCM_table= ICDCM10_table,
          indicator_def = indicator_def,
          indicator_key = ph.indicator
        )
      }

      # If we have both ICD-9 and ICD-10 results, merge them ----
      # When data spans both ICD-9 and ICD-10 periods, we need to merge the results
      # This handles the ICD transition in 2016 by combining hospitalization counts
      # while preserving all demographic stratifications
      if (!is.null(row_results$icd9) && !is.null(row_results$icd10) &&
          nrow(row_results$icd9) > 0 && nrow(row_results$icd10) > 0) {

        # Identify common columns for merging
        merge_cols <- intersect(
          names(row_results$icd9)[!names(row_results$icd9) %in% c("hospitalizations", "icd_version", "year_range")],
          names(row_results$icd10)[!names(row_results$icd10) %in% c("hospitalizations", "icd_version", "year_range")]
        )

        # Merge data
        combined <- merge(
          row_results$icd9,
          row_results$icd10,
          by = merge_cols,
          suffixes = c("_icd9", "_icd10"),
          all = TRUE
        )

        # Sum hospitalizations (handling missing values)
        combined[, hospitalizations := sum(
          ifelse(is.na(hospitalizations_icd9), 0, hospitalizations_icd9),
          ifelse(is.na(hospitalizations_icd10), 0, hospitalizations_icd10)
        ), by = merge_cols]

        # Set year to the full original range
        combined[, year := original_year_range]

        return(combined)
      } else if (!is.null(row_results$icd9) && nrow(row_results$icd9) > 0) {
        # Only ICD-9 results available
        row_results$icd9[, year := original_year_range]
        return(row_results$icd9)
      } else if (!is.null(row_results$icd10) && nrow(row_results$icd10) > 0) {
        # Only ICD-10 results available
        row_results$icd10[, year := original_year_range]
        return(row_results$icd10)
      } else {
        # No results available
        return(NULL)
      }
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

  # Remove temporary columns if they exist
  cols_to_remove <- intersect(names(result),
                              c('icd_version_icd9', 'icd_version_icd10', 'hospitalizations_icd9',
                                'hospitalizations_icd10', 'icd_version', 'original_year_range'))
  if (length(cols_to_remove) > 0) {
    result[, (cols_to_remove) := NULL]
  }

  # Ensure we have complete data (for all ages withing the specified range) ----
  # This creates a template with all possible combinations to avoid gaps when needing to age standardize the results
  template <- unique(result[, .(tab,
                                cat1, cat1_varname, cat1_group,
                                cat2, cat2_varname, cat2_group,
                                indicator_key = ph.indicator, year)])

  template <- template[, .(chi_age = seq(age_start, age_end)), by = names(template)]

  result <- merge(template,
                  result[, chi_age := as.numeric(chi_age)],
                  by = intersect(names(template), names(result)),
                  all = TRUE)

  result[is.na(hospitalizations), hospitalizations := 0]

  result <- result[, .(indicator_key, year, chi_age = as.integer(chi_age), hospitalizations, tab, cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group)]

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
      setcolorder(instructions, names(result_combos)),
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
      capture.output(
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
