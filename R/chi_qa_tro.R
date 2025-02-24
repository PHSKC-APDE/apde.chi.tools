#' Quality Assurance Check for CHI Tableau Ready Output
#'
#' @description
#' Validates data structures and values in CHI estimates and metadata for compliance
#' with CHI Tableau Ready Output (TRO) standards.
#'
#' @details
#' This function performs comprehensive validation of data structure and content against
#' CHI TRO specifications. When verbose mode is enabled (default), it provides detailed
#' diagnostic information through warnings (for deviations from standards) and messages
#' (for progress and successful checks).
#'
#' The CHI Tableau Ready Output (TRO) standards can be reviewed here:
#' \href{https://kc1.sharepoint.com/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady\%20Output.xlsx}{
#' SharePoint > Community Health Indicators > CHI_vizes > CHI-Standards-TableauReady Output.xlsx}
#'
#' @param CHIestimates Name of a data.table or data.frame containing the prepared data to be pushed to SQL
#' @param CHImetadata Name of a data.table or data.frame containing the metadata to be pushed to SQL
#' @param acs default \code{FALSE}, Indicates whether it is ACS data (which does not have / need varnames)
#' @param verbose default \code{TRUE}, Controls whether to display detailed progress and diagnostic messages
#'
#' @return Returns 1 if all checks pass, 0 if any deviations from standards are detected.
#' If the return value is 0, we suggest running the function with verbose=TRUE to review
#' specific details about potential issues that may need attention.
#'
#' @seealso
#' \code{\link{chi_calc}} for generating estimates
#'
#' \code{\link{chi_generate_metadata}} for creating metadata
#'
#' \code{\link{chi_compare_estimates}} for comparing estimates between versions
#'
#' @keywords CHI, Tableau, Production
#'
#' @importFrom data.table setDT copy setcolorder is.data.table %between% uniqueN
#' @importFrom glue glue
#' @importFrom yaml read_yaml
#' @importFrom rads tsql_validate_field_types round2
#'
#' @examples
#' \dontrun{
#' # Basic QA check of estimates and metadata
#' qa_status <- chi_qa_tro(
#'   CHIestimates = my_estimates,
#'   CHImetadata = my_metadata,
#'   acs = FALSE,
#'   verbose = TRUE
#' )
#' }
#'
#' @export

chi_qa_tro <- function(CHIestimates,
                       CHImetadata,
                       acs = FALSE,
                       verbose = TRUE){
  status <- 1

  ## Check arguments ----
  if(!is.logical(verbose)){
    stop('verbose must a logical, i.e., TRUE or FALSE')
  }


  if(verbose){
    message("Checking that that `acs` is logical")
  }
  if(!is.logical(acs)){
    stop('acs must a logical, i.e., TRUE or FALSE')
  }


  if(verbose){
    message("Checking that both the results and the metadata were provided")
  }
    if(is.null(CHIestimates)){
      status <- 0
      if(verbose) {
        warning("You must provide the name of a data.frame or data.table that contains the CHI results.")
      }

    }
    if(is.null(CHImetadata)){
      status <- 0
      if(verbose){
        warning("You must provide the name of a data.frame or data.table that contains the CHI metadata ")
      }

    }
    #if both data sets are not provided, abort check
    if(status == 0) {
      if(verbose){
        stop("Check incomplete. Please correct errors to proceed")
      }
      return(status)
    }

    CHIestimates <- data.table::setDT(copy(CHIestimates))
    CHImetadata <- data.table::setDT(copy(CHImetadata))

  ## Check columns ----
  if(verbose) {
    message("Checking that all column names are unique")
  }
  if(length(names(CHIestimates)) != length(unique(names(CHIestimates)))) {
    status <- 0
    if(verbose){
      warning("You submitted a dataset where at least two columns have the same name. All names in CHIestimates must be unique.")
    }
  }
  if(length(names(CHImetadata)) != length(unique(names(CHImetadata)))) {
    status <- 0
    if(verbose){
      warning("You submitted a metadata table where at least two columns have the same name. All names in CHImetadata must be unique.")
    }
  }

  if(verbose){
    message("Checking that all necessary columns exist")
  }
  missing.var <- setdiff(chi_get_cols(), names(CHIestimates))
  if(length(missing.var) > 0){
    status <- 0
    if(verbose) {
      missing.var <- paste(missing.var, collapse = ", ")
      warning(glue::glue("You are missing the following critical columns(s) in CHIestimates: {missing.var}"))
    }
  }

  missing.var <- setdiff(names(unlist(chi_get_yaml()$metadata)), names(CHImetadata))
  if(length(missing.var) > 0){
    status <- 0
    if(verbose){
      missing.var <- paste(missing.var, collapse = ", ")
      warning(glue::glue("You are missing the following critical columns(s) in CHImetadata: {missing.var}"))
    }
  }

  if(verbose){
    message("Checking for unexpected columns")
  }
  missing.var <- setdiff(names(CHIestimates), chi_get_cols())
  if(length(missing.var) > 0){
    status <- 0
    if(verbose) {
      missing.var <- paste(missing.var, collapse = ", ")
      warning(glue::glue("Your CHIestimates table has the following non-standard columns: {missing.var}
                         Please drop these variables from CHIestimates before attempting to QA the data again."))
    }
  }

  missing.var <- setdiff(names(CHImetadata), names(unlist(chi_get_yaml()$metadata)))
  if(length(missing.var) > 0){
    status <- 0
    if(verbose){
      missing.var <- paste(missing.var, collapse = ", ")
      warning(glue::glue("Your CHImetadata table has the following non-standard columns: {missing.var}
                         Please drop these variables from CHIestimates before attempting to QA the data again."))
    }
  }

  ## Confirm variable class ----
  if(verbose){
    message("Checking that variables are of the proper class")
  }

  if(verbose){
    message("Validating CHI estimates: ")
  }
  rads::tsql_validate_field_types(ph.data = CHIestimates, field_types = unlist(chi_get_yaml()$vars)) # check CHI estimate table

  if(verbose){
    message(paste("", "Validating CHI metadata: ", sep = "\n"))
  }
  rads::tsql_validate_field_types(ph.data = CHImetadata, field_types = unlist(chi_get_yaml()$metadata)) # check CHI metadata table
  if(verbose) message(paste("", "", sep = "\n"))

  ## Check for missingness ----
  if(verbose){
    message("Checking that critical columns are not missing any values")
  }

  for(mycol in c("indicator_key", "year", "data_source", "tab", "cat1", "cat1_group", "run_date")){
    if(nrow(CHIestimates[is.na(get(mycol))]) > 0){
      status <- 0
      warning(paste0("'", mycol, "' is missing in at least one row but is a critical identifier column in CHI data. \n", "Fix the error and run this QA script again."))
    }
  }

  for(mycol in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "chi", "source_date", "run_date")){
    if(nrow(CHIestimates[is.na(get(mycol)) & is.na(suppression)]) > 0){
      status <- 0
      warning(paste0("\U00026A0 Warning: '", mycol, "' is missing in at least one row of the CHI data."))
    }
  }
  for(mycol in names(unlist(chi_get_yaml()$metadata))){
    if(nrow(CHImetadata[is.na(get(mycol))]) > 0){
      status <- 0
      warning(paste0("'", mycol, "' is missing in at least one row but is a critical identifier column in CHI metadata. \n", "Fix the error and run this QA script again."))
    }
  }

  if(status == 0) {
    if(verbose) {
      warning("Check incomplete. Please correct errors and try again")
    }
    return(status)
  }

  ## Set the columns in standard order ----
  setcolorder(CHIestimates, chi_get_cols())
  setcolorder(CHImetadata, names(unlist(chi_get_yaml()$metadata)))

  ## Basic logic checks for estimates ----

  if(verbose){
    message("Checking for infinite values, which cannot be pushed to SQL")
  }
  for(var in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator")){
    if(nrow(CHIestimates[is.infinite(get(var))]) > 0 ){
      status <- 0
      if(verbose){
          warning(glue::glue("There is at least one row where is.infinite({var}) == T.
                       Please fix this problem before rerunning chi_qa_tro() (e.g., by setting it equal to NA)
                       You can view the problematic data by typing something like: View(CHIestimates[is.infinite({var}), ])"))
      }
    }
  }

  if(verbose){
    message("Checking that proportions are between zero and one")
  }
  CHIestimates <- merge(CHIestimates, CHImetadata[, list(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE) # merge on result_type
  if(nrow(CHIestimates[result_type=="proportion" & !result %between% c(0, 1)]) > 0){
    status <- 0
    if(verbose){
      warning("There is at least one row where where the metadata states that the indicator is a proportion but the result is outside [0,1].
                 Please fix either the metadata table or the CHI estimates and try again.")
    }
  }

  if(verbose){
    message("Checking if upper_bound is greater than lower_bound")
  }
  if(nrow(CHIestimates[upper_bound < lower_bound, ])){
    status <- 0
    if(verbose){
      warning("There is at least one row where the upper_bound is less than the lower_bound.
                 Please fix this error prior to re-running the chi_qa_tro() function.
                 You can view the problematic data by typing something like: View(CHIestimates[upper_bound < lower_bound, ])")
    }
  }

  if(verbose){
    message("Checking that result is less than or equal to the upper bound")
  }
  if(nrow(CHIestimates[!(result <= upper_bound)])){
    status <- 0
    if(verbose){
      warning("There is at least one row where the result is not less than or equal to the upper_bound.
               Please fix this error prior to rerunning the chi_qa_tro() function.
               You can view the problematic data by typing something like: View(CHIestimates[!(result <= upper_bound)])")
    }
  }

  if(verbose){
    message("Checking that result is greater than or equal to the lower_bound")
  }
  if(nrow(CHIestimates[!(result >= lower_bound)])){
    status <- 0
    if(verbose){
      warning("There is at least one row where the result is not greater than or equal to the lower_bound.
           Please fix this error prior to rerunning the chi_qa_tro() function.
           You can view the problematic data by typing something like: View(CHIestimates[!(result >= lower_bound)])")
    }
  }
  if(verbose){
    message("Checking that lower_bound is not less than zero")
  }
  if(nrow(CHIestimates[lower_bound < 0])){
    status <- 0
    if(verbose){
          warning("There is at least one row where the lower_bound is less than zero (i.e., it is negative).
           Please fix this error prior to rerunning the chi_qa_tro() function.
          You can view the problematic data by typing something like: View(CHIestimates[lower_bound < 0])")
    }
  }

  if(verbose){
    message("Checking that RSE is between 0 and 100")
  }
  # confirmed with Abby 2/7/2020 that want RSE * 100
  if(nrow(CHIestimates[!rse %between% c(0, 100)]) > 0 ){
    status <- 0
    if(verbose){
      warning(paste("There is at least one row where the RSE (relative standard error) is outside the range of (0, 100].",
                 "This is not necessarily an error, but you should examine the data to make sure it makes sense.",
                 "You can view the data in question by typing something like: View(CHIestimates[!rse %between% c(0, 100)])", sep = "\n"))
    }
  }

  if(verbose){
    message("Checking that RSE is on scale of 0-100 (i.e., the proportion should have been multiplied by 100)")
  }
  if(nrow(CHIestimates[!is.na(rse)]) == nrow(CHIestimates[rse <=1])){
    status <- 0
    if(verbose){
      warning("All RSEs are within the range (0, 1]. CHI Tableau Ready standards necessitate that these proportions
               be mutliplied by 100. I.e., .12345 >> 12.345
               Please fix this error prior to rerunning the chi_qa_tro() function.")
    }
  }

  if(verbose){
    message("Checking that caution flag exists if RSE >= 30%")
  }
  if(nrow(CHIestimates[rse>=30 & (caution != "!" | is.na(caution)) ]) > 0 ){
    status <- 0
    if(verbose){
      warning("There is at least one row where a caution flag ('!') is not used and rse >= 30% or is.na(rse) == T.
                 Please fix this error prior to rerunning the chi_qa_tro() function.
                 You can view the problematic data by typing something like: View(CHIestimates[(rse>=30 | is.na(rse)) & (caution != '!' | is.na(caution))])")
    }
  }

  if(verbose){
    message("Checking for proper rounding")
  }
  if(verbose){
    message("Checking that result is rounded to three digits")
  }
  if(sum(CHIestimates$result != rads::round2(CHIestimates$result, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'result' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }
  if(verbose){
    message("Checking that lower_bound is rounded to three digits")
  }
  if(sum(CHIestimates$lower_bound != rads::round2(CHIestimates$lower_bound, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'lower_bound' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }
  if(verbose){
    message("checking that upper_bound is rounded to three digits")
  }
  if(sum(CHIestimates$upper_bound != rads::round2(CHIestimates$upper_bound, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'upper_bound' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }
  if(verbose){
    message("Checking that rse is rounded to three digits")
  }
  if(sum(CHIestimates$rse != rads::round2(CHIestimates$rse, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'rse' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }

  if(verbose){
    message("Checking that se is rounded to four digits")
  }
  if(sum(CHIestimates$se != rads::round2(CHIestimates$se, 4), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'se' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }

  if(verbose){
    message("Check that all observations have indicators, categories, tab, and year")
  }
  for(var in c("indicator_key", "tab", "year", "cat1", "cat1_group", "source_date", "run_date")){
    if(nrow(CHIestimates[is.na(get(var))]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa_tro()"))
      }
    }
  }
  if(acs==F){
    if(nrow(CHIestimates[is.na(cat1_varname)]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where 'cat1_varname' is missing.
                        Please fill in the missing value before rerunning chi_qa_tro()"))
      }
    }
  }
  for(var in c("cat2", "cat2_group")){
    if(nrow(CHIestimates[tab=="crosstabs" & is.na(get(var))]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where tab=='crosstabs' & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa_tro()"))
      }
    }
  }
  if(acs==F){
    if(nrow(CHIestimates[tab=="crosstabs" & is.na(cat2_varname)]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where 'cat2_varname' is missing.
                      Please fill in the missing value before rerunning chi_qa_tro()"))
      }
    }
  }

  if(verbose){
    message("Checking that results are present if row is not suppressed")
  }
  for(var in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator")){
    if(nrow(CHIestimates[suppression != "^" & is.na(get(var))]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row that is not suppressed & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa_tro()"))
      }
    }
  }

  ## Ensure cat1/cat2 values meet CHI standards ----
    if(verbose) {
      message("Checking that category combinations align with CHI standards")
    }

    # Get reference data
      ref_combos <- rads.data::misc_chi_byvars[, list(cat, varname, group)]

    # For ACS data, we only check cat and group combinations
      if(acs) {
        # Check cat1 combinations
        chi_cat1_combos <- unique(CHIestimates[!is.na(cat1), list(
          cat = cat1,
          group = cat1_group
        )])

        cat1_invalid <- chi_cat1_combos[!unique(ref_combos[, list(cat, group)]), on = list(cat, group)]

        if(nrow(cat1_invalid) > 0) {
          status <- 0
          if(verbose) {
            warning("\U00026A0 \U0001F4E3 WARNING: Found non-standard cat1 combinations:\n",
                    paste0(" - cat1='", cat1_invalid$cat, "', cat1_group='",
                           cat1_invalid$group, "'", collapse = "\n"),
                    "\nThese combinations are not found in rads.data::misc_chi_byvars reference table.")
          }
        }

        # Check cat2 combinations
        chi_cat2_combos <- unique(CHIestimates[!is.na(cat2), list(
          cat = cat2,
          group = cat2_group
        )])

        cat2_invalid <- chi_cat2_combos[!unique(ref_combos[, list(cat, group)]), on = list(cat, group)]

        if(nrow(cat2_invalid) > 0) {
          status <- 0
          if(verbose) {
            warning("\U00026A0 \U0001F4E3 WARNING: Found non-standard cat2 combinations:\n",
                    paste0(" - cat2='", cat2_invalid$cat, "', cat2_group='",
                           cat2_invalid$group, "'", collapse = "\n"),
                    "\nThese combinations are not found in rads.data::misc_chi_byvars reference table.")
          }
        }
      } else {
        # For non-ACS data, check complete combinations including varname
        # Check cat1 combinations
        chi_cat1_combos <- unique(CHIestimates[!is.na(cat1), list(
          cat = cat1,
          varname = cat1_varname,
          group = cat1_group
        )])

          # wonky tweaks because of annoying structure for Birthing Person's race/ethnicity
            chi_cat1_combos[cat %in% c("Birthing person's race/ethnicity", "Birthing person's race"), cat := "[Birthing person's] Race"]
            chi_cat1_combos[cat == "Birthing person's ethnicity", cat := "[Birthing person's] Ethnicity"]
            chi_cat1_combos[cat == "[Birthing person's] Race" & varname == 'race3' & group == 'Hispanic', cat := "[Birthing person's] Ethnicity"]
            chi_cat1_combos[, cat := gsub("Birthing person's eth", "[Birthing person's] Eth", cat)]

        cat1_invalid <- chi_cat1_combos[!ref_combos, on = list(cat, varname, group)]

        if(nrow(cat1_invalid) > 0) {
          status <- 0
          if(verbose) {
            warning("\U00026A0 \U0001F4E3 WARNING: Found non-standard cat1 combinations:\n",
                    paste0(" - cat1='", cat1_invalid$cat, "', cat1_varname='",
                           cat1_invalid$varname, "', cat1_group='", cat1_invalid$group, "'",
                           collapse = "\n"),
                    "\nThese combinations are not found in rads.data::misc_chi_byvars reference table.")
          }
        }

        # Check cat2 combinations
        chi_cat2_combos <- unique(CHIestimates[!is.na(cat2), list(
          cat = cat2,
          varname = cat2_varname,
          group = cat2_group
        )])

          # wonky tweaks because of annoying structure for Birthing Person's race/ethnicity
            chi_cat2_combos[cat %in% c("Birthing person's race/ethnicity", "Birthing person's race"), cat := "[Birthing person's] Race"]
            chi_cat2_combos[cat == "Birthing person's ethnicity", cat := "[Birthing person's] Ethnicity"]
            chi_cat2_combos[cat == "[Birthing person's] Race" & varname == 'race3' & group == 'Hispanic', cat := "[Birthing person's] Ethnicity"]
            chi_cat2_combos[, cat := gsub("Birthing person's eth", "[Birthing person's] Eth", cat)]

        cat2_invalid <- chi_cat2_combos[!ref_combos, on = list(cat, varname, group)]

        if(nrow(cat2_invalid) > 0) {
          status <- 0
          if(verbose) {
            warning("\U00026A0 \U0001F4E3 WARNING: Found non-standard cat2 combinations:\n",
                    paste0(" - cat2='", cat2_invalid$cat, "', cat2_varname='",
                           cat2_invalid$varname, "', cat2_group='", cat2_invalid$group, "'",
                           collapse = "\n"),
                    "\nThese combinations are not found in rads.data::misc_chi_byvars reference table.")
          }
        }
      }

  ## Ensure there are no more than 10 years of trend data ----
    trend.years <- sort(unique(CHIestimates[tab == 'trends']$year))
    if( length(trend.years) > 10){
      warning('\U00026A0 There are more than 10 unique years of trend data:\n',
           paste(trend.years, collapse = ', '))
    }

  ## Print success statement!!!!!!!! ####
  if(verbose) {
    if(status == 1){
      message("Your data has passed all CHI Tableau Ready formatting, style, and logic checks.")

    } else {
      warning("At least one check has failed. Please review messages, make corrections, and rerun this check.")
    }
  }

  ## Return ----
  return(status)

}

