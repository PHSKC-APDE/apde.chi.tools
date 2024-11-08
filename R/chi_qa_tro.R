#' CHI - QA: Tableau Ready output
#'
#' @description
#' Checks data for compliance with CHI TRO standard

#'
#' @details
#' This function tests if the structure of the data matches CHI Tableau Ready Output
#' specifications. If set to verbose mode (default) will report diagnostic information in
#' The user interface as warnings (if failed) and messages (for progress and pass). In any
#' Case will return 1 or 0 for pass or fail respectively.
#' The CHI Tableau Ready Output (TRO) standards can be reviewed [here]("//phshare01/epe_share/WORK/CHI Visualizations/Tableau Ready Output Format_v2.xlsx")
#'
#' @param chi_est Name of a data.table or data.frame containing the prepared data to be pushed to SQL
#' @param chi_meta Name of a data.table or data.frame containing the metadata to be pushed to SQL
#' @param acs default FALSE, Indicates whether it is ACS data (which does not have / need varnames)
#' @param ignore_trends default TRUE, Indicates whether the time_trends column should be ignored when checking for missing data.
#' @param verbose default TRUE, if false will only return status
#'
#' @return 1 or 0 for pass or fail status
#'
#' @keywords CHI, Tableau, Production
#'
#' @importFrom data.table is.data.table ':=' setDT setDF data.table setorder copy setnames setorder dcast setcolorder fread shift "%between%"
#' @importFrom glue glue
#' @importFrom utils write.table
#' @importFrom yaml yaml.load
#' @importFrom rads chi_qa
#'
#' @examples
#'
#' \dontrun{
#' # create sample data
#'
#' # run function
#' }
#'
#' @export

chi_qa_tro <- function(chi_est, chi_meta, acs = F, ignore_trends = T, verbose = F){
  status <- 1

  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  indicator_key <- result_type <- result <- upper_bound <- lower_bound <- rse <- caution <- tab <- suppression <- time_trends <- NULL

  if(verbose){
    message("Checking that both the results and the metadata were provided")
  }
  if(is.null(chi_est)){
    status <- 0
    if(verbose) {
      warning("You must provide the name of a data.frame or data.table that contains the CHI results.")
    }

  }
  if(is.null(chi_meta)){
    status <- 0
    if(verbose){
      warning("You must provide the name of a data.frame or data.table that contains the CHI metadata ")
    }

  }
  #if both data sets are not provided, abort check
  if(status == 0) {
    if(verbose){
      warning("Check incomplete. Please correct errors to proceed")
    }
    return(status)
  }

  chi_est <- data.table::setDT(copy(chi_est))
  chi_meta <- data.table::setDT(copy(chi_meta))

  ## Load reference YAML ----
  chi.yaml <- yaml::yaml.load(httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/rads/main/ref/chi_qa.yaml", httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")))

  ## Check columns ----
  if(verbose) {
    message("Checking that all column names are unique")
  }
  if(length(names(chi_est)) != length(unique(names(chi_est)))) {
    status <- 0
    if(verbose){
      warning("You submitted a dataset where at least two columns have the same name. All names in chi_est must be unique.")
    }
  }
  if(length(names(chi_meta)) != length(unique(names(chi_meta)))) {
    status <- 0
    if(verbose){
      warning("You submitted a metadata table where at least two columns have the same name. All names in chi_meta must be unique.")
    }
  }

  if(verbose){
    message("Checking that all necessary columns exist")
  }
  missing.var <- setdiff(names(chi.yaml$vars), names(chi_est))
  if(length(missing.var) > 0){
    status <- 0
    if(verbose) {
      missing.var <- paste(missing.var, collapse = ", ")
      warning(glue::glue("You are missing the following critical columns(s) in chi_est: {missing.var}"))
    }
  }

  missing.var <- setdiff(names(chi.yaml$metadata), names(chi_meta))
  if(length(missing.var) > 0){
    status <- 0
    if(verbose){
      missing.var <- paste(missing.var, collapse = ", ")
      warning(glue::glue("You are missing the following critical columns(s) in chi_meta: {missing.var}"))
    }
  }

  # Confirm that there are no additional variables ----
  extra.var <- setdiff(names(chi_est), names(chi.yaml$vars))
  if(length(extra.var) > 0){
    status <- 0
    if(verbose){
      extra.var <- paste(extra.var, collapse = ", ")
      warning(glue::glue("Your dataset contains the following columns that are not CHI compliant: {extra.var}.
                            Please drop these variables from chi_est before attempting to QA the data again."))
    }
  }

  extra.var <- setdiff(names(chi_meta), names(chi.yaml$meta))
  if(length(extra.var) > 0){
    status <- 0
    if(verbose){
      extra.var <- paste(extra.var, collapse = ", ")
      warning(glue::glue("Your metadata table contains the following columns that are not CHI compliant: {extra.var}.
                            Please drop these variables from chi_meta before attempting to QA the data again."))
    }
  }

  if(verbose){
    message("Checking that variables are of the proper class")
  }
  if(verbose){
    message("Validating CHI estimates: ")
  }
  rads::validate_yaml_data(DF = chi_est, YML = chi.yaml, VARS = "vars") # check CHI estimate table
  if(verbose){
    message(paste("", "Validating CHI metadata: ", sep = "\n"))
  }
  rads::validate_yaml_data(DF = chi_meta, YML = chi.yaml, VARS = "metadata") # check CHI metadata table
  if(verbose) message(paste("", "", sep = "\n"))

  if(verbose){
    message("Checking that critical columns are not missing any values")
  }
  for(mycol in c("indicator_key", "year", "data_source", "tab", "cat1", "cat1_group", "run_date")){
    if(nrow(chi_est[is.na(get(mycol))]) > 0){
      status <- 0
      warning(paste0("'", mycol, "' is missing in at least one row but is a critical identifier column in CHI data. \n", "Fix the error and run this QA script again."))
    }
  }
  for(mycol in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "chi", "source_date", "run_date", "comparison_with_kc")){
    if(nrow(chi_est[is.na(get(mycol)) & is.na(suppression)]) > 0){
      status <- 0
      warning(paste0("\U00026A0 Warning: '", mycol, "' is missing in at least one row of the CHI data."))
    }
  }
  for(mycol in setdiff(names(chi.yaml$metadata), c("latest_year_kc_pop", "latest_year_count"))){
    if(nrow(chi_meta[is.na(get(mycol))]) > 0){
      status <- 0
      warning(paste0("'", mycol, "' is missing in at least one row but is a critical identifier column in CHI metadata. \n", "Fix the error and run this QA script again."))
    }
  }
  for(mycol in c("latest_year_kc_pop", "latest_year_count")){
    if(nrow(chi_meta[is.na(get(mycol))]) > 0){
      status <- 0
      warning(paste0("\U00026A0 Warning: '", mycol, "' is missing in at least one row of the metadata."))
    }
  }

  if(status == 0) {
    if(verbose) {
      warning("Check incomplete. Please correct errors and try again")
    }
    return(status)
  }

  ## Set the columns in standard order ----
  setcolorder(chi_est, names(chi.yaml$vars))
  setcolorder(chi_meta, names(chi.yaml$meta))

  #Basic logic checks for estimates

  if(verbose){
    message("Checking for infinite values, which cannot be pushed to SQL")
  }
  for(var in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator")){
    if(nrow(chi_est[is.infinite(get(var))]) > 0 ){
      status <- 0
      if(verbose){
          warning(glue::glue("There is at least one row where is.infinite({var}) == T.
                       Please fix this problem before rerunning chi_qa() (e.g., by setting it equal to NA)
                       You can view the problematic data by typing something like: View(chi_est[is.infinite({var}), ])"))
      }
    }
  }

  if(verbose){
    message("Checking that proportions are between zero and one")
  }
  chi_est <- merge(chi_est, chi_meta[, list(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE) # merge on result_type
  if(nrow(chi_est[result_type=="proportion" & !result %between% c(0, 1)]) > 0){
    status <- 0
    if(verbose){
      warning("There is at least one row where where the metadata states that the indicator is a proportion but the result is outside [0,1].
                 Please fix either the metadata table or the CHI estimates and try again.")
    }
  }

  if(verbose){
    message("Checking if upper_bound is greater than lower_bound")
  }
  if(nrow(chi_est[upper_bound < lower_bound, ])){
    status <- 0
    if(verbose){
      warning("There is at least one row where the upper_bound is less than the lower_bound.
                 Please fix this error prior to re-running the chi_qa() function.
                 You can view the problematic data by typing something like: View(chi_est[upper_bound < lower_bound, ])")
    }
  }

  if(verbose){
    message("Checking that result is less than or equal to the upper bound")
  }
  if(nrow(chi_est[!(result <= upper_bound)])){
    status <- 0
    if(verbose){
      warning("There is at least one row where the result is not less than or equal to the upper_bound.
               Please fix this error prior to rerunning the chi_qa() function.
               You can view the problematic data by typing something like: View(chi_est[!(result <= upper_bound)])")
    }
  }

  if(verbose){
    message("Checking that result is greater than or equal to the lower_bound")
  }
  if(nrow(chi_est[!(result >= lower_bound)])){
    status <- 0
    if(verbose){
      warning("There is at least one row where the result is not greater than or equal to the lower_bound.
           Please fix this error prior to rerunning the chi_qa() function.
           You can view the problematic data by typing something like: View(chi_est[!(result >= lower_bound)])")
    }
  }
  if(verbose){
    message("Checking that lower_bound is not less than zero")
  }
  if(nrow(chi_est[lower_bound < 0])){
    status <- 0
    if(verbose){
          warning("There is at least one row where the lower_bound is less than zero (i.e., it is negative).
           Please fix this error prior to rerunning the chi_qa() function.
          You can view the problematic data by typing something like: View(chi_est[lower_bound < 0])")
    }
  }

  if(verbose){
    message("Checking that RSE is between 0 and 100")
  }
  # confirmed with Abby 2/7/2020 that want RSE * 100
  if(nrow(chi_est[!rse %between% c(0, 100)]) > 0 ){
    status <- 0
    if(verbose){
      warning(paste("There is at least one row where the RSE (relative standard error) is outside the range of (0, 100].",
                 "This is not necessarily an error, but you should examine the data to make sure it makes sense.",
                 "You can view the data in question by typing something like: View(chi_est[!rse %between% c(0, 100)])", sep = "\n"))
    }
  }

  if(verbose){
    message("Checking that RSE is on scale of 0-100 (i.e., the proportion should have been multiplied by 100)")
  }
  if(nrow(chi_est[!is.na(rse)]) == nrow(chi_est[rse <=1])){
    status <- 0
    if(verbose){
      warning("All RSEs are within the range (0, 1]. CHI Tableau Ready standards necessitate that these proportions
               be mutliplied by 100. I.e., .12345 >> 12.345
               Please fix this error prior to rerunning the chi_qa() function.")
    }
  }

  if(verbose){
    message("Checking that caution flag exists if RSE >= 30%")
  }
  if(nrow(chi_est[rse>=30 & (caution != "!" | is.na(caution)) ]) > 0 ){
    status <- 0
    if(verbose){
      warning("There is at least one row where a caution flag ('!') is not used and rse >= 30% or is.na(rse) == T.
                 Please fix this error prior to rerunning the chi_qa() function.
                 You can view the problematic data by typing something like: View(chi_est[(rse>=30 | is.na(rse)) & (caution != '!' | is.na(caution))])")
    }
  }

  if(verbose){
    message("Checking for proper rounding")
  }
  if(verbose){
    message("Checking that result is rounded to three digits")
  }
  if(sum(chi_est$result != round2(chi_est$result, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'result' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }
  if(verbose){
    message("Checking that lower_bound is rounded to three digits")
  }
  if(sum(chi_est$lower_bound != round2(chi_est$lower_bound, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'lower_bound' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }
  if(verbose){
    message("checking that upper_bound is rounded to three digits")
  }
  if(sum(chi_est$upper_bound != round2(chi_est$upper_bound, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'upper_bound' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }
  if(verbose){
    message("Checking that rse is rounded to three digits")
  }
  if(sum(chi_est$rse != round2(chi_est$rse, 3), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'rse' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }

  if(vebrose){
    message("Checking that se is rounded to four digits")
  }
  if(sum(chi_est$se != round2(chi_est$se, 4), na.rm = T) != 0) {
    status <- 0
    if(verbose){
      warning("The 'se' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
    }
  }

  if(verbose){
    message("Check that all observations have indicators, categories, tab, and year")
  }
  for(var in c("indicator_key", "tab", "year", "cat1", "cat1_group", "source_date", "run_date")){
    if(nrow(chi_est[is.na(get(var))]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))
      }
    }
  }
  if(acs==F){
    if(nrow(chi_est[is.na("cat1_varname")]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where 'cat1_varname' is missing.
                        Please fill in the missing value before rerunning chi_qa()"))
      }
    }
  }
  for(var in c("cat2", "cat2_group")){
    if(nrow(chi_est[tab=="crosstabs" & is.na(get(var))]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where tab=='crosstabs' & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))
      }
    }
  }
  if(acs==F){
    if(nrow(chi_est[tab=="crosstabs" & is.na("cat2_varname")]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row where 'cat2_varname' is missing.
                      Please fill in the missing value before rerunning chi_qa()"))
      }
    }
  }

  if(verbose){
    message("Checking that results are present if row is not suppressed")
  }
  for(var in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc")){
    if(nrow(chi_est[suppression != "^" & is.na(get(var))]) > 0 ){
      status <- 0
      if(verbose){
        warning(glue::glue("There is at least one row that is not suppressed & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))
      }
    }
  }

  if(ignore_trends == F){
    if(verbose){
      message("Checking that time_trends are provided when tab=='trends'")
    }
    if(nrow(chi_est[tab == "trends" & is.na(time_trends)]) > 0 ){
      status <- 0
      if(verbose) {
              warning(glue::glue("There is at least one row where tab=='trends' & where 'time_trends' is missing.
                            Please fill in the missing value before rerunning chi_qa()"))
      }
    }
  }

  ## Compare with previous year's results (FOR FUTURE???)----
      # in function arguments, have user submit most recent and comparison year(s). Submit as character b/c can be 2013-2017, not just 2017
      # if both are null, skip the comparison
      # it not submitted, merge newer data on old data and identify rows with > 3% absolute difference
      # save this dataset for manual review by the user
  ## Compare with a CSV (FOR FUTURE) ----
      # sometimes want to compare with external data source ---
      # must identify year of interest as above
      # instead of submitting a reference year, the user specifies a reference file
      # reference file will attempt to match on all columns that have the same name, except those with results (i.e., results, lower_bound, se, etc.)
      # actual comparison code should be the same as when comparing to a previous year, so write a small funcion to do this

  ## Print success statement!!!!!!!! ####
  if(verbose) {
    if(status == 1){
      message("Your data has passed all CHI Tableau Ready formatting, style, and logic checks.")

    } else {
      warning("At least one check has failed. Please review messages, make corrections, and rerun this check.")
    }
  }

  return(status)

}

