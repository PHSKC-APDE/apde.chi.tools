#' Calculate CHI Estimates
#'
#' @description
#' Generates CHI estimates from input data according to provided instructions.
#' Handles both proportions and means, with options for suppression of small numbers.
#'
#' @param ph.data data.frame or data.table. Input data containing analytic read data.
#' @param ph.instructions data.frame or data.table. Calculation instructions for processing.
#' @param ci numeric. Confidence level between 0 and 1. Default: \code{0.90}.
#' @param rate logical. If TRUE calculates rates, if FALSE calculates proportions. Default: \code{FALSE}.
#' @param rate_per numeric. Rate multiplier when \code{rate = TRUE} (e.g., 100000 for per 100,000). Default: \code{NULL}.
#' @param small_num_suppress logical. If TRUE suppresses small numbers. Default: \code{TRUE}.
#' @param suppress_low numeric. Lower bound for suppression. Default: \code{0}.
#' @param suppress_high numeric. Upper bound for suppression. Default: \code{9}.
#' @param source_name character. Name of data source. Default: \code{NULL}.
#' @param source_date Date. Date ph.data was created. Default: \code{NULL}.
#' @param non_chi_byvars character vector. Variable names to exclude from CHI byvar encoding validation. Default: \code{NULL}.
#'
#' @details
#' Uses \code{ph.instructions} created by \code{\link{chi_generate_tro_shell}} to
#' generate standard CHI output following \href{https://kc1.sharepoint.com/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady\%20Output.xlsx}{
#' SharePoint > Community Health Indicators > CHI_vizes > CHI-Standards-TableauReady Output.xlsx}.
#' The exception is the inclusion of the column \code{'level'}, which contains
#' the specific factor level for categorical variables. For example, if
#' \code{indicator_key == 'fetal_pres'}, the function would return separate rows
#' for each possible presentation: a row with \code{level == 'Breech'}, another
#' with \code{level == 'Cephalic'}, and another with \code{level == 'Other'}. In
#' these cases, we would use the level column to filter for our factor level of
#' interest.
#'
#' For convenience, details of the returned column are presented here:
#' \itemize{
#'   \item{\code{data_source}} Data source (e.g., acs, brfss, etc.)
#'   \item{\code{indicator_key}} Unique indicator key
#'   \item{\code{level}} factor level of the indicator_key (e.g., Breech vs Cephalic vs other for fetal_pres in birth data)
#'   \item{\code{tab}} Type of analysis (e.g., demgroups, _kingcounty, etc.)
#'   \item{\code{year}} Year(s) of data
#'   \item{\code{cat1}} Describes data field (e.g., Gender, Ethnicity, etc.)
#'   \item{\code{cat1_group}} Demographics variables for cat1 (e.g., Female, Hispanic, etc.)
#'   \item{\code{cat1_varname}} cat1 variable name in the analytic data sets
#'   \item{\code{cat2}} Describes data field (e.g., Gender, Ethnicity, etc.)
#'   \item{\code{cat2_group}} Demographics variables for cat2 (e.g., Female, Hispanic, etc.)
#'   \item{\code{cat2_varname}} cat2 variable name in the analytic data sets
#'   \item{\code{result}} Calculated proportion/percent or rate
#'   \item{\code{lower_bound}} Lower bound of confidence interval
#'   \item{\code{upper_bound}} Upper bound of confidence interval
#'   \item{\code{se}} Standard error
#'   \item{\code{rse}} Relative standard error
#'   \item{\code{caution}} '!' when RSE>=30% | N == 0
#'   \item{\code{suppression}} '^' when suppressed
#'   \item{\code{numerator}} For line-level data, count of events; for surveys, people who responded yes or no for binary variable
#'   \item{\code{denominator}} For line-level data, population; for surveys, sample size
#'   \item{\code{chi}} '1' indicates that rows is used for CHI
#'   \item{\code{source_date}} date analytic ready data was created
#'   \item{\code{run_date}} date of this analysis
#' }
#'
#' @return Returns a data.table containing CHI estimates
#'
#' @seealso
#' \code{\link{chi_generate_tro_shell}} for creating calculation instructions
#'
#' \code{\link{chi_qa_tro}} for validating results
#'
#' \code{\link{chi_generate_metadata}} for creating metadata from results
#'
#' @importFrom data.table setDT copy setnames := setorder set .SD data.table setcolorder
#' @importFrom rads calc suppress round2
#' @importFrom future.apply future_lapply
#' @importFrom stats na.omit qnorm
#' @import progressr
#' @export
#'
#'
chi_calc <- function(ph.data = NULL,
                     ph.instructions = NULL,
                     ci = 0.90,
                     rate = FALSE,
                     rate_per = NULL,
                     small_num_suppress = TRUE,
                     suppress_low = 0,
                     suppress_high = 9,
                     source_name = NULL,
                     source_date = NULL,
                     non_chi_byvars = NULL){
  # Input validation ----
    if (is.null(ph.data)) stop("\n\U1F6D1 ph.data must be provided")
    if (!is.data.frame(ph.data)) stop("\n\U1F6D1 ph.data must be a data.frame or data.table")
    if (nrow(ph.data) == 0) stop("\n\U1F6D1 ph.data is empty")
    if (!is.data.table(ph.data)) setDT(ph.data)

    if (is.null(ph.instructions)) stop("\n\U1F6D1 ph.instructions must be provided")
    if (!is.data.frame(ph.instructions)) stop("\n\U1F6D1 ph.instructions must be a data.frame or data.table")
    if (nrow(ph.instructions) == 0) stop("\n\U1F6D1 ph.instructions is empty")
    if (!is.data.table(ph.instructions)) setDT(ph.instructions)

    # Validate year range in ph.instructions
    if (nrow(ph.instructions[end > max(ph.data[['chi_year']], na.rm = T), ]) > 0){
      warning("\u26A0\ufe0f There are rows in ph.instructions where the end year is > the maximum chi_year in ph.data.\n",
              "These rows have been dropped from your instructions.", immediate. = TRUE)
      ph.instructions <- ph.instructions[!(end > max(ph.data[['chi_year']], na.rm = T)),]
    }

    if (nrow(ph.instructions[start < min(ph.data[['chi_year']], na.rm = T), ]) > 0){
      warning("\u26A0\ufe0f There are rows in ph.instructions where the start year is < the minimum chi_year in ph.data.\n",
              "These rows have been dropped from your instructions.", immediate. = TRUE)
      ph.instructions <- ph.instructions[!(start < min(ph.data[['chi_year']], na.rm = T)),]
    }

    # Validate ci parameter
    if (!is.numeric(ci)) stop("\n\U1F6D1 ci must be numeric")
    if (ci <= 0 || ci >= 1) stop("\n\U1F6D1 ci must be between 0 and 1")

    # Validate rate-related parameters
    if (!is.logical(rate)) stop("\n\U1F6D1 rate must be logical (TRUE/FALSE)")
    if (rate && is.null(rate_per)) stop("\n\U1F6D1 rate_per must be provided when rate=TRUE")
    if (rate && !is.numeric(rate_per)) stop("\n\U1F6D1 rate_per must be numeric")
    if (rate && rate_per <= 0) stop("\n\U1F6D1 rate_per must be positive")

    # Validate suppression parameters
    if (!is.logical(small_num_suppress)) stop("\n\U1F6D1 small_num_suppress must be logical (TRUE/FALSE)")
    if (!is.numeric(suppress_low)) stop("\n\U1F6D1 suppress_low must be numeric")
    if (!is.numeric(suppress_high)) stop("\n\U1F6D1 suppress_high must be numeric")
    if (suppress_low >= suppress_high) stop("\n\U1F6D1 suppress_low must be less than suppress_high")

    # Validate source_name
    if (is.null(source_name)) stop("\n\U1F6D1 source_name must be provided")
    if (!is.character(source_name)) stop("\n\U1F6D1 source_name must be a character string")
    if (nchar(trimws(source_name)) == 0) stop("\n\U1F6D1 source_name cannot be an empty string")

    # validate source_date
    if (is.null(source_date)) stop("\n\U1F6D1 source_date must be provided")
    if (!inherits(source_date, "Date")) stop("\n\U1F6D1 source_date must be a be a Date object")

    # Validate non_chi_byvars
    if(!is.null(non_chi_byvars)) {
      if(!is.character(non_chi_byvars)) stop("\n\U1F6D1 non_chi_byvars must be a character vector")
    }

  # Create 'Overall' if needed for crosstabs ----
    if(!'overall' %in% names(ph.data)){
      ph.data$overall <- with(ph.data, ifelse(chi_geo_kc == 'King County', 'Overall', NA_character_))
    }

  # Check to make sure all variables needed exist in the data ----
  neededbyvars <- unique(na.omit(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)))

  # Handle the race3/race3_hispanic relationship
  if("race3" %in% neededbyvars & !"race3_hispanic" %in% neededbyvars) {
    neededbyvars <- c(neededbyvars, 'race3_hispanic')
    message("\U00002139 Note: Adding 'race3_hispanic' as a required variable because 'race3' is present. By definition, race3 requires separate Hispanic ethnicity information.")
  }

  if(!"race3" %in% neededbyvars & "race3_hispanic" %in% neededbyvars) {
    neededbyvars <- c(neededbyvars, 'race3')
    message("\U00002139 Note: Adding 'race3' as a required variable because 'race3_hispanic' is present. These two variables work together to represent race/ethnicity.")
  }

  neededvars <- unique(na.omit(c(ph.instructions$indicator_key, neededbyvars)))

  missingvars <- setdiff(neededvars, names(ph.data))
  if(length(missingvars) > 0 ){
    stop(paste0("\n\U2620 ph.data is missing the following columns that are required: ", paste0(missingvars, collapse = ', ')))
  } else{message("\U0001f642 All specified variables exist in ph.data")}

  # Check to make sure all byvariables have the CHI specified encoding ----
    # Filter out non-CHI variables if specified
      chi_byvars <- neededbyvars
      if(!is.null(non_chi_byvars)) {
        chi_byvars <- setdiff(neededbyvars, non_chi_byvars)
        if(length(setdiff(non_chi_byvars, neededbyvars)) > 0) {
          message("\U00002139 Note: Some specified non_chi_byvars are not used in the analysis: ",
                  paste0(setdiff(non_chi_byvars, neededbyvars), collapse = ", "))
        }
        if(length(setdiff(neededbyvars, chi_byvars)) > 0) {
          message("\U00002139 Note: The following variables will be excluded from CHI encoding validation: ",
                  paste0(setdiff(neededbyvars, chi_byvars), collapse = ", "))
        }
      }

    # Only validate CHI variables
      stdbyvars <- rads.data::misc_chi_byvars[varname %in% unique(na.omit(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)))]
      stdbyvars <- stdbyvars[!varname %in% non_chi_byvars][, list(varname, group, keepme, reference = 1)]
      stdbyvars[group %in% c("Hispanic", 'Non-Hispanic') & varname == 'race3', varname := 'race3_hispanic'] # necessary because race3 & Hispanic must be two distinct variables in raw data

      phbyvars <- rbindlist(lapply(
        X=as.list(chi_byvars),
        FUN = function(X){data.table::data.table(varname = X, group = unique(na.omit(ph.data[[X]])), ph.data = 1)}))

    # Skip validation if there are no CHI variables to validate after excluding non_chi_byvars
      if(nrow(phbyvars) > 0 && nrow(stdbyvars) > 0) {
        compbyvars <- merge(stdbyvars, phbyvars, by = c('varname', 'group'), all = T)
        if(nrow(compbyvars[is.na(reference) | is.na(ph.data)]) > 0){
          print(compbyvars[is.na(reference) | is.na(ph.data)])
          stop("\n\U2620 the table above shows the varname/group combinations that do not align between the reference table and your ph.data.")
        } else {message("\U0001f642 All specified cat1_group and cat2_group values align with the reference standard.")}
      } else if(length(chi_byvars) > 0) {
        message("\U00002139 Note: No CHI variables to validate after excluding non_chi_byvars.")
      } else {
        message("\U00002139 Note: No variables to validate for CHI encoding.")
      }

  # Use rads::calc to generate estimates for each row of ph.instructions ----
  message("\U023F3 Be patient! The function is generating estimates for each row of ph.instructions.")

  progressr::handlers(handler_progress())
  with_progress({
    p <- progressor(nrow(ph.instructions))
      tempCHIest <- rbindlist(future_lapply(
        X = as.list(seq(1, nrow(ph.instructions), 1)),
        FUN = function(X){
          p(sprintf("Processing row %d of %d", X, nrow(ph.instructions)))

          # get the current row
          current_row <- ph.instructions[X, ]

          # create constants for calc()----
          tempbv1 <- setdiff(current_row$cat1_varname, c())
          tempbv2 <- setdiff(current_row$cat2_varname, c())
          if(length(tempbv2) == 0){tempbv2 = NA}

          # create variables of interest used in calc function below
          tempbv <- unique(na.omit(c(tempbv1, tempbv2)))
          tempend <- current_row$end
          tempstart <- current_row$start
          temptab <- current_row$tab

          # use calc()----
          if(rate == FALSE){ # standard proportion analysis
            if(temptab == '_wastate'){
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend],
                              what = current_row$indicator_key,
                              by = tempbv,
                              ci = ci,
                              metrics = c('mean', 'numerator', 'denominator', 'rse'))
            } else {
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County'],
                              what = current_row$indicator_key,
                              by = tempbv,
                              ci = ci,
                              metrics = c('mean', 'numerator', 'denominator', 'rse'))
            }
          }
          if(rate == TRUE){
            if(temptab == '_wastate'){
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend],
                              what = current_row$indicator_key,
                              by = tempbv,
                              ci = ci,
                              metrics = c('rate', 'numerator', 'denominator', 'rse'),
                              per = rate_per)
            } else {
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County'],
                              what = current_row$indicator_key,
                              by = tempbv,
                              ci = ci,
                              metrics = c('rate', 'numerator', 'denominator', 'rse'),
                              per = rate_per)
            }
            data.table::setnames(tempest, gsub("^rate", "mean", names(tempest)))
          }

          # add on CHI standard columns that are from ph.instructions (in order of standard results output)----
          tempest[, indicator_key := current_row$indicator_key]
          tempest[, tab := current_row$tab]
          tempest[current_row$end != current_row$start,
                  year := paste0(current_row$start, "-", current_row$end)]
          tempest[current_row$end == current_row$start,
                  year := current_row$end]
          tempest[, cat1 := current_row$cat1]
          data.table::setnames(tempest, current_row$cat1_varname, 'cat1_group')
          tempest[, cat1_varname := current_row$cat1_varname]
          tempest[, cat2 := current_row$cat2]
          if(!is.na(tempbv2) & tempbv1 != tempbv2){
            data.table::setnames(tempest, current_row$cat2_varname, 'cat2_group')} else{
              tempest[, cat2_group := NA] }
          tempest[, cat2_varname := current_row$cat2_varname]
          data.table::setnames(tempest,
                   c("mean", "mean_lower", "mean_upper", "mean_se"),
                   c("result", "lower_bound", "upper_bound", "se"))

          if(!"level" %in% names(tempest)) {
            tempest[, level := NA_character_]
          }

          # set correct data types for TSQL database
          tempest[, denominator := as.numeric(denominator)]
          tempest[, numerator := as.numeric(numerator)]

          return(tempest)
        }
      ), use.names = TRUE)
  })


  # Tidy results ----
    # When we have no events, but a valid denominator ----
    tempCHIest[
      (is.na(numerator) | numerator == 0) & !is.na(denominator) & denominator > 0,
      `:=` (
        result = 0,
        se = 0,
        lower_bound = 0,
        upper_bound = 0,
        numerator = ifelse(is.na(numerator), 0, numerator),
        rse = NA # undefined because mean will be zero
      )
    ]

    # When we have no events in the denominator ----
    tempCHIest[
      (is.na(denominator) | denominator == 0),
      `:=` (
        result = NA,
        se = NA,
        lower_bound = NA,
        upper_bound = NA,
        numerator = 0,  # This might not be necessary depending on your needs
        denominator = 0,  # Setting NA denominator to 0
        rse = NA
      )
    ]

    # drop when cat1_group is missing (e.g., cat1 == 'Regions' and region is NA) ----
    tempCHIest <- tempCHIest[!is.na(cat1_group)]

    # drop when cat2_group is missing but cat2 is not missing ----
    tempCHIest <- tempCHIest[!(is.na(cat2_group) & !is.na(cat2))]

    # Apply Wilson Score method for confidence intervals when result is 0% or 100% ----
      # This handles cases where standard methods fail at the extremes by providing more appropriate
      # bounds that don't exceed the logical limits while maintaining the specified confidence level

      # Calculate z-value based on the provided confidence interval
      z_value <- qnorm(1-0.5*(1-ci))

      # Lower bound using Wilson Score method
      tempCHIest[result %in% c(0, 1) & denominator > 10,
                 lower_bound := (2 * numerator + z_value^2 - z_value * sqrt(z_value^2 + 4 * numerator * (1 - numerator/denominator))) /
                   (2 * (denominator + z_value^2))]

      # Upper bound using Wilson Score method
      tempCHIest[result %in% c(0, 1) & denominator > 10,
                 upper_bound := (2 * numerator + z_value^2 + z_value * sqrt(z_value^2 + 4 * numerator * (1 - numerator/denominator))) /
                   (2 * (denominator + z_value^2))]

    # drop if cat1_group | cat2_group had `keepme == "No"` in the reference table ----
    dropme <- unique(stdbyvars[keepme == 'No'][, reference := NULL])
    tempCHIest <- merge(tempCHIest,
                        dropme,
                        by.x = c('cat1_varname', 'cat1_group'),
                        by.y = c('varname', 'group'),
                        all.x = T,
                        all.y = F)
    tempCHIest <- tempCHIest[is.na(keepme)][, keepme := NULL]

    tempCHIest <- merge(tempCHIest,
                        dropme,
                        by.x = c('cat2_varname', 'cat2_group'),
                        by.y = c('varname', 'group'),
                        all.x = T,
                        all.y = F)
    tempCHIest <- tempCHIest[is.na(keepme)][, keepme := NULL]

    # change all NaN to a normal NA or SQL will vomit ----
    for(col in names(tempCHIest)) set(tempCHIest, i=which(is.nan(tempCHIest[[col]])), j=col, value=NA)

    # apply rounding rules for proportions----
    if(rate == FALSE){
      tempCHIest[, c("result", "lower_bound", "upper_bound", "rse") := lapply(.SD, round2, 3), .SDcols = c("result", "lower_bound", "upper_bound", "rse")]
      tempCHIest[, c("se") := lapply(.SD, round2, 4), .SDcols = c("se")]
    }

    # apply rounding rules for rates----
    if(rate == TRUE){
      tempCHIest[, c("result", "lower_bound", "upper_bound") := lapply(.SD, round2, 1), .SDcols = c("result", "lower_bound", "upper_bound")]
      tempCHIest[, c("rse") := lapply(.SD, round2, 3), .SDcols = c("rse")]
      tempCHIest[, c("se") := lapply(.SD, round2, 2), .SDcols = c("se")]
    }

    # prevent negative lower CI ----
    tempCHIest[lower_bound < 0, lower_bound := 0]

    # race3/race4 messiness ----
    tempCHIest[cat1_varname == 'race3_hispanic', cat1_varname := 'race3']
    tempCHIest[cat2_varname == 'race3_hispanic', cat2_varname := 'race3']

    if(any(grepl("Birthing per", unique(tempCHIest$cat1)))){
      tempCHIest[cat1_varname %in% c("race3", "race4") & tab == 'trends', cat1 := "Birthing person's race/ethnicity"]
      tempCHIest[cat2_varname %in% c("race3", "race4") & tab == 'trends', cat2 := "Birthing person's race/ethnicity"]

      tempCHIest[cat1_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat1_group == 'Hispanic',
                 cat1 := "Birthing person's ethnicity"]
      tempCHIest[cat2_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat2_group == 'Hispanic',
                 cat2 := "Birthing person's ethnicity"]
    } else {
      tempCHIest[cat1_varname %in% c("race3", "race4") & tab == 'trends', cat1 := "Race/ethnicity"]
      tempCHIest[cat2_varname %in% c("race3", "race4") & tab == 'trends', cat2 := "Race/ethnicity"]

      tempCHIest[cat1_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat1_group == 'Hispanic',
                 cat1 := 'Ethnicity']
      tempCHIest[cat2_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat2_group == 'Hispanic',
                 cat2 := 'Ethnicity']
    }

    # Create additional necessary CHI columns ----
    tempCHIest[, source_date := as.Date(source_date)]
    tempCHIest[, run_date := as.Date(Sys.Date(), "%Y%m%d")]
    tempCHIest[, chi := 1] # set to 1 by default, because this is for CHI. Manually set to zero when needed afterward
    tempCHIest[tab == 'metadata', chi := 0]
    tempCHIest[, data_source := source_name]

    if(small_num_suppress == TRUE){
      tempCHIest <- apde.chi.tools::chi_suppress_results(ph.data = tempCHIest,
                                                         suppress_range = c(suppress_low, suppress_high),
                                                         secondary = T,
                                                         secondary_exclude = cat1_varname != 'race3')
    } else {tempCHIest[, suppression := NA_character_]}

    tempCHIest[rse>=30 | numerator == 0, caution := "!"]

    tempCHIest[, c('cat2', 'cat2_group', 'cat2_varname') := lapply(.SD, as.character), .SDcols = c('cat2', 'cat2_group', 'cat2_varname')]


    # Keep and order standard CHI columns ----
    all_cols <- c(chi_get_cols(), "level")
    all_cols <- unique(all_cols)  # In case level is already included
    tempCHIest <- tempCHIest[, all_cols, with = F]

    tempCHIest <- tempCHIest[, cat1 := factor(cat1, levels = c("King County", sort(setdiff(unique(tempCHIest$cat1), "King County"))) )]
    tempCHIest <- tempCHIest[, tab := factor(tab, levels = c(c("_kingcounty","demgroups", "trends"),  sort(setdiff(unique(tempCHIest$tab), c("_kingcounty","demgroups", "trends")))) )]
    setorder(tempCHIest, indicator_key, tab, -year, cat1, cat1_group, cat2, cat2_group)
    setcolorder(tempCHIest, c('data_source', 'indicator_key', 'level'))

  # return the CHI table ----
  return(tempCHIest)
}
