#' Calculate CHI Estimates
#'
#' @description
#' Generates CHI estimates from input data according to provided instructions
#' created by \code{\link{chi_generate_tro_shell}}.
#' Handles both proportions and rates, with options for suppression of small numbers.
#'
#' @param ph.data Input data.frame or data.table containing analytic read data
#' @param ph.instructions data.frame or data.table containing calculation instructions
#' @param rate Logical; if \code{TRUE} calculates rates, if \code{FALSE} calculates proportions
#' @param rate_per Rate multiplier when \code{rate=TRUE} (e.g., 100000 for per 100,000)
#' @param small_num_suppress Logical; if \code{TRUE} suppresses small numbers
#' @param suppress_low Lower bound for suppression
#' @param suppress_high Upper bound for suppression
#' @param source_name Name of data source
#' @param source_date Date of data source
#'
#' @return A data.table containing CHI estimates with the following columns:
#' \itemize{
#'   \item{\code{data_source}} Data source (e.g., acs, brfss, etc.)
#'   \item{\code{indicator_key}} Unique indicator key
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
#'   \item{\code{numerator}} For line-level data, population; for surveys, sample size
#'   \item{\code{chi}} '1' indicates that rows is used for CHI
#'   \item{\code{source_date}} date analytic ready data was created
#'   \item{\code{run_date}} date of this analysis
#' }
#'
#' @seealso
#' \code{\link{chi_generate_tro_shell}} for creating calculation instructions
#'
#' \code{\link{chi_qa_tro}} for validating results
#'
#' \code{\link{chi_generate_metadata}} for creating metadata from results
#'
#' @importFrom data.table setDT copy setnames := setorder set .SD data.table
#' @importFrom rads calc compare_estimate suppress chi_cols
#' @importFrom future.apply future_lapply
#' @importFrom stats na.omit
#' @import progressr
#' @export
#'
#'
chi_calc <- function(ph.data = NULL,
                     ph.instructions = NULL,
                     rate = F,
                     rate_per = NULL,
                     small_num_suppress = T,
                     suppress_low = 0,
                     suppress_high = 9,
                     source_name = 'blahblah',
                     source_date = NULL){
  # Input validation ----
    if (is.null(ph.data)) stop("\n\U1F6D1 ph.data must be provided")
    if (!is.data.frame(ph.data)) stop("\n\U1F6D1 ph.data must be a data.frame or data.table")

    if (is.null(ph.instructions)) stop("\n\U1F6D1 ph.instructions must be provided")
    if (!is.data.frame(ph.instructions)) stop("\n\U1F6D1 ph.instructions must be a data.frame or data.table")

    if (!is.logical(rate)) stop("\n\U1F6D1 rate must be logical (TRUE/FALSE)")
    if (rate && is.null(rate_per)) stop("\n\U1F6D1 rate_per must be provided when rate=TRUE")

    if (!is.logical(small_num_suppress)) stop("\n\U1F6D1 small_num_suppress must be logical (TRUE/FALSE)")

    # Convert to data.table if needed
    if (!is.data.table(ph.data)) setDT(ph.data)
    if (!is.data.table(ph.instructions)) setDT(ph.instructions)

  # Error if ph.instructions has no data ----
    if(nrow(ph.instructions) == 0){
      stop("\n\U0001f47f the table ph.instructions does not have any rows.")
      #tempCHIest <- data.table(setNames(data.frame(matrix(ncol = length(chi_cols()), nrow = 0), stringsAsFactors = FALSE), chi_cols()))
    }

  # Create 'Overall' if needed for crosstabs ----
    if(!'overall' %in% names(ph.data)){
      ph.data$overall <- with(ph.data, ifelse(chi_geo_kc == 'King County', 'Overall', NA_character_))
    }

  # Check to make sure all variables needed exist in the data ----
  neededbyvars <- unique(na.omit(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)))
  if("race3" %in% neededbyvars & !"race3_hispanic" %in% neededbyvars){neededbyvars <- c(neededbyvars, 'race3_hispanic')} # By definition, Hispanic cannot be contained within race3

  neededvars <- unique(na.omit(c(ph.instructions$indicator_key, neededbyvars)))

  missingvars <- setdiff(neededvars, names(ph.data))
  if(length(missingvars) > 0 ){
    stop(paste0("\n\U2620 ph.data is missing the following columns that are specified in ph.instructions: ", paste0(missingvars, collapse = ', '), ". ",
                "\nIf `race3_hispanic` is listed, that is because, by definition, `race3` cannot have a Hispanic ethnicity in the same variable. So, two ",
                "\nvariables (`race3` & `race3_hispanic`) will be processed and in the output, it will be called `race3`"))
  } else{message("\U0001f642 All specified variables exist in ph.data")}

  # Check to make sure all byvariables have the CHI specified encoding ----
  stdbyvars <- rads.data::misc_chi_byvars[varname %in% unique(na.omit(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)))][, list(varname, group, keepme, reference = 1)]
  stdbyvars[group %in% c("Hispanic", 'Non-Hispanic') & varname == 'race3', varname := 'race3_hispanic'] # necessary because race3 & Hispanic must be two distinct variables in raw data
  phbyvars <- rbindlist(lapply(
    X=as.list(neededbyvars),
    FUN = function(X){data.table::data.table(varname = X, group = unique(na.omit(ph.data[[X]])), ph.data = 1)}))
  compbyvars <- merge(stdbyvars, phbyvars, by = c('varname', 'group'), all = T)
  if(nrow(compbyvars[is.na(reference)| is.na(ph.data)]) > 0){
    print(compbyvars[is.na(reference)| is.na(ph.data)])
    stop("\n\U2620 the table above shows the varname/group combinations that do not align between the reference table and your ph.data.")
  } else {message("\U0001f642 All specified cat1_group and cat2_group values align with the reference standard.")}

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

          # use calc()----
          if(rate == FALSE){ # standard proportion analysis
            if(any(grepl('wastate', tempbv))){
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend],
                              what = current_row$indicator_key,
                              by = tempbv,
                              metrics = c('mean', 'numerator', 'denominator', 'rse'))
            } else {
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County'],
                              what = current_row$indicator_key,
                              by = tempbv,
                              metrics = c('mean', 'numerator', 'denominator', 'rse'))
            }
          }
          if(rate == TRUE){
            if(any(grepl('wastate', tempbv))){
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend],
                              what = current_row$indicator_key,
                              by = tempbv,
                              metrics = c('rate', 'numerator', 'denominator', 'rse'),
                              per = rate_per)
            } else {
              tempest <- rads::calc(ph.data = ph.data[chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County'],
                              what = current_row$indicator_key,
                              by = tempbv,
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

          # set correct data types for TSQL database
          tempest[, denominator := as.numeric(denominator)]
          tempest[, numerator := as.numeric(numerator)]

          return(tempest)
        }
      ), use.names = TRUE)
  })


  # Tidy results ----
    # drop when cat1_group is missing (e.g., cat1 == 'Regions' and region is NA) ----
    tempCHIest <- tempCHIest[!is.na(cat1_group)]

    # drop when cat2_group is missing but cat2 is not missing ----
    tempCHIest <- tempCHIest[!(is.na(cat2_group) & !is.na(cat2))]

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
      tempCHIest <- rads::suppress(sup_data = tempCHIest,
                                   suppress_range = c(suppress_low, suppress_high),
                                   secondary = T,
                                   secondary_exclude = cat1_varname != 'race3')
    } else {tempCHIest[, suppression := NA_character_]}

    tempCHIest[rse>=30, caution := "!"]

    tempCHIest[, c('cat2', 'cat2_group', 'cat2_varname') := lapply(.SD, as.character), .SDcols = c('cat2', 'cat2_group', 'cat2_varname')]


    # Keep and order standard CHI columns ----
    tempCHIest <- tempCHIest[, chi_get_cols(), with = F]

    tempCHIest <- tempCHIest[, cat1 := factor(cat1, levels = c("King County", sort(setdiff(unique(tempCHIest$cat1), "King County"))) )]
    tempCHIest <- tempCHIest[, tab := factor(tab, levels = c(c("_kingcounty","demgroups", "trends"),  sort(setdiff(unique(tempCHIest$tab), c("_kingcounty","demgroups", "trends")))) )]
    setorder(tempCHIest, indicator_key, tab, -year, cat1, cat1_group, cat2, cat2_group)

  # return the CHI table ----
  return(tempCHIest)
}
