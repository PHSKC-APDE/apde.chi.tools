#' CHI generate TRO shell
#'
#' @description
#' This function takes an analysis set table created by [chi_generate_analysis_set()]
#' and specified time period parameters to generate a complete set of analysis
#' instructions which can be used by [chi_calc()], [chi_count_by_age()],
#' [chi_chars_ccs()], and [chi_chars_injury()].
#'
#' @details
#' The expected format of `ph.analysis_set` is:
#'
#' - `cat1`: character. A standard value from the `'cat'` column
#'   of [rads.data::misc_chi_byvars], e.g. `'Gender'`
#' - `cat1_varname`: character. A standard value from the `'varname'`
#'   column of [rads.data::misc_chi_byvars], e.g., `'chi_sex'`
#' - `_kingcounty`: character `'x'`. Is the analysis for King County as a whole?
#' - `_wastate`: character `'x'`. Is the analysis is for WA state as a whole?
#' - `demgroups`: character `'x'`. Is the analysis for demgroups?
#' - `crosstabs`: character `'x'`. Is the analysis for crosstabs?
#' - `trends`: character `'x'`, Is the the analysis for trends?
#' - `set`: Integer identifying groups of indicators with identical analysis patterns
#' - `set_indicator_keys`: Comma-separated list of indicator keys sharing the pattern
#'
#' @param ph.analysis_set name of a formal `analysis_set` as described in the
#' **Details** section below
#' @param end.year the latest year to be used for aggregate estimates
#' @param year.span the number of years to be included in a single non-trend period.
#' e.g. if `end.year = 2023` & `year.span = 5`, calculations would be
#' performed for 2019-2023
#' @param trend.span the number of years to be included in a single trend period
#' @param trend.periods the number of periods to be included in a trend series,
#' e.g., if `end.year = 2023` & `trend.span = 2` & `trend.periods = 5`,
#' then trends would be calculated for c('2023-2022', '2022-2021', '2021-2020',
#' '2019-2020', '2018-2019')
#'
#' @return Returns a data table of calculation instructions with columns:
#'
#' - `indicator_key` Unique identifier for each indicator
#' - `tab` Analysis type (_kingcounty, demgroups, trends, etc.)
#' - `cat1` A standard value from the `'cat'` column of [rads.data::misc_chi_byvars]
#' - `cat1_varname` A standard value from the `'varname'` column of [rads.data::misc_chi_byvars]
#' - `cat2` A standard value from the `'cat'` column of [rads.data::misc_chi_byvars]
#' - `cat2_varname` A standard value from the `'varname'` column of [rads.data::misc_chi_byvars]
#' - `start` Starting year of the time period to be analyzed
#' - `end` Ending year of the time period to be analyzed
#'
#' @seealso
#' [chi_generate_analysis_set()] for creating analysis sets
#'
#' [chi_calc()], [chi_count_by_age()], [chi_chars_ccs()],
#' and [chi_chars_injury()] for performing
#' calculations using the output of `chi_generate_tro_shell`
#'
#' @keywords CHI, Tableau, Production
#'
#' @export
#'
chi_generate_tro_shell <- function(ph.analysis_set,
                                   end.year,
                                   year.span = NULL,
                                   trend.span = NULL,
                                   trend.periods = NULL){

  # Input validation
  if (missing(ph.analysis_set)) stop("\n\U1F6D1 ph.analysis_set must be provided")
  if (!is.data.frame(ph.analysis_set)) stop("\n\U1F6D1 ph.analysis_set must be a data.frame or data.table")
  if (!("set" %in% names(ph.analysis_set)) | anyNA(ph.analysis_set$set)) {
    stop("\n\u1F6D1 set number must be provided for all rows")
  }
  if (missing(end.year)) stop("\n\U1F6D1 end.year must be provided")
  if (!is.numeric(end.year) || length(end.year) != 1) stop("\n\U1F6D1 end.year must be a single numeric value")

  if (!is.null(year.span) && (!is.numeric(year.span) || length(year.span) != 1)) {
    stop("\n\U1F6D1 year.span must be NULL or a single numeric value")
  }

  if (!is.null(trend.span) && (!is.numeric(trend.span) || length(trend.span) != 1)) {
    stop("\n\U1F6D1 trend.span must be NULL or a single numeric value")
  }

  if (!is.null(trend.periods) && (!is.numeric(trend.periods) || length(trend.periods) != 1)) {
    stop("\n\U1F6D1 trend.periods must be NULL or a single numeric value")
  }


  # Convert to data.table if needed
  if (!data.table::is.data.table(ph.analysis_set)) data.table::setDT(ph.analysis_set)

  # parameterization checks
  if("x" %in% ph.analysis_set$trends & (is.null(trend.span) | is.null(trend.periods))) {stop("you have indicated that a trends analysis is to be conducted, but have not indicated both the span and number of periods for this analysis.")}

  # advisory messages
  if("x" %in% ph.analysis_set$trends) {message("Note: trends are applied backwards from end.year")}

  # Race / ethnicity is a chronic headache with CHI. Need to remove rows for race4 & Ethnicity because should be Race/ethnicity
  ph.analysis_set <- ph.analysis_set[!(cat1_varname == 'race4' & cat1 == 'Ethnicity')]

  # apply the template generating function
  # generate vector of sets
  sets <- unique(ph.analysis_set$set)
  template <- data.table::rbindlist(
    lapply(X = sets,
           FUN = chi_process_nontrends, ph.analysis_set = ph.analysis_set))

  # split trends from other tabs because processed for multiple years
  template.trends <- template[tab=='trends']
  if(nrow(template.trends) > 0){
    template <- template[tab != 'trends']
  }

  # add years to template (non-trend)
  template[, end := end.year]
  template[, start := end.year - (year.span - 1)]
  template <- rbind(template,
                    template[tab == '_kingcounty'][, tab := 'metadata'][, start := end.year])

  # add years to template (trends)
  if(nrow(template.trends) > 0){
    trend.years <- chi_process_trends(indicator_key = intersect(unique(template$indicator_key), unique(template.trends$indicator_key)),
                                      trend.span = trend.span,
                                      end.year = end.year,
                                      trend.periods = trend.periods)
    template.trends <- merge(template.trends, trend.years, by = 'indicator_key', all = T, allow.cartesian = T)

    # append trends template to main template
    template <- rbind(template, template.trends)
  }

  return(template)
}


