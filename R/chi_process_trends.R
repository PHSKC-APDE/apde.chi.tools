#' CHI Generate Trend Years
#'
#' @description
#' **Internal**, used by [chi_generate_tro_shell()]
#'
#' @param indicator_key chi indicator key variable
#' @param trend.span the number of years to be included in a single trend period
#' @param end.year last year of a trend year time series
#' @param trend.periods number of periods to calculate
#'
#' @details
#' This function is used internally by [chi_generate_analysis_set()]
#' to create instructions for trend analyses.
#'
#' @return Returns a data table of trend calculation instructions with columns:
#'
#' - `indicator_key` Unique identifier for each indicator
#' - `tab` Always `'trends'`
#' - `cat1` A standard value from the `'cat'` column of [rads.data::misc_chi_byvars]
#' - `cat1_varname` A standard value from the `'varname'` column of [rads.data::misc_chi_byvars]
#' - `cat2` A standard value from the `'cat'` column of [rads.data::misc_chi_byvars]
#' - `cat2_varname` A standard value from the `'varname'` column of [rads.data::misc_chi_byvars]
#' - `start` Starting year of the time period to be analyzed
#' - `end` Ending year of the time period to be analyzed
#'
#' @seealso
#' [chi_generate_tro_shell()], which calls on this function to create
#' ph.instructions
#'
#' [chi_process_nontrends()], which is a counterpart to this function and
#' is also an internal function used by [chi_generate_analysis_set()]
#'
#' @keywords CHI, Tableau, Production, internal

chi_process_trends <- function(indicator_key = NULL,
                                     trend.span = NULL,
                                     end.year = NULL,
                                     trend.periods = NULL){
  last.start <- end.year-(trend.span-1)
  all.start.years <- last.start:(last.start-(trend.periods-1))
  all.end.years <- end.year:(end.year-(trend.periods-1))
  spandt <- data.table::data.table(end = all.end.years, start = all.start.years)
  spandt <- data.table::setorder(data.table::setDT(tidyr::crossing(data.table::data.table(indicator_key), spandt)), indicator_key, -end)
  return(spandt)
}
