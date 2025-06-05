#' CHI Generate Trend Years
#'
#' @description
#' \strong{Internal}, used by \code{\link{chi_generate_tro_shell}}
#'
#' @param indicator_key chi indicator key variable
#' @param trend.span the number of years to be included in a single trend period
#' @param end.year last year of a trend year time series
#' @param trend.periods number of periods to calculate
#'
#' @details
#' This function is used internally by \code{\link{chi_generate_analysis_set}}
#' to create instructions for trend analyses.
#'
#' @return Returns a data table of trend calculation instructions with columns:
#' \itemize{
#'   \item \code{indicator_key} Unique identifier for each indicator
#'   \item \code{tab} Always \code{'trends'}
#'   \item \code{cat1} A standard value from the \code{'cat'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{cat1_varname} A standard value from the \code{'varname'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{cat2} A standard value from the \code{'cat'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{cat2_varname} A standard value from the \code{'varname'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{start} Starting year of the time period to be analyzed
#'   \item \code{end} Ending year of the time period to be analyzed
#' }
#'
#' @seealso
#' \code{\link{chi_generate_tro_shell}}, which calls on this function to create
#' ph.instructions
#'
#' \code{\link{chi_process_nontrends}}, which is a counterpart to this function and
#' is also an internal function used by \code{\link{chi_generate_analysis_set}}
#'
#' @keywords CHI, Tableau, Production, internal
#' @importFrom data.table setDT setorder
#' @importFrom tidyr crossing

chi_process_trends <- function(indicator_key = NULL,
                                     trend.span = NULL,
                                     end.year = NULL,
                                     trend.periods = NULL){
  last.start <- end.year-(trend.span-1)
  all.start.years <- last.start:(last.start-(trend.periods-1))
  all.end.years <- end.year:(end.year-(trend.periods-1))
  spandt <- data.table(end = all.end.years, start = all.start.years)
  spandt <- setorder(setDT(tidyr::crossing(data.table(indicator_key), spandt)), indicator_key, -end)
  return(spandt)
}
