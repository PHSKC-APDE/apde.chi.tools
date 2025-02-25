#' CHI generate TRO shell
#'
#' @description
#' This function takes an analysis set file and indicator of which set should be
#' processed. It returns a skeleton of CHI Tableau Ready Output, which is used
#' as an instruction set for \code{\link{chi_calc}}.
#'
#' @details
#' It takes in a data.table containing a compact list of \code{indicator_keys},
#' byvariables, and analysis types, and returns a shell table of the rows and columns expected
#' in a CHI Tableau ready output. For details on TRO format, review here:
#' \href{https://kc1.sharepoint.com/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady\%20Output.xlsx}{
#' SharePoint > Community Health Indicators > CHI_vizes > CHI-Standards-TableauReady Output.xlsx}
#'
#' The expected format of \code{ph.analysis_set} is:
#'   \itemize{
#'     \item \code{set}: Integer identifying groups of indicators with identical analysis patterns
#'     \item \code{cat1}: Category name as expected in CHI TRO
#'     \item \code{cat1_varname}: Category variable name as expected in CHI TRO
#'     \item \code{_kingcounty}: \code{'x'} if analysis includes King County tab
#'     \item \code{_wastate}: \code{'x'} if analysis includes Washington State tab
#'     \item \code{demgroups}: \code{'x'} if analysis includes demographic groups
#'     \item \code{crosstabs}: \code{'x'} if analysis includes crosstabulations
#'     \item \code{trends}: \code{'x'} if analysis includes trends analysis
#'     \item \code{set_indicator_keys}: Comma-separated list of indicator keys sharing the pattern
#'   }
#'
#' @param ph.analysis_set name of data.table to parse
#' @param start.year the earliest year to be used for estimates
#' @param end.year the latest year to be used for aggregate estimates
#' @param year.span the number of years to be included in a single non-trend period
#' @param trend.span the number of years to be included in a single trend period
#' @param trend.periods the number of periods to be included in a trend
#'
#' @return A data table containing calculation instructions with columns:
#' \itemize{
#'   \item{indicator_key} Unique identifier for each indicator
#'   \item{tab} Analysis type (_kingcounty, demgroups, trends, etc.)
#'   \item{year} Time period for analysis
#'   \item{Additional columns} As specified in analysis_set input
#' }
#'
#' @seealso
#' \code{\link{chi_generate_analysis_set}} for creating analysis sets
#'
#' \code{\link{chi_calc}} for performing calculations using these instructions
#'
#' @keywords CHI, Tableau, Production
#'
#' @importFrom data.table rbindlist
#' @import future
#' @import future.apply
#'
#' @export
#'
chi_generate_tro_shell <- function(ph.analysis_set,
                                      start.year,
                                      end.year,
                                      year.span = NULL,
                                      trend.span = NULL,
                                      trend.periods = NULL){

  # Input validation
    if (missing(ph.analysis_set)) stop("\n\U1F6D1 ph.analysis_set must be provided")
    if (!is.data.frame(ph.analysis_set)) stop("\n\U1F6D1 ph.analysis_set must be a data.frame or data.table")

    if (missing(start.year)) stop("\n\U1F6D1 start.year must be provided")
    if (!is.numeric(start.year) || length(start.year) != 1) stop("\n\U1F6D1 start.year must be a single numeric value")

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
    if (!is.data.table(ph.analysis_set)) setDT(ph.analysis_set)

  #parameterization checks
  if("x" %in% ph.analysis_set$trends & (is.null(trend.span) | is.null(trend.periods))) {stop("you have indicated that a trends analysis is to be conducted, but have not indicated both the span and number of periods for this analysis.")}

  #ph.analysis_set checks


  #advisory messages
  if("x" %in% ph.analysis_set$trends) {message("Note: trends are applied backwards from end.year")}


  # apply the template generating function
  template <- rbindlist(
    lapply(X = seq(1, length(unique(ph.analysis_set$set))),
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


