#' CHI generate nontrend years
#'
#' @description
#' \strong{Internal}, used by \code{\link{chi_generate_tro_shell}}
#'
#' @details
#' This function takes an analysis set table created by \code{\link{chi_generate_analysis_set}}
#' and returns analysis instructions for non-trend analyses.
#'
#' The expected format of \code{ph.analysis_set} is:
#'   \itemize{
#'     \item \code{cat1}: character. A standard value from the \code{'cat'} column
#'     of \code{\link[rads.data]{misc_chi_byvars}}, e.g. \code{'Gender'}
#'     \item \code{cat1_varname}: character. A standard value from the \code{'varname'}
#'     column of \code{\link[rads.data]{misc_chi_byvars}}, e.g., \code{'chi_sex'}
#'     \item \code{_kingcounty}: character \code{'x'}. Is the analysis for King County as a whole?
#'     \item \code{_wastate}: character \code{'x'}. Is the analysis is for WA state as a whole?
#'     \item \code{demgroups}: character \code{'x'}. Is the analysis for demgroups?
#'     \item \code{crosstabs}: character \code{'x'}. Is the analysis for crosstabs?
#'     \item \code{trends}: character \code{'x'}, Is the the analysis for trends?
#'     \item \code{set}: Integer identifying groups of indicators with identical analysis patterns
#'     \item \code{set_indicator_keys}: Comma-separated list of indicator keys sharing the pattern
#'   }
#'
#' @param ph.analysis_set name of a formal \code{analysis_set} as described in the
#' \strong{Details} section below
#' @param myset chosen set number from table
#'
#' @seealso
#' \code{\link{chi_generate_tro_shell}}, which calls on this function to create
#' ph.instructions
#'
#' \code{\link{chi_process_trends}}, which is a counterpart to this function and
#' is also an internal function used by \code{\link{chi_generate_analysis_set}}
#'
#'
#' @return Returns a data table of calculation instructions with columns:
#' \itemize{
#'   \item \code{indicator_key} Unique identifier for each indicator
#'   \item \code{tab} Analysis type (_kingcounty, demgroups, etc.)
#'   \item \code{cat1} A standard value from the \code{'cat'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{cat1_varname} A standard value from the \code{'varname'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{cat2} A standard value from the \code{'cat'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{cat2_varname} A standard value from the \code{'varname'} column of \code{\link[rads.data]{misc_chi_byvars}}
#'   \item \code{start} Starting year of the time period to be analyzed
#'   \item \code{end} Ending year of the time period to be analyzed
#' }
#'
#' @keywords CHI, Tableau, Production, internal
#'
#' @importFrom data.table setDT rbindlist setcolorder `:=`
#' @import dtsurvey
#' @import future
#' @import future.apply
#' @importFrom tidyr crossing
#' @importFrom rads string_clean
#'
chi_process_nontrends <- function(ph.analysis_set = NULL,
                                  myset = NULL){

  subsets <- ph.analysis_set[set == myset] # process a single analysis set
  sub_indicators <- unlist(strsplit(unique(subsets$set_indicator_keys), ",")) # create vector of distinct indicator keys
  subtabs = setdiff(names(subsets), c('set', 'set_indicator_keys', 'cat1', 'cat1_varname', 'crosstabs')) # identify the tabs of interest

  # create table for all tabs except crosstabs
  tempy <- rbindlist(lapply(as.list(seq(1, length(subtabs))),
                            FUN = function(subtab){
                              tempx <- subsets[get(subtabs[subtab]) == 'x',
                                               list(tab = subtabs[subtab], cat1, cat1_varname, cat2 = NA_character_, cat2_varname = NA_character_)]
                              tempx <- setDT(tidyr::crossing(tempx, data.table(indicator_key = sub_indicators)))

                            }))

  # crosstabs are a bit more complicated
  sub_crosstabs = setDT(tidyr::crossing(
    unique(subsets[crosstabs == 'x', list(cat1, cat1_varname)]),
    unique(subsets[crosstabs == 'x', list(cat2 = cat1, cat2_varname = cat1_varname)]) ))
  sub_crosstabs <- sub_crosstabs[cat1 == 'King County' | cat1_varname != cat2_varname]
  sub_crosstabs <- sub_crosstabs[!(cat1_varname == 'race3' & cat2_varname %in% c('race3', 'race4'))] # do not want race x race
  sub_crosstabs <- sub_crosstabs[!(cat2_varname == 'race3' & cat1_varname %in% c('race3', 'race4'))] # do not want race x race
  sub_crosstabs <- sub_crosstabs[!(grepl('aic', cat1_varname) & grepl('race3|race4', cat2_varname))] # do not want race_aic x race
  sub_crosstabs <- sub_crosstabs[!(grepl('aic', cat2_varname) & grepl('race3|race4', cat1_varname))] # do not want race_aic x race
  sub_crosstabs <- sub_crosstabs[!(grepl('_aic_', cat1_varname) & grepl('_aic_', cat2_varname))] # do not want race_aic x race_aic

  sub_crosstabs[, tab := 'crosstabs']
  sub_crosstabs <- setDT(tidyr::crossing(sub_crosstabs, data.table(indicator_key = sub_indicators)))


  # append crosstabs
  tempy <- rbind(tempy, sub_crosstabs)

  # tidy
  tempy[cat1 %in% c('Ethnicity', "Birthing person's ethnicity") & cat1_varname == 'race3', cat1_varname := 'race3_hispanic']
  tempy[cat2 %in% c('Ethnicity', "Birthing person's ethnicity") & cat2_varname == 'race3', cat2_varname := 'race3_hispanic']
  setcolorder(tempy, 'indicator_key')
  rads::string_clean(tempy)
  tempy <- tempy[!(tab == 'crosstabs' & cat1 == 'King County' & cat2 != 'King County')] # only legit xtab for KC is KC by itself
  tempy[tab == 'crosstabs' & cat2 == 'King County', `:=` (cat2 = 'Overall', cat2_varname = 'overall')]

  # return object
  return(tempy)
}

