#' CHI generate nontrend years
#'
#' @description
#' This function takes an analysis set file and indicator of which set should be
#' processed. It returns a skeleton of CHI Tableau Ready Output. Hidden and meant
#' to be called by CHI generate instructions. Not exported, but called by
#' chi_generate_tro_shell().
#'
#' @details
#' It takes in a data.table containing a compact list of variables, byvariables,
#' and analysis types, and returns a shell table of the rows and columns expected
#' in a CHI Tableau ready output. For details on TRO format, review here:
#' \href{https://kc1.sharepoint.com/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady\%20Output.xlsx}{
#' SharePoint > Community Health Indicators > CHI_vizes > CHI-Standards-TableauReady Output.xlsx}
#'
#' The expected format of the analysis file is:
#' \itemize{
#'    \item \code{set}: numeric integer 1...x, indicates set or batch of analyses since not all indicators have all the same analyses
#'    \item \code{cat1}: character, the name expected in CHI TRO for cat1
#'    \item \code{cat1_varname}: character, the name expected in CHI TRO for cat1_varname
#'    \item \code{_kingcounty}: character "":"X", indicator of if analysis is King County specific (could be removed, this is imputable by variable name)
#'    \item \code{_wastate}: character "":"x", indicator of if analysis is of WA state
#'    \item \code{demgroups}: character "":"x", indicator of if analysis includes single demographic
#'    \item \code{crosstabs}: character "":"x", indicator of if analysis includes crosstabulations
#'    \item \code{trends}: character "":"x", indicator of if analysis includes trends
#'    \item \code{set_indictaor_keys} character comma sep list, list of indicators variables expected from data source
#' }
#'
#' @param ph.analysis_set name of data.table to parse
#' @param myset chosen set number from table
#'
#' @returns data table with a single row for each calculation to be performed in generating Tableau Ready Output for CHI reporting
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

