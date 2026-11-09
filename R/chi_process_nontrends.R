#' CHI generate nontrend years
#'
#' @description
#' **Internal**, used by [chi_generate_tro_shell()]
#'
#' @details
#' This function takes an analysis set table created by [chi_generate_analysis_set()]
#' and returns analysis instructions for non-trend analyses.
#'
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
#' @param myset chosen set number from table
#'
#' @seealso
#' [chi_generate_tro_shell()], which calls on this function to create
#' ph.instructions
#'
#' [chi_process_trends()], which is a counterpart to this function and
#' is also an internal function used by [chi_generate_analysis_set()]
#'
#'
#' @return Returns a data table of calculation instructions with columns:
#'
#' - `indicator_key` Unique identifier for each indicator
#' - `tab` Analysis type (_kingcounty, demgroups, etc.)
#' - `cat1` A standard value from the `'cat'` column of [rads.data::misc_chi_byvars]
#' - `cat1_varname` A standard value from the `'varname'` column of [rads.data::misc_chi_byvars]
#' - `cat2` A standard value from the `'cat'` column of [rads.data::misc_chi_byvars]
#' - `cat2_varname` A standard value from the `'varname'` column of [rads.data::misc_chi_byvars]
#' - `start` Starting year of the time period to be analyzed
#' - `end` Ending year of the time period to be analyzed
#'
#' @keywords CHI, Tableau, Production, internal
#'
chi_process_nontrends <- function(ph.analysis_set = NULL,
                                  myset = NULL){

  subsets <- ph.analysis_set[set == myset] # process a single analysis set
  sub_indicators <- unlist(strsplit(unique(subsets$set_indicator_keys), ",")) # create vector of distinct indicator keys
  subtabs = setdiff(names(subsets), c('set', 'set_indicator_keys', 'cat1', 'cat1_varname', 'crosstabs')) # identify the tabs of interest

  # create table for all tabs except crosstabs
  tempy <- data.table::rbindlist(lapply(as.list(seq(1, length(subtabs))),
                            FUN = function(subtab){
                              tempx <- subsets[get(subtabs[subtab]) == 'x',
                                               list(tab = subtabs[subtab], cat1, cat1_varname, cat2 = NA_character_, cat2_varname = NA_character_)]
                              tempx <- data.table::setDT(tidyr::crossing(tempx, data.table::data.table(indicator_key = sub_indicators)))

                            }))

  # crosstabs are a bit more complicated
  sub_crosstabs = data.table::setDT(tidyr::crossing(
    unique(subsets[crosstabs == 'x', list(cat1, cat1_varname)]),
    unique(subsets[crosstabs == 'x', list(cat2 = cat1, cat2_varname = cat1_varname)]) ))
  sub_crosstabs <- sub_crosstabs[cat1 == 'King County' | cat1_varname != cat2_varname]
  sub_crosstabs <- sub_crosstabs[!(cat1_varname == 'race3' & cat2_varname %in% c('race3', 'race4'))] # do not want race x race
  sub_crosstabs <- sub_crosstabs[!(cat2_varname == 'race3' & cat1_varname %in% c('race3', 'race4'))] # do not want race x race
  sub_crosstabs <- sub_crosstabs[!(grepl('aic', cat1_varname) & grepl('race3|race4', cat2_varname))] # do not want race_aic x race
  sub_crosstabs <- sub_crosstabs[!(grepl('aic', cat2_varname) & grepl('race3|race4', cat1_varname))] # do not want race_aic x race
  sub_crosstabs <- sub_crosstabs[!(grepl('_aic_', cat1_varname) & grepl('_aic_', cat2_varname))] # do not want race_aic x race_aic

  sub_crosstabs[, tab := 'crosstabs']
  sub_crosstabs <- data.table::setDT(tidyr::crossing(sub_crosstabs, data.table::data.table(indicator_key = sub_indicators)))


  # append crosstabs
  tempy <- rbind(tempy, sub_crosstabs)

  # tidy
  tempy[cat1 %in% c('Ethnicity') & cat1_varname == 'race3', cat1_varname := 'race3_hispanic']
  tempy[cat2 %in% c('Ethnicity') & cat2_varname == 'race3', cat2_varname := 'race3_hispanic']
  tempy[cat1_varname %in% c('race3', 'race4') & tab != 'trends', cat1 := 'Race'] # 'Race/ethnicity' is only for trends
  tempy[cat2_varname %in% c('race3', 'race4') & tab != 'trends', cat2 := 'Race'] # 'Race/ethnicity' is only for trends
  tempy[cat1_varname %in% c('race3', 'race4', 'race3_hispanic') & tab == 'trends', cat1 := 'Race/ethnicity'] # 'Race/ethnicity' is only for trends
  tempy[cat2_varname %in% c('race3', 'race4', 'race3_hispanic') & tab == 'trends', cat2 := 'Race/ethnicity'] # 'Race/ethnicity' is only for trends
  data.table::setcolorder(tempy, 'indicator_key')
  rads::string_clean(tempy)
  tempy <- tempy[!(tab == 'crosstabs' & cat1 == 'King County' & cat2 != 'King County')] # only legit xtab for KC is KC by itself
  tempy[tab == 'crosstabs' & cat2 == 'King County', `:=` (cat2 = 'Overall', cat2_varname = 'overall')]

  # return object
  return(tempy)
}

