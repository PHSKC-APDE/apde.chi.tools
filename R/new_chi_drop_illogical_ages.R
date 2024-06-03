#' CHI_drop_illogical_ages
#'
#' @description
#' a helper function for filtering ages from a set of observations
#'
#' @details
#' CHI analyses relay
#'
#'
#' @param DTx
#' @param agevar
#'
#' @return
#' @export
#'
#' @examples
#'
chi_drop_illogical_ages <- function(DTx, agevar = 'chi_age'){
  DTx = copy(DTx)
  for(CatNum in c("cat1", "cat2")){
    DTx[, paste0(CatNum, '_group_temp') := fcase(get(paste0(CatNum, "_group")) == '<1', '0-0', # <1 is special!
                                                 get(CatNum) %in% c("Age", "Birthing person's age"), gsub("<", "0-", gsub("\\+", "-120", get(paste0(CatNum, '_group')))))]
    DTx[, AgeMin := gsub("-.*", "", get(paste0(CatNum, '_group_temp')))]
    DTx[, AgeMax := gsub(".*-", "", get(paste0(CatNum, '_group_temp')))]
    DTx <- DTx[!get(CatNum) %in% c("Age", "Birthing person's age")  | between(get(agevar), AgeMin, AgeMax)]
    DTx[, c("AgeMin", paste0(CatNum, '_group_temp'), "AgeMax") := NULL]
  }
  return(DTx)
}
