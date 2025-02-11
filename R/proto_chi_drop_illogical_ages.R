#' Drop Illogical Age Combinations from CHI Data
#'
#' @description
#' Removes age combinations that don't make logical sense based on category
#' groupings. For example, removes cases where the age falls outside the range
#' specified by age group categories.
#'
#' @param ph.data Input data.table to process
#' @param agevar Name of the age variable (defaults to 'chi_age')
#'
#' @return A data.table with illogical age combinations removed
#' @importFrom data.table copy := fcase between
#' @export
#'

chi_drop_illogical_ages <- function(ph.data, agevar = 'chi_age'){
  ph.data = copy(ph.data)
  for(CatNum in c("cat1", "cat2")){
    ph.data[, paste0(CatNum, '_group_temp') := fcase(get(paste0(CatNum, "_group")) == '<1', '0-0', # <1 is special!
                                                 get(CatNum) %in% c("Age", "Birthing person's age"), gsub("<", "0-", gsub("\\+", "-120", get(paste0(CatNum, '_group')))))]
    ph.data[, AgeMin := gsub("-.*", "", get(paste0(CatNum, '_group_temp')))]
    ph.data[, AgeMax := gsub(".*-", "", get(paste0(CatNum, '_group_temp')))]
    ph.data <- ph.data[!get(CatNum) %in% c("Age", "Birthing person's age")  | between(get(agevar), AgeMin, AgeMax)]
    ph.data[, c("AgeMin", paste0(CatNum, '_group_temp'), "AgeMax") := NULL]
  }
  return(ph.data)
}
