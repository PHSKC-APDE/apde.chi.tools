# chi_compare_estimates() ----
#' Compare and validate CHI (Community Health Indicator) estimates between two datasets
#'
#' @description
#' Analyzes differences between two datasets containing CHI estimates, calculating both absolute
#' and relative differences between matched records. The function supports optional metadata
#' integration for result type classification.
#'
#' @param OLD data.frame/data.table containing the reference/baseline CHI estimates
#' @param NEW data.frame/data.table containing the CHI estimates to be validated
#' @param OLD.year Year to filter from the OLD dataset (e.g., "2022")
#' @param NEW.year Year to filter from the NEW dataset (e.g., "2023")
#' @param META data.frame/data.table containing metadata with indicator_key and
#'             result_type columns for classifying comparison results
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Merges datasets using CHI-specific identifiers (indicator_key, tab, cat1, cat2, etc.)
#'   \item Calculates absolute differences (percentage points for non-rates, raw for rates)
#'   \item Calculates relative differences as percentages
#'   \item Identifies notable differences needing further exploration using standard CHI criteria
#' }
#'
#' @examples
#' \dontrun{
#' # Compare two versions of estimates
#' comparison <- chi_compare_estimates(
#'   OLD = previous_estimates,
#'   NEW = current_estimates,
#'   OLD.year = "2022",
#'   NEW.year = "2023",
#'   META = metadata
#' )
#' }
#'
#' @seealso
#' \code{\link{chi_qa_tro}} for validating individual datasets
#'
#' \code{\link{chi_update_sql}} for uploading validated results
#'
#' @return Returns a data.table ordered by absolute differences, containing:
#'
#' \itemize{
#'   \item Difference metrics (absolute_diff, relative_diff)
#'   \item All matching identifiers and categories
#'   \item Original values and metadata from both datasets
#'   \item Diagnostic information (bounds, numerators, denominators, standard errors)
#' }
#'
#' @importFrom data.table data.table setnames ":=" setDT copy
#' @export
#'
chi_compare_estimates <- function(OLD = NULL, NEW = NULL, OLD.year = NULL, NEW.year = NULL, META = NULL){
  # Check inputs ----
    # Check if necessary arguments are present
      if(is.null(OLD)){stop("You must provide 'OLD', i.e., the name of the table with the OLD data")}
      if(is.null(NEW)){stop("You must provide 'NEW', i.e., the name of the table with the NEW data")}
      if(is.null(OLD.year)){stop("You must provide 'OLD.year', i.e., the year of interest in the OLD data")}
      if(is.null(NEW.year)){stop("You must provide 'NEW.year', i.e., the year of interest in the NEW data")}
      if(is.null(META)){stop("You must provide 'Meta', a metadata table with indicator_key and result_type columns for classifying comparison results")}

    # Check if objects are data.frames & make into data.table if need be
      if(is.data.frame(OLD) == FALSE){
        stop("'OLD' must be a data.frame or a data.table")
      }else{OLD <- data.table::setDT(copy(OLD))}

      if(is.data.frame(NEW) == FALSE){
        stop("'NEW' must be a data.frame or a data.table")
      }else{NEW <- data.table::setDT(copy(NEW))}

      if(is.data.frame(META) == FALSE){
        stop("'META' must be a data.frame or a data.table")
      }else{META <- data.table::setDT(copy(META))}

  # Process data ----
    # If metadata provided, add it to the columns to help interpret the output
      if(!is.null(META)){
        NEW <- merge(NEW, META[, list(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE)
      } else { NEW[, result_type := "Metadata not provided"]}

    # Merge old and new data based on identifiers
      comp <- merge(copy(OLD[year == OLD.year]),
                    copy(NEW[year == NEW.year]),
                    by = c("indicator_key", "tab",
                           "cat1", "cat1_group", "cat1_varname",
                           "cat2", "cat2_group", "cat2_varname"),
                    all = T)

    # calculate percent differences between old (x) and new(y)
      comp[, relative.diff := round2(abs((result.x - result.y) / result.x)*100, 1)]
      comp[grepl("mean|proportion", result_type, ignore.case = T), absolute.diff := round2(abs(result.x - result.y)*100, 1)]
      comp[grepl("rate", result_type, ignore.case = T), absolute.diff := round2(abs(result.x - result.y), 1)]
      comp <- comp[!is.na(absolute.diff)]  # drop if absolute difference is NA

    # order variables
      comp <- comp[, c("absolute.diff", "relative.diff", "result_type",
                       "indicator_key", "tab",
                       "cat1", "cat1_group", "cat1_varname",
                       "cat2", "cat2_group", "cat2_varname", "year.x", "year.y",
                       "result.x", "result.y", "lower_bound.x", "lower_bound.y",
                       "upper_bound.x", "upper_bound.y",
                       "numerator.x", "numerator.y", "denominator.x", "denominator.y",
                       "se.x", "se.y")]

    # rename suffixes
      setnames(comp, names(comp), gsub("\\.x$", ".OLD", names(comp)))
      setnames(comp, names(comp), gsub("\\.y$", ".NEW", names(comp)))

    # order based on percent difference
      setorder(comp, -absolute.diff)

  # Identify notable changes based on Joie's criteria ----
    # 1) absolute 3-point difference (for any indicators with KC estimate >=5% or RATE >=5)
    # 2) a relative increase/decrease of 50% (for any indicators with KC estimate <5% or RATE < 5)
      # First, identify whether to use absolute or relative change for each indicator_key
        qa_type = NEW[tab=='_kingcounty']
        qa_type[, qa_type := 'relative']
        qa_type[grepl('mean|proportion', result_type, ignore.case = T) & result >= 0.05, qa_type := 'absolute']
        qa_type[grepl('rate', result_type, ignore.case = T) & result >= 5, qa_type := 'absolute']
        qa_type <- qa_type[, list(indicator_key, qa_type)]

      # Merge on qa_type
        comp <- merge(comp, qa_type, by = 'indicator_key', all = T)

      # Identify notable changes
        comp[qa_type == 'absolute' & grepl('mean|proportion', result_type, ignore.case = T) & absolute.diff >= 3, notable := 1] # absolute difference was multiplied by 100, so assess with >= 3
        comp[qa_type == 'absolute' & grepl('rate', result_type, ignore.case = T) & absolute.diff >= 3, notable := 1]
        comp[qa_type == 'relative' & grepl('mean|proportion', result_type, ignore.case = T) & relative.diff >= 50, notable := 1] # absolute difference was multiplied by 100, so assess with >= 3
        comp[qa_type == 'relative' & grepl('rate', result_type, ignore.case = T) & relative.diff >= 50, notable := 1]

  # Return object ----
    return(comp)
}
