#' Get CHI variable column names
#'
#' @description
#' Returns a character vector of column names defined in the CHI YAML reference file.
#' This helper function provides easy access to the standardized CHI Tableau
#' Ready Output variable names.
#'
#' @param metadata returns metadata column names instead of primary data
#'
#' @return Returns a character vector of column names for the chi data (Default) or metadata
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
#' cols <- chi_get_cols()
chi_get_cols <- function(metadata = FALSE) {
  chi.yaml.filepath <- system.file("ref", "chi_qa.yaml", package = "apde.chi.tools")
  if (chi.yaml.filepath == "") {
    stop("Could not find reference file chi_qa.yaml")
  }
  chi.yaml <- yaml::read_yaml(chi.yaml.filepath)
  if(metadata){
    return(names(chi.yaml$metadata))
  }
  return(names(chi.yaml$vars))
}
