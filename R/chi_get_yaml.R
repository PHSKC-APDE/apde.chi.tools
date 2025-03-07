#' Get CHI variable column names
#'
#' Returns a character vector of column names defined in the CHI YAML reference file.
#' This helper function provides easy access to the standardized CHI variable names.
#'
#' @return A character vector of column names
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
#' cols <- chi_get_cols()
chi_get_cols <- function() {
  chi.yaml.filepath <- system.file("ref", "chi_qa.yaml", package = "apde.chi.tools")
  if (chi.yaml.filepath == "") {
    stop("Could not find reference file chi_qa.yaml")
  }
  chi.yaml <- yaml::read_yaml(chi.yaml.filepath)
  return(names(chi.yaml$vars))
}

#' Get CHI YAML configuration
#'
#' Returns the complete CHI YAML configuration as a list.
#' This helper function provides access to the full YAML configuration
#' which contains variable definitions and other CHI-related settings.
#'
#' @return A list containing the parsed YAML configuration
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
#' config <- chi_get_yaml()
chi_get_yaml <- function() {
  chi.yaml.filepath <- system.file("ref", "chi_qa.yaml", package = "apde.chi.tools")
  if (chi.yaml.filepath == "") {
    stop("Could not find reference file chi_qa.yaml")
  }
  return(yaml::read_yaml(chi.yaml.filepath))
}
