#' Get CHI YAML configuration
#'
#' This helper function provides access to the full YAML configuration
#' which contains variable definitions and other CHI-related settings.
#'
#' @return Returns a list containing the parsed YAML configuration
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
