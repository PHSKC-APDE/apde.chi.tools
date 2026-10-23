.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    check_version()
  }
}

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  rm(list = "chi_standard_varnames", envir = ns)
  makeActiveBinding(
    "chi_standard_varnames",
    function() apde.etl::chi_standard_varnames,
    ns
  )
}
