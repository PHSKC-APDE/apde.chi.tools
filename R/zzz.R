.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    getNamespace("apde.chi.tools")$check_version()
  }
}
