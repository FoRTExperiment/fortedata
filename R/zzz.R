.onAttach <- function(libname, pkgname) {
  if(stats::runif(1) < 0.25) {
    packageStartupMessage("Thanks for using fortedata. Please cite the package correctly! See citation('fortedata')")
  }
}
