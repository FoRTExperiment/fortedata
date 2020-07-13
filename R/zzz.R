.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thanks for using the fortedata package")

  packageStartupMessage("Please cite the package as:

Atkins JW, Agee E, Barry A, Dahlin KM, Dorheim K, Grigri MS, Haber LT, Hickey
LJ, Kamoske AG, Mathes K, McGuigan C, Paris E, Pennington SC, Rodriguez C,
Shafer A, Shiklomanov A, Tallant J, Gough CM, Bond-Lamberty B (2020). The
fortedata R package: open-science datasets from a manipulative experiment
testing forest resilience. R package version 1.0.1, <URL:
https://github.com/FoRTExperiment/fortedata>.")
}
