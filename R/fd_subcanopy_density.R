# Subcanopy (1-8cm DBH) diameter stem density

# These data were collected by randomly selecting 1/4 of each experimental subplot (250m^2)
# and counting the total number of stems 1-8cm DBH identified to the species level.
# These data can be used to scale subcanopy subsample measurements.

#' Subcanopy density data
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @note Data were collected by multiple Gough Lab team members
#' @export
#' @examples
#' fd_subcanopy_density()
fd_subcanopy_density <- function () {
  sc_density <- read_csv_file("fd_subcanopy_stem_count_2019.csv")

  # convert species codes to USDA taxon codes
  # find unique values
  unique(sc_density$species)

  # replace
  sc_density$species[sc_density$species == "POGR"] <- "POGR4"
  sc_density$species[sc_density$species == "ACSA"] <- "ACSA3"
  sc_density$species[sc_density$species == "BEAL"] <- "BEAL2"
  sc_density$species[sc_density$species == "AMEL"] <- "AMELA"
  sc_density$species[sc_density$species == "POTR"] <- "POTR5"

  #reformat date class
  sc_density$date <- as.Date(sc_density$date, "%Y-%m-%d")

  # Data creation and authorship information
  contact_person <- "Maxim S. Grigri"
  citation <- "ESSD"

  # data conditions
  data_conditions(sc_density, published = TRUE, contact_person, citation)
}
