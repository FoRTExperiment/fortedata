# Subcanopy (1-8cm DBH) diameter measurements

# These data were collected using digital calipers at ~1.3m bole height. Data were collected bi-weekly in the 2019 field season with end of season diameter in November 2019. In subsequent years, data are collected twice annually, once during the growing season and once in November. All stems in this size class within the 2m^2 vegetation sampling plots were sampled for DBH. When less than 2 stems of this size class were present in the sampling areas, the two closest stems to vegetation plot center were selected resulting in a minimum of 8 diameter samples per subplot

#' @note Data were collected by multiple Gough Lab team members
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_subcanopy()
fd_subcanopy_diameter <- function() {
  sc_2019 <- read_csv_file("fd_subcanopy_diameter_2019.csv")
  sc_2020 <- read_csv_file("fd_subcanopy_diameter_2020.csv")

  # bind years into one df
  sc_alltime <- rbind(sc_2019, sc_2020)

  # convert species codes to USDA taxon codes
  # find unique values
  unique(sc_alltime$species)

  # replace
  sc_alltime$species[sc_alltime$species == "POGR"] <- "POGR4"
  sc_alltime$species[sc_alltime$species == "ACSA"] <- "ACSA3"
  sc_alltime$species[sc_alltime$species == "BEAL"] <- "BEAL2"
  sc_alltime$species[sc_alltime$species == "AMEL"] <- "AMELA"
  sc_alltime$species[sc_alltime$species == "POTR"] <- "POTR5"

  #reformat date class
  sc_alltime$date <- as.Date(sc_alltime$date, "%Y-%m-%d")

  # Data creation and authorship information
  contact_person <- "Maxim S. Grigri"
  citation <- "ESSD"

  # data conditions
  data_conditions(sc_alltime, published = TRUE, contact_person, citation)

}
