# Subcanopy (1-8cm DBH) diameter measurements

# These data were collected using custom made steel band dendrometer bands fixed to a subsample of trees at ~1.3 height. Custom ruler stickers were used to measure the incremental circumference changes in inches. Dendrometer bands were fixed to ~ 700 trees in the summer of 2018. Measurements began in November 2018 with weekly measurements over the 2019 growing season and annual or bi-annual measurements in subsequent years.

#' Upper canopy diameter measurements
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @note Data were collected by multiple Gough Lab team members
#' @export
#' @examples
#' fd_dendro()
fd_dendro <- function() {
  dendro <- read_csv_file("fd_dendroband.csv")

  # convert species codes to USDA taxon codes
  # find unique values
  unique(dendro$species)

  # replace
  dendro$species[dendro$species == "POGR"] <- "POGR4"
  dendro$species[dendro$species == "ACSA"] <- "ACSA3"
  dendro$species[dendro$species == "BEAL"] <- "BEAL2"
  dendro$species[dendro$species == "AMEL"] <- "AMELA"
  dendro$species[dendro$species == "POTR"] <- "POTR5"

  #reformat date class
  dendro$date <- as.Date(dendro$date, "%m/%d/%Y")

  # Data creation and authorship information
  contact_person <- "Maxim S. Grigri"
  citation <- "ESSD"

  # data conditions
  data_conditions(dendro, published = TRUE, contact_person, citation)
}
