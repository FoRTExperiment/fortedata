# Seedling and sapling (>1cm DBH or >1.3m height) basal diameter and height measurements

# These data were collected using digital calipers to measure basal diameter of all seedling and saplings >1cm DBH or >1.3m height within 1/4 of each vegatation sampling area (1m^2). Basal diameter of each sampled stem was binned into 1cm categories (i.e. 0-1cm, 1-2cm, etc). Two height measurements for each sampled stem was measured with a ruler or meter stick: 1) height of previous year budscar, 2) total height. Measurements were collected twice in 2019 (June and August), and once in August 2020.

#' Seedling and sapling data
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @note Data were collected by Maxim S. Grigri
#' @export
#' @examples
#' fd_seedling_sapling()
fd_seedling_sapling <- function() {
  ss_2019 <- read_csv_file("fd_seedling_sapling_2019.csv")
  ss_2020 <- read_csv_file("fd_seedling_sapling_2020.csv")

  # bind years into one df
  ss_alltime <- rbind(ss_2019, ss_2020)

  # convert species codes to USDA taxon codes
  # find unique values
  unique(ss_alltime$species)

  # replace
  ss_alltime$species[ss_alltime$species == "POGR"] <- "POGR4"
  ss_alltime$species[ss_alltime$species == "ACSA"] <- "ACSA3"
  ss_alltime$species[ss_alltime$species == "BEAL"] <- "BEAL2"
  ss_alltime$species[ss_alltime$species == "AMEL"] <- "AMELA"
  ss_alltime$species[ss_alltime$species == "POTR"] <- "POTR5"

  #reformat date class
  ss_alltime$date <- as.Date(ss_alltime$date, "%Y-%m-%d")

  # Data creation and authorship information
  contact_person <- "Maxim S. Grigri"
  citation <- "ESSD"

  # data conditions
  data_conditions(ss_alltime, published = TRUE, contact_person, citation)
}
