# Inventory data


#' Forest Inventory Data
#'
#' @details Forest inventory data for the 32 FoRTE subplots.
#' Includes diameter-at-breast-height (DBH) measurements in units
#' of cm as well as information on health (i.e. see
#' `health_status` column where D = dead, M = moribund, and L = live) and canopy status (i.e. see `canopy_status` where OD = overstory dominant, UN = understory, OS = overstory submissive, SA = sapling,  and NA is a blank or missing record) and `species`, not species are identified using the USDA Taxon system (e.g. FAGR is Fagus grandfolia, QURU is Quercus rubra, etc.). The `tag` column is the tree tag number. These data were originally collected in 2018, pre-disturbance. For stem-girdled tree information, i.e. which trees were targeted for mortality, see `fd_mortatlity()`. Remeasure for dbh is planned for 2022.
#'
#' @note Data were collected by Gough Lab team members Autym Shafer,
#' Catherine McGuigan, and Alexandra Barry in 2018
#' using a Haglof Postex Inventory Unit with PD II Caliper
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_inventory()
fd_inventory <- function() {

  inv <- read_csv_file("forte_inventory.csv")
  inv$date <- as.Date(inv$date, format = "%m/%d/%Y")

  inv <- split_subplot_id(inv)

  # Currently there's a bad entry in the table. Nuke it. Temporary
  inv$dbh_cm <- as.numeric(inv$dbh_cm)  # temporary, until we fix row 791

  inv <- inv[c("subplot_id", "replicate", "plot", "subplot", "date", "tag",
               "species", "dbh_cm", "health_status", "canopy_status", "notes")]

  # Data creation and authorship information
  contact_person <- "Jeff Atkins"
  citation <- "ESSD"
  data_conditions(inv, published = FALSE, contact_person, citation)
}

#' Basic statistics generated from the raw inventory data.
#'
#' @details The returned columns are as follows:
#' - `replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `ba_m2_ha` (numeric): Basal area, square meters per hectare.
#' - `stocking_ha` (numeric): Stocking, trees per hectare.
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic. More detailed summaries could be made,
#' e.g. by live/dead, species, etc.
#' @export
#' @importFrom stats aggregate
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_inventory_summary()
fd_inventory_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_subplots()[c("replicate", "plot", "subplot", "subplot_area_m2")]
  inv <- merge(fd_inventory(), subplots)

  # Subset and compute basal area and stocking
  inv <- inv[inv$health_status != "D",]  # non-dead trees only
  message("Live and moribund trees only")

  hectare_area <- 10000 # m2
  # radius[cm] => DBH[cm] / 2
  # radius[m] => radius[cm] / 100
  # area[ha] => area[m2] / hectare_area
  # basal_area[m2 ha-1] => (pi * radius[m]^2) * (1 / area[ha])
  inv$ba_m2_ha <- pi * (inv$dbh_cm / 100 / 2) ^ 2 * hectare_area / inv$subplot_area_m2
  inv$stocking_ha <- hectare_area / inv$subplot_area_m2
  stocking <- aggregate(stocking_ha ~ replicate + plot + subplot , data = inv, FUN = sum)
  ba <- aggregate(ba_m2_ha ~ replicate + plot + subplot, data = inv, FUN = sum)

  #ba$Stocking <- stocking$DBH_cm
  weak_as_tibble(merge(ba, stocking))
}



#' Mortality Assignments for 2018 Stem Girdling
#'
#' A data set similar to `fd_inventory` but includes the column `fate` which has a value of either "kill" or "live" with "kill" indicating the tree was stem girdled.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.#'
#' @export
#' @author Atkins
#' @examples
#' fd_mortality()
fd_mortality <- function() {
  kill <- read_csv_file("fd_mortality_assignment.csv")
  #inv$date <- as.Date(inv$date, format = "%m/%d/%Y")

  kill <- split_subplot_id(kill)


  kill <- kill[c("subplot_id", "replicate", "plot", "subplot","tag", "species", "dbh_cm", "health_status", "canopy_status", "fate", "notes")]

  weak_as_tibble(kill)
}

#' Dendroband readings for canopy trees
#'
#' A data set including measurements taken from dendrobands, which give fine scale readings with a precision of 0.01 inch, allowing for
#' quantifying small, incremental growth not possible with DBH tapes. These data were collected using custom made steel band dendrometer bands fixed to a subsample of trees at ~1.3 height. Custom ruler stickers were used to measure the incremental circumference changes in inches. Dendrometer bands were fixed to ~ 700 trees in the summer of 2018. Measurements began in November 2018 with weekly measurements over the 2019 growing season and annual or bi-annual measurements in subsequent years.
#'
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @note Data were collected by multiple Gough Lab team members
#' @export
#' @author Maxim S Grigri
#' @examples
#' fd_dendro()
fd_dendro <- function() {

  # read in data
  df <- read_csv_file("fd_dendroband.csv")

  # replace
  df$species[df$species == "POGR"] <- "POGR4"
  df$species[df$species == "ACSA"] <- "ACSA3"
  df$species[df$species == "BEAL"] <- "BEAL2"
  df$species[df$species == "AMEL"] <- "AMELA"
  df$species[df$species == "POTR"] <- "POTR5"

  # restructure
  df$date <- as.Date(df$date, format = "%m/%d/%Y")
  df$tag <- as.integer(df$tag)

  # rename if issue
  names(df)[names(df) == "subplot"] <- "subplot_id"
  names(df)[names(df) == "bands_in"] <- "band_in"

  #sort by tag
  df <- df[order(df$tag , df$date),]

  # change the str of band_in
  df$band_in <- as.numeric(df$band_in)
  # add plot associated metadata
  df <- split_subplot_id(df)

  # reorganize and sort
  df <- df[c("subplot_id", "replicate", "plot", "subplot","date", "tag", "species", "band_in", "notes")]

  # Data creation and authorship information
  contact_person <- "Maxim S. Grigri [grigrims@vcu.edu], Jeff Atkins [jwatkins6@vcu.edu]"
  citation <- "Grigri, M. S., Atkins, J. W., Vogel, C., Bond-Lamberty, B., & Gough, C. M. (2020). Aboveground Wood Production Is Sustained in the First Growing Season after Phloem-Disrupting Disturbance. Forests, 11(12), 1306."
  data_conditions(df, published = TRUE, contact_person, citation)
}


# Subcanopy (1-8cm DBH) diameter measurements

# These data were collected using digital calipers at ~1.3m bole height. Data were collected bi-weekly in the 2019 field season with end of season diameter in November 2019. In subsequent years, data are collected twice annually, once during the growing season and once in November. All stems in this size class within the 2m^2 vegetation sampling plots were sampled for DBH. When less than 2 stems of this size class were present in the sampling areas, the two closest stems to vegetation plot center were selected resulting in a minimum of 8 diameter samples per subplot

#' Subcanopy diameter data
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @note Data were collected by multiple Gough Lab team members
#' @export
#' @examples
#' fd_subcanopy_diameter()
fd_subcanopy_diameter <- function() {
  sc_2019 <- read_csv_file("fd_subcanopy_diameter_2019.csv")
  sc_2020 <- read_csv_file("fd_subcanopy_diameter_2020.csv")

  # bind years into one df
  sc_alltime <- rbind(sc_2019, sc_2020)

  # convert species codes to USDA taxon codes
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
  data_conditions(sc_alltime, published = TRUE, contact_person, citation)
}


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
  data_conditions(sc_density, published = TRUE, contact_person, citation)
}


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
  ss_alltime$species[ss_alltime$species == "POGR"] <- "POGR4"
  ss_alltime$species[ss_alltime$species == "ACSA"] <- "ACSA3"
  ss_alltime$species[ss_alltime$species == "BEAL"] <- "BEAL2"
  ss_alltime$species[ss_alltime$species == "AMEL"] <- "AMELA"
  ss_alltime$species[ss_alltime$species == "POTR"] <- "POTR5"

  # reformat date column
  ss_alltime$date <- as.Date(ss_alltime$date, "%Y-%m-%d")

  # Data creation and authorship information
  contact_person <- "Maxim S. Grigri"
  citation <- "ESSD"
  data_conditions(ss_alltime, published = TRUE, contact_person, citation)
}
