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

  # Data Creation and Authorship Information
  contact_person <- "Jeff Atkins"
  citation <- "ESSD"

  inv <- read_csv_file("forte_inventory.csv")
  inv$date <- as.Date(inv$date, format = "%m/%d/%Y")

  inv <- split_subplot_id(inv)

  # Currently there's a bad entry in the table. Nuke it. Temporary
  inv$dbh_cm <- as.numeric(inv$dbh_cm)  # temporary, until we fix row 791

  inv <- inv[c("subplot_id", "replicate", "plot", "subplot","date","tag", "species", "dbh_cm", "health_status", "canopy_status", "notes")]

  weak_as_tibble(inv)

  # data conditions
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
#' quantifying small, incremental growth not possible with DBH tapes.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.#'
#' @export
#' @author Grigri
#' @examples
#' fd_dendro()
fd_dendro <- function() {

  # Data Creation and Authorship Information
  contact_person <- "Max Grigri [grigrims@vcu.edu], Jeff Atkins [jwatkins6@vcu.edu]"
  citation <- "Grigri, M. S., Atkins, J. W., Vogel, C., Bond-Lamberty, B., & Gough, C. M. (2020). Aboveground Wood Production Is Sustained in the First Growing Season after Phloem-Disrupting Disturbance. Forests, 11(12), 1306."

  # read in data
  df <- read_csv_file("fd_dendroband.csv")

  # restructure
  df$date <- as.Date(df$date, format = "%m/%d/%Y")
  df$tag <- as.integer(df$tag)

  # rename if issue
  names(df)[names(df) == "subplot"] <- "subplot_id"
  names(df)[names(df) == "bands_in"] <- "band_in"

  # change the str of band_in
  df$band_in <- as.numeric(df$band_in)
  # add plot associated metadata
  df <- split_subplot_id(df)

  # reorganize and sort
  df <- df[c("subplot_id", "replicate", "plot", "subplot","date", "tag", "species", "band_in", "notes")]

  weak_as_tibble(df)

  # data conditions
  data_conditions(df, published = TRUE, contact_person, citation)
}
