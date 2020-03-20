# Inventory data


#' Return the raw inventory table.
#'
#' @details The columns are as follows:
#' - `Site` (character): Replicate code. Each replicate contains
#'   multiple plots with different disturbance treatments.
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Tag` (integer): Tree tag number.
#' - `Species` (character): Species code from the USDA Plants Database; see
#' \url{https://plants.sc.egov.usda.gov/java/}.
#' - `DBH_cm` (numeric): Diameter at breast height (1.37m), cm.
#' - `Health_status` (character): live (L), moribund (M), or dead (D).
#' - `Canopy_status` (character): Canopy status: overstory dominant (OD),
#' overstory submissive (OS), sapling (SA), understory (UN).
#' - `Date` (date): Inventory entry date.
#' - `Notes` (character): Notes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#' @examples
#' fd_inventory()
fd_inventory <- function() {
  inv <- read_csv_file("forte_inventory.csv")
  inv$Date <- as.Date(inv$Date, format = "%m/%d/%Y")

  # Split the SubplotID column into more useful individual columns
  inv$Replicate <- substr(inv$SubplotID, 1, 1)
  inv$Plot <- as.integer(substr(inv$SubplotID, 2, 3))
  inv$Subplot <- substr(inv$SubplotID, 4, 4)

  # Currently there's a bad entry in the table. Nuke it. Temporary
  inv$DBH_cm <- as.numeric(inv$DBH_cm)  # temporary, until we fix row 791

  inv
}

#' Return basic statistics generated from the raw inventory data.
#'
#' @details The returned columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `BA_m2_ha` (numeric): Basal area, square meters per hectare.
#' - `Stocking_ha` (numeric): Stocking, trees per hectare.
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic. More detailed summaries could be made,
#' e.g. by live/dead, species, etc.
#' @export
#' @importFrom stats aggregate
#' @examples
#' fd_inventory_summary()
fd_inventory_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_subplots()[c("Replicate", "Plot", "Subplot", "Subplot_area_m2")]
  inv <- merge(fd_inventory(), subplots)

  # Subset and compute basal area and stocking
  inv <- inv[inv$Health_status != "D",]  # non-dead trees only
  message("Live and moribund trees only")

  hectare_area <- 10000 # m2
  # radius[cm] => DBH[cm] / 2
  # radius[m] => radius[cm] / 100
  # area[ha] => area[m2] / hectare_area
  # basal_area[m2 ha-1] => (pi * radius[m]^2) * (1 / area[ha])
  inv$BA_m2_ha <- pi * (inv$DBH_cm / 100 / 2) ^ 2 * hectare_area / inv$Subplot_area_m2
  inv$Stocking_ha <- hectare_area / inv$Subplot_area_m2
  stocking <- aggregate(Stocking_ha ~ Replicate + Plot + Subplot , data = inv, FUN = sum)
  ba <- aggregate(BA_m2_ha ~ Replicate + Plot + Subplot, data = inv, FUN = sum)
  biomass <- aggregate(Biomass_kg ~ Replicate + Plot + Subplot, data = inv, FUN = sum)

  # adding in subplot varible too, but this is personal preference.
  #ba$SubplotID <- as.character(paste(ba$Replicate, "0", ba$Plot, ba$Subplot, sep = ""))

  #ba$Stocking <- stocking$DBH_cm
  combo <- weak_as_tibble(merge(ba, stocking))

  weak_as_tibble(merge(combo, biomass))

}
