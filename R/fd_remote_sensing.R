# Remote Sensing data


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
#' - `a.biomass` (numeric): a coefficent for allmetric equation for aboveground biomass in kg.
#' - `b.biomass` (numeric): b coefficent for allmetric equation for aboveground biomass in kg.
#' - `Biomass_kg` (numeric): SBiomass derived from allometry the form a * dbh^b in kg
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_inventory()
fd_hemi_ndvi <- function() {
  ndvi <- read_csv_file("fd_ndvi_hemi.csv")
  inv$Date <- as.Date(inv$Date, format = "%m/%d/%Y")
  # original header: date	plotlong	project	SubplotID	plot.side	nps	ndvi	gf	open	lai	ratio	ci

  # Split the SubplotID column into more useful individual columns
  inv$Replicate <- substr(inv$SubplotID, 1, 1)
  inv$Plot <- as.integer(substr(inv$SubplotID, 2, 3))
  inv$Subplot <- substr(inv$SubplotID, 4, 4)

  # Currently there's a bad entry in the table. Nuke it. Temporary
  inv$DBH_cm <- as.numeric(inv$DBH_cm)  # temporary, until we fix row 791

  # bring in the allometry values
  allo.df <- read_csv_file("biomass_allometry_table.csv") #this has the same equations AmeriFlux uses

  # changing column names
  names(allo.df)[1] <- paste("Species")

  # Add in the allometries
  inv <- merge(inv, allo.df)

  #calculates biomass in units of kg
  inv$Biomass_kg <- inv$a.biomass * inv$DBH_cm ^ inv$b.biomass

  inv
}
