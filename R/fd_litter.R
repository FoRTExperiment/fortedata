# Litterfall data from litter trap collections

#' Litter trap data.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_litter()
fd_litter <- function() {
  litter <- read_csv_file("fd_littertrap.csv")

  litter$Species <- toupper(litter$Species)
  litter <- split_subplot_id(litter)

  # Reorder columns, dropping unneeded ones
  litter[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "Species", "BagTare_g", "BagMass_g")]
}

#' Leaf area computed from litter trap data.
#'
#' @details The columns are as follows:
#'
#' SubplotID	Species	bag_no	bag_tare_g	bag_mass_g
#' - `SubplotID` (character): Subplot ID number. These subplot codesarea
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' #' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Year` (integer): year of litter collection
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Species` (character): Species code from the USDA Plants Database; see
#' \url{https://plants.sc.egov.usda.gov/java/}.
#' - `BagTare_g` (numeric): Diameter at breast height (1.37m), cm.
#' - `BagMass_g` (numeric): weight of bag + litter
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_lai()
fd_lai <- function() {
  leaf <- read_csv_file("fd_littertrap.csv")
  leaf$bag_no <- NULL  #I am not sure why this was here, and it's kind of useless
  leaf$Species <- toupper(leaf$Species)

  # Create leaf mass column
  leaf$LeafMass_g <- leaf$BagMass_g - leaf$BagTare_g

  # Need to bring in SLA (specific leaf area) data to convert from mass to leaf area

  # Bring in the allometry values
  sla <- read_csv_file("fd_sla.csv") #this has the same equations AmeriFlux uses

  # Change column names
  names(sla)[1] <- paste("Species")

  # Add SLA to the leaf tibble
  leaf <- merge(leaf, sla)

  # Calculate leaf area totals
  leaf$LeafArea <- leaf$LeafMass_g * leaf$SLA

  # Make plot lai
  lai <- aggregate(LeafArea ~ SubplotID + Year, data = leaf, FUN = sum)

  # Add in plot area
  lai$PlotArea <- 1000  #plot area in m^2 (is 0.1 ha)

  # Calculate LAI
  lai$LAI <- lai$LeafArea / lai$PlotArea

  lai <- split_subplot_id(lai)

  # Reorder columns, dropping unneeded ones
  lai[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "LAI")]
}

#' Basic statistics generated from the raw litter trap data
#'
#' @details The returned columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this one doesn't have everything
#' @export
#' @importFrom stats aggregate sd na.omit
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_lai_summary()
fd_lai_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_subplots()[c("Replicate", "Plot", "Subplot", "Subplot_area_m2")]
  df <- merge(fd_lai(), subplots)

  weak_as_tibble(df)
}
