# Litterfall data from litter trap collections


#' Return the raw inventory table.
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
#'
#' @examples
#' fd_lai()
fd_lai <- function() {
  leaf <- read_csv_file("fd_littertrap.csv")
  leaf$bag_no <- NULL  #I am not sure why this was here, and it's kind of useless
  leaf$Species <- toupper(leaf$Species)

  # create leaf mass column
  leaf$LeafMass_g <- leaf$BagMass_g - leaf$BagTare_g

  # Need to bring in SLA (specific leaf area) data to convert from mass to leaf area

  # bring in the allometry values
  sla <- read_csv_file("fd_sla.csv") #this has the same equations AmeriFlux uses

  # changing column names
  names(sla)[1] <- paste("Species")

  # Add SLA to the leaf tibble
  leaf <- merge(leaf, sla)

  # calculate leaf area totals
  leaf$LeafArea <- leaf$LeafMass_g * leaf$SLA

  # make plot lai
  lai <- aggregate(LeafArea ~ SubplotID + Year, data = leaf, FUN = sum)

  # adds in plot area
  lai$PlotArea <- 1000  #plot area in m^2 (is 0.1 ha)

  # calculates LAI
  lai$LAI <- lai$LeafArea / lai$PlotArea

  # removes columns
  lai$LeafArea <- NULL
  lai$PlotArea <- NULL

  # Split the SubplotID column into more useful individual columns
  lai$Replicate <- substr(lai$SubplotID, 1, 1)
  lai$Plot <- as.integer(substr(lai$SubplotID, 2, 3))
  lai$Subplot <- substr(lai$SubplotID, 4, 4)

  # reorders columns
  lai <- lai[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "LAI")]

  weak_as_tibble(lai)
}

#' Return basic statistics generated from the raw litter trap data
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
#' @examples
#' fd_lai_summary()
fd_lai_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_subplots()[c("Replicate", "Plot", "Subplot", "Subplot_area_m2")]
  df <- merge(fd_lai(), subplots)

  # Subset and compute basal area and stocking

  weak_as_tibble(df)

}
