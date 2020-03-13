#  Belowground data


#' Return the raw soil CO2 efflux table.
#'
#' @details The columns are as follows:
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Year` (integer): Year of mesmt
#' - `Date` (character) Date of measurement in YYYY-MM-DD
#' - `DateTime` (POSIXlt) Format in "%Y-%m-%d %H:%M:%S"
#' - `NestedPlot` (integer): Nested subplot sampling point inside subplot
#' - `soilCO2Efflux` (numeric): soil CO2 efflux measured with a LiCor 6400 in mu mol CO2 m^-2 s^-1
#' - `soilTemp` (numeric): soil temperature measured to 7 cm depth in celsius
#' - `VWC` (numeric): volumetric water content in percent
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_inventory()
fd_soilCO2 <- function() {
  flux <- read_csv_file("fd_soil_efflux.csv")

  # makes the subplotID column
  flux$subplotID <- as.factor(paste(flux$Rep_ID, "0", flux$Plot_ID, flux$Subplot, sep = ""))

  # change column name
  names(flux)[names(flux) == "subplotID"] <- "SubplotID"
  names(flux)[names(flux) == "run"] <- "Run"
  names(flux)[names(flux) == "nestedPlot"] <- "NestedPlot"
  names(flux)[names(flux) == "date"] <- "Date"

  # adjusting column data
  flux$DateTime <- as.POSIXlt(flux$dateTime, format = "%Y-%m-%d %H:%M:%S")
  flux$Date <- as.Date(flux$Date, format = "%m/%d/%Y")
  flux$Year <- as.integer(format(as.Date(flux$Date, format = "%Y-%m-%d"),"%Y"))

  # removing dead columns
  flux$dateTime <- NULL #remove column
  flux$X <- NULL
  flux$Rep_ID <- NULL
  flux$Plot_ID <- NULL
  flux$strdate <- NULL

  # Split the SubplotID column into more useful individual columns
  flux$Replicate <- substr(flux$SubplotID, 1, 1)
  flux$Plot <- as.integer(substr(flux$SubplotID, 3, 3))
  flux$Subplot <- substr(flux$SubplotID, 4, 4)

  # We need to remove the columns that have no data in them
  flux <- flux[!is.na(flux$soilCO2Efflux), ]

  # reorders columns
  flux <- flux[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "Date", "DateTime", "NestedPlot", "Run", "soilCO2Efflux", "soilTemp", "VWC")]

  flux
}


#' Return basic statistics generated from soil co2 effluxx.
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
fd_soil_co2_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_subplots()[c("Replicate", "Plot", "Subplot", "Subplot_area_m2")]
  df <- merge(fd_soilCO2(), subplots)

  # Subset and compute basal area and stocking

  weak_as_tibble(merge(df))

}
