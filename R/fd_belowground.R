#  Belowground data


#' Raw soil CO2 efflux table
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
#' - `Run` (integer): indicates first or second sample take with IRGA
#' - `soilCO2Efflux` (numeric): soil CO2 efflux measured with a LiCor 6400
#' in µmol CO2 m^-2 s^-1
#' - `soilTemp` (numeric): soil temperature measured to 7 cm depth, degC
#' - `VWC` (numeric): volumetric water content in percent
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_soilCO2()
fd_soilCO2 <- function() {
  flux <- read_csv_file("fd_soil_efflux.csv")

  # Make subplotID column
  flux$subplotID <- as.factor(paste(flux$Rep_ID, "0", flux$Plot_ID, flux$Subplot, sep = ""))

  # Change column name
  names(flux)[names(flux) == "subplotID"] <- "SubplotID"
  names(flux)[names(flux) == "run"] <- "Run"
  names(flux)[names(flux) == "nestedPlot"] <- "NestedPlot"
  names(flux)[names(flux) == "date"] <- "Date"

  # Adjust column data
  flux$DateTime <- as.POSIXlt(flux$dateTime, format = "%Y-%m-%d %H:%M:%S")
  flux$Date <- as.Date(flux$Date, format = "%m/%d/%Y")
  flux$Year <- as.integer(format(as.Date(flux$Date, format = "%Y-%m-%d"),"%Y"))

  # Remove dead columns
  flux$dateTime <- NULL #remove column
  flux$X <- NULL
  flux$Rep_ID <- NULL
  flux$Plot_ID <- NULL
  flux$strdate <- NULL

  # Split the SubplotID column into more useful individual columns
  flux$Replicate <- substr(flux$SubplotID, 1, 1)
  flux$Plot <- as.integer(substr(flux$SubplotID, 3, 3))
  flux$Subplot <- substr(flux$SubplotID, 4, 4)

  # Remove the rows that have no data in them
  flux <- flux[!is.na(flux$soilCO2Efflux), ]

  # Reorder columns
  flux[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "Date", "DateTime", "NestedPlot", "Run", "soilCO2Efflux", "soilTemp", "VWC")]
}


#' Basic statistics generated from soil co2 effluxx.
#'
#' @details The returned columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Date` (date): Date of measurements.
#' - `soilCO2Efflux` (numeric): Mean of Soil Co2 Efflux.
#' - `soilCO2Efflux_sd` (numeric): Standard Deviation of Soil CO2 Efflux.
#' - `soilCO2Efflux_n` (integer): number of soil CO2 Efflux msmts per
#' - `soilCO2Efflux_se` (numeric): Standard error of Soil Co2 Efflux.
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic. More detailed summaries could be made,
#' e.g. by live/dead, species, etc.
#' @export
#' @importFrom stats aggregate sd na.omit
#' @examples
#' fd_soilCO2_summary()
fd_soilCO2_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_subplots()[c("Replicate", "Plot", "Subplot", "Subplot_area_m2")]
  df <- merge(fd_soilCO2(), subplots)

  # Calculate soil CO2 means by plot and date
  co2 <- aggregate(soilCO2Efflux ~ Replicate + Plot + Subplot + Date, data = df, FUN = mean)
  co2.sd <- aggregate(soilCO2Efflux ~ Replicate + Plot + Subplot+ Date, data = df, FUN = sd)
  co2.n <- aggregate(soilCO2Efflux ~ Replicate + Plot + Subplot+ Date, data = df, FUN = length)

  # Change name, merge, then make SE
  names(co2.sd)[names(co2.sd) == "soilCO2Efflux"] <- "soilCO2Efflux_sd"
  names(co2.n)[names(co2.n) == "soilCO2Efflux"] <- "soilCO2Efflux_n"

  co2 <- merge(co2, co2.sd)
  co2 <- merge(co2, co2.n)

  co2$soilCO2Efflux_se <- co2$soilCO2Efflux_sd / sqrt(co2$soilCO2Efflux_n)  # based on the SD /sqrt(n)

  weak_as_tibble(co2)
}
