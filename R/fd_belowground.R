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
#' - `Run` (integer): indicates first or second sample take with IRGA
#' - `soilCO2Efflux` (numeric): soil CO2 efflux measured with a LiCor 6400 in mu mol CO2 m^-2 s^-1
#' - `soilTemp` (numeric): soil temperature measured to 7 cm depth in celsius
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

#' Belowground micrometeorology data
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
#' - `soilTemp` (numeric): soil temperature measured to 7 cm depth in celsius
#' - `VWC` (numeric): volumetric water content in percent
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_micromet()
fd_micromet <- function() {
  met <- read_csv_file("fd_soil_efflux.csv")

  # Make the subplotID column
  met$subplotID <- as.factor(paste(met$Rep_ID, "0", met$Plot_ID, met$Subplot, sep = ""))

  # change column name
  names(met)[names(met) == "subplotID"] <- "SubplotID"
  names(met)[names(met) == "run"] <- "Run"
  names(met)[names(met) == "nestedPlot"] <- "NestedPlot"
  names(met)[names(met) == "date"] <- "Date"

  # Adjust column data
  met$DateTime <- as.POSIXlt(met$dateTime, format = "%Y-%m-%d %H:%M:%S")
  met$Date <- as.Date(met$Date, format = "%m/%d/%Y")
  met$Year <- as.integer(format(as.Date(met$Date, format = "%Y-%m-%d"),"%Y"))

  # Remove dead columns
  met$dateTime <- NULL #remove column
  met$X <- NULL
  met$Rep_ID <- NULL
  met$Plot_ID <- NULL
  met$strdate <- NULL
  met$soilCO2Efflux <- NULL
  met$Run <- NULL

  # Split the SubplotID column into more useful individual columns
  met$Replicate <- substr(met$SubplotID, 1, 1)
  met$Plot <- as.integer(substr(met$SubplotID, 3, 3))
  met$Subplot <- substr(met$SubplotID, 4, 4)

  # Reorder columns
  met[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "Date", "DateTime", "NestedPlot", "soilTemp", "VWC")]
}

#' Basic statistics generated from soil micrometeorology
#'
#' @details The returned columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Date` (date): Date of measurements.
#' - `soilTemp` (numeric): Mean of soil temperature
#' - `soilTemp_sd` (numeric): Standard Deviation of soil temperature.
#' - `soilTemp_n` (integer): number of soil temperature msmts per
#' - `soilTemp_se` (numeric): Standard error of soil temperature.
#' - `VWC` (numeric): Mean of volumetric water content.
#' - `VWC_sd` (numeric): Standard Deviation of volumetric water content.
#' - `VWC_n` (integer): Number of VWC msmts per
#' - `VWC_se` (numeric): Standard error of volumetric water content

#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic. More detailed summaries could be made
#' @export
#' @importFrom stats aggregate sd na.omit
#' @examples
#' fd_micromet_summary()
fd_micromet_summary <- function() {
  # Load the inventory and subplot tables and merge them
  df <- fd_micromet()

  # Calculate soil temperature means by plot, by date
  t <- aggregate(soilTemp ~ Replicate + Plot + Subplot + Date, data = df, FUN = mean)
  t.sd <- aggregate(soilTemp ~ Replicate + Plot + Subplot+ Date, data = df, FUN = sd)
  t.n <- aggregate(soilTemp ~ Replicate + Plot + Subplot+ Date, data = df, FUN = length)

  # Change name, merge, then make SE
  names(t.sd)[names(t.sd) == "soilTemp"] <- "soilTemp_sd"
  names(t.n)[names(t.n) == "soilTemp"] <- "soilTemp_n"

  # Combine data
  t <- merge(t, t.sd)
  t <- merge(t, t.n)
  t$soilTemp_se <- t$soilTemp_sd / sqrt(t$soilTemp_n)  # based on the SD /sqrt(n)

  # Calculate soil temperature means by plot, by date
  vwc <- aggregate(VWC ~ Replicate + Plot + Subplot + Date, data = df, FUN = mean)
  vwc.sd <- aggregate(VWC ~ Replicate + Plot + Subplot+ Date, data = df, FUN = sd)
  vwc.n <- aggregate(VWC ~ Replicate + Plot + Subplot+ Date, data = df, FUN = length)

  # Change name, merge, then make SE
  names(vwc.sd)[names(vwc.sd) == "VWC"] <- "VWC_sd"
  names(vwc.n)[names(vwc.n) == "VWC"] <- "VWC_n"

  # Combine data
  vwc <- merge(vwc, vwc.sd)
  vwc <- merge(vwc, vwc.n)
  vwc$VWC_se <- vwc$VWC_sd / sqrt(vwc$VWC_n)  # based on the SD /sqrt(n)

  # Bring them all together
  weak_as_tibble(merge(t, vwc))
}
