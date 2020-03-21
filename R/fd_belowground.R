#  Belowground data


#' Soil respiration (soil to atmosphere CO2 efflux) table.
#'
#' @details The columns are as follows:
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Timestamp` (POSIXlt) Format in "%Y-%m-%d %H:%M:%S"
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
#' fd_soil_respiration()
fd_soil_respiration <- function() {
  flux <- read_csv_file("fd_soil_efflux.csv")

  # Make subplotID column
  flux$subplotID <- as.factor(paste0(flux$Rep_ID, "0", flux$Plot_ID, flux$Subplot))

  # Change column names
  names(flux)[names(flux) == "subplotID"] <- "SubplotID"
  names(flux)[names(flux) == "run"] <- "Run"
  names(flux)[names(flux) == "nestedPlot"] <- "NestedPlot"
  names(flux)[names(flux) == "date"] <- "Date"

  # Timestamp
  flux$Timestamp <- as.POSIXlt(flux$dateTime, format = "%m/%d/%Y %H:%M")

  # Remove dead columns
  flux$dateTime <- flux$X <- flux$Rep_ID <- flux$Plot_ID <- flux$strdate <- NULL

  # Split the SubplotID column into more useful individual columns
  flux$Replicate <- substr(flux$SubplotID, 1, 1)
  flux$Plot <- as.integer(substr(flux$SubplotID, 3, 3))
  flux$Subplot <- substr(flux$SubplotID, 4, 4)

  # Reorder columns
  flux[c("SubplotID", "Replicate", "Plot", "Subplot", "Timestamp", "NestedPlot",
         "Run", "soilCO2Efflux", "soilTemp", "VWC")]
}
