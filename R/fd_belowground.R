#  Belowground data


#' Soil respiration (soil to atmosphere CO2 efflux) table.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Kayla Mathes at the University of Michigan Biological Station.
#' @examples
#' fd_soil_respiration()
fd_soil_respiration <- function() {
  flux <- read_csv_file("fd_soil_efflux.csv")

  # Make subplotID column
  flux$SubplotID <- paste0(flux$Rep_ID, "0", flux$Plot_ID, flux$Subplot)

  # Change column names
  names(flux)[names(flux) == "run"] <- "Run"
  names(flux)[names(flux) == "nestedPlot"] <- "NestedPlot"
  #names(flux)[names(flux) == "date"] <- "Date"

  # Timestamp
  flux$TimeStamp <- as.POSIXct(flux$dateTime, format = "%m/%d/%Y %H:%M", tz = "America/Detroit")

  # Retaining date
  flux$Date <- as.Date(flux$date, "%m/%d/%Y")

  flux <- split_subplot_id(flux)

  # Reorder columns, dropping ones we don't need
  flux[c("SubplotID", "Replicate", "Plot", "Subplot", "Date", "TimeStamp", "NestedPlot",
         "Run", "soilCO2Efflux", "soilTemp", "VWC")]
}
