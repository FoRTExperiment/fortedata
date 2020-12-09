#  Belowground data


#' Soil respiration (soil to atmosphere CO2 efflux) table.
#'
#' @note Measurements taken by Kayla Mathes and Carly Rodriguez
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#'
#' @examples
#' fd_soil_respiration()
fd_soil_respiration <- function() {
  flux <- read_csv_file("fd_soil_efflux.csv")

  # Change column names
  names(flux)[names(flux) == "nestedPlot"] <- "nested_subplot"
  names(flux)[names(flux) == "soilCO2Efflux"] <- "soil_co2_efflux"
  names(flux)[names(flux) == "soilTemp"] <- "soil_temp"
  names(flux)[names(flux) == "VWC"] <- "vwc"
  #names(flux)[names(flux) == "date"] <- "Date"

  # fix lower case values
  flux$Rep_ID <- toupper(flux$Rep_ID)
  flux$Subplot <- toupper(flux$Subplot)

  # Make subplotID column
  flux$subplot_id <- paste0(flux$Rep_ID, "0", flux$Plot_ID, flux$Subplot)

    # Timestamp
  flux$timestamp <- as.POSIXct(flux$dateTime, format = "%m/%d/%Y %H:%M", tz = "America/Detroit")

  # Retaining date
  flux$date <- as.Date(flux$date, "%m/%d/%Y")

  flux <- split_subplot_id(flux)

  # fix any missing/error lowercase
  flux$soil_temp <- suppressWarnings(as.numeric(flux$soil_temp))

  # this removes these weird lines.
  flux <- flux[!is.na(flux$soil_temp), ]

  # Reorder columns, dropping ones we don't need
  flux <- flux[c("subplot_id", "replicate", "plot", "subplot", "date", "timestamp", "nested_subplot",  "run", "soil_co2_efflux", "soil_temp", "vwc")]

  weak_as_tibble(flux)
}
