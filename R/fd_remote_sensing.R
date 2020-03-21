# Remote sensing data


#' Hemispherical camera data.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @importFrom stats na.omit
#' @export
#' @author Measurements by Jeff Atkins at the University of Michigan Biological Station.
#' @examples
#' fd_hemi_camera()
fd_hemi_camera <- function() {
  cam <- read_csv_file("fd_hemi_camera.csv")

  # Format columns
  cam$Date <- as.Date(cam$date, format = "%m/%d/%Y")
  names(cam)[names(cam) == "nsp"] <- "NestedPlot"
  names(cam)[names(cam) == "ndvi"] <- "NDVI"
  names(cam)[names(cam) == "gf"] <- "GapFraction"
  names(cam)[names(cam) == "open"] <- "Openness"
  names(cam)[names(cam) == "lai"] <- "LAI_cam"
  names(cam)[names(cam) == "ci"] <- "ClumpingIndex"
  names(cam)[names(cam) == "year"] <- "Year"

  cam <- split_subplot_id(cam)

  # Filter to just FoRTE data
  cam <- subset(cam, cam$project == "forte")

  # Replace NSP data with numbers
  cam$NestedPlot[cam$NestedPlot == "C"] <- 0
  cam$NestedPlot[cam$NestedPlot == "N"] <- 1
  cam$NestedPlot[cam$NestedPlot == "E"] <- 3
  cam$NestedPlot[cam$NestedPlot == "S"] <- 5
  cam$NestedPlot[cam$NestedPlot == "W"] <- 7
  cam$NestedPlot[cam$NestedPlot == "X"] <- NA
  cam$NestedPlot[cam$NestedPlot == ""] <- NA

  cam$NestedPlot <- as.integer(cam$NestedPlot)
  # Remove the images that were retained as placemarkers
  # (if there are any that missed being culled)
  cam <- na.omit(cam)

  # Reorder columns, dropping ones we don't need
  cam[c("SubplotID", "Replicate", "Plot", "Subplot", "NestedPlot", "Date",
        "NDVI", "GapFraction", "Openness", "LAI_cam", "ClumpingIndex")]
}

#' Canopy structural traits from 2D canopy LiDAR.
#'
#' @note The Canopy structural traits were derived using the
#' \code{forestr} 1.0.1 package from 2D portable canopy lidar
#' @return A `data.frame` or `tibble` of hemispherical camera data.
#' Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Jeff Atkins at the University of Michigan Biological Station.
#' @examples
#' fd_canopy_structure()
fd_canopy_structure <- function() {
  cst <- read_csv_file("canopy_structural_traits.csv")

  # Rename columns that need it
  names(cst)[names(cst) == "subplotID"] <- "SubplotID"
  names(cst)[names(cst) == "year"] <- "Year"

  cst <- split_subplot_id(cst)

  # Reorder columns
  cst[c(1, 31, 32, 33, 2, 3:30 )]
}


#' Ceptometer data.
#'
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Timestamp` (POSIXlt): Date of measurment
#' - `aPAR` (numeric): above canopy PAR (photosynthetically available radiation)
#' - `bPAR` (numeric): below canopy PAR (photosynthetically available radiation)
#' - `faPAR` (numeric): fraction of PAR absorbed by the canopy
#' - `LAI_cept` (numeric): leaf area index derived from ceptometer
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#' @author Measurements by Jeff Atkins at the University of Michigan Biological Station.
#' @examples
#' fd_ceptometer()
fd_ceptometer <- function() {
  cept <- read_csv_file("fd_ceptometer.csv")

  # Rename that weird column and filter to just FoRTE data
  colnames(cept)[1] <- "project"
  cept <- subset(cept, cept$project == "forte")

  # Rename columns
  names(cept)[names(cept) == "Average.Above.PAR"] <- "aPAR"
  names(cept)[names(cept) == "Average.Below.PAR"] <- "bPAR"
  names(cept)[names(cept) == "Leaf.Area.Index..LAI."] <- "LAI_cept"

  # Make DateTime column as datetime object
  cept$Timestamp <- as.POSIXct(cept$DateTime, format = "%m/%d/%Y %H:%M")

  # Remove erroneous entires
  cept$Annotation <- gsub("2019", "", cept$Annotation)

  # Create the SubPlotID column now that it's clean
  cept$SubplotID <- cept$Annotation
  cept <- split_subplot_id(cept)

  # faPAR
  cept$faPAR <- cept$bPAR / cept$aPAR

  # Reorder columns, dropping ones we don't need
  cept[c("SubplotID", "Replicate", "Plot", "Subplot", "Timestamp", "aPAR", "bPAR", "faPAR", "LAI_cept")]
}
