# Remote Sensing data


#' Hemispherical camera data.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
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
#' @return The hemispherical camera data.
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Year` (integer): Year in which measurement was taken
#' - `mean.height` (numeric): mean height of vai
#' - `height.2` (numeric): standard deviation of mean height
#' - `mean.height.var` (numeric): variance of mean height
#' - `mean.height.rms` (numeric): root mean square height
#' - `transect.length` (numeric): length of transect
#' - `mode.el` (numeric): i forgot what this is
#' - `max.el` (numeric): greatest density of VAI x, z position
#' - `mode.2` (numeric): variance of maximum VAI
#' - `max.can.ht` (numeric): maximum measured canopy height
#' - `mean.max.ht` (numeric): mean outer canopy height or MOCH
#' - `mean.vai` (numeric): average VAI across transect
#' - `mean.peak.vai` (numeric): average height of maximum VAI
#' - `deep.gaps` (numeric): number of 1 m wide bins with no lidar returns
#' - `deep.gap.fraction` (numeric): deep gaps dividied by transect length
#' - `porosity` (numeric): ratio of empty to filled bins in the canopy
#' - `std.std` (numeric): precursor to rugosity
#' - `mean.std` (numeric): precursor to rugosity
#' - `rugosity` (numeric): accumulated canopy complexity metric
#' - `top.rugosity` (numeric): standard deviation of final lidar returns
#' - `mean.return.ht` (numeric): average lidar return distance
#' - `sd.return.ht` (numeric): standard deviation of lidar return distances
#' - `sky.fraction` (numeric): ratio of sky hits to lidar returns
#' - `cover.fraction` (numeric): 1/sky fraction
#' - `max.ht` (numeric): same as max can ht, removed in later forestr updates
#' - `scan.density` (numeric): no. of lidar returns divided by transect length
#' - `rumple` (numeric): outer surface variability divided by transect length
#' - `clumping.index` (numeric): clumpiness
#' - `enl` (numeric): effective number of layers
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
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
#' fd_par()
fd_par <- function() {
  par <- read_csv_file("fd_ceptometer.csv")

  # Rename that weird column
  colnames(par)[1] <- "project"

  # Rename columns
  names(par)[names(par) == "Average.Above.PAR"] <- "aPAR"
  names(par)[names(par) == "Average.Below.PAR"] <- "bPAR"
  names(par)[names(par) == "Leaf.Area.Index..LAI."] <- "LAI_cept"

  # Make DateTime column as datetime object
  par$Timestamp <- as.POSIXlt(par$DateTime, format = "%m/%d/%Y %H:%M")

  # Filter to just FoRTE data
  par <- subset(par, par$project == "forte")

  # Remove erroneous entires
  par$Annotation <- gsub("2019", "", par$Annotation)

  # Create the SubPlotID column now that it's clean
  par$SubplotID <- par$Annotation
  par <- split_subplot_id(par)

  # faPAR
  par$faPAR <- par$bPAR / par$aPAR

  # Reorder columns, dropping ones we don't need
  par[c("SubplotID", "Replicate", "Plot", "Subplot", "Timestamp", "aPAR", "bPAR", "faPAR", "LAI_cept")]
}
