# Remote Sensing data


#' Return hemispherical camera data
#'
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `NestedSubPlot` (integer):  NestedSubplotSampling points but need to check other data
#' - `Date` (date): Date of measurment
#' - `NDVI` (numeric): Replicate code, extracted from `SubplotID`.
#' - `GapFraction` (numeric): Ratio of gap space in the canopy, or open area.
#' - `Openness` (numeric): something something
#' - `LAI` (numeric): leaf area index
#' - `ClumpingIndex` (numeric): Clumping index
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_hemi_camera()
fd_hemi_camera <- function() {
  cam <- read_csv_file("fd_hemi_camera.CSV")

  # formatting columns
  cam$Date <- as.Date(cam$date, format = "%m/%d/%Y")
  names(cam)[names(cam) == "nps"] <- "NestedPlot"
  names(cam)[names(cam) == "ndvi"] <- "NDVI"
  names(cam)[names(cam) == "gf"] <- "GapFraction"
  names(cam)[names(cam) == "open"] <- "Openness"
  names(cam)[names(cam) == "lai"] <- "LAI"
  names(cam)[names(cam) == "ci"] <- "ClumpingIndex"

  # Split the SubplotID column into more useful individual columns
  cam$Replicate <- substr(cam$SubplotID, 1, 1)
  cam$Plot <- as.integer(substr(cam$SubplotID, 2, 3))
  cam$Subplot <- substr(cam$SubplotID, 4, 4)

  # filters to just FoRTE data
  cam <- subset(cam, project == "forte")

  # replaces NSP data with numbers
  cam$NestedPlot[cam$NestedPlot == "C"] <- 0
  cam$NestedPlot[cam$NestedPlot == "N"] <- 1
  cam$NestedPlot[cam$NestedPlot == "E"] <- 3
  cam$NestedPlot[cam$NestedPlot == "S"] <- 5
  cam$NestedPlot[cam$NestedPlot == "W"] <- 7

  cam$NestedPlot <- as.integer(cam$NestedPlot)
  # reorders columns
  cam <- cam[c("SubplotID", "Replicate", "Plot", "Subplot", "NestedPlot", "Date", "NDVI", "GapFraction", "Openness", "LAI", "ClumpingIndex")]

  cam
}

#' Canopy Structural Traits from 2D Canopy LiDAR
#'
#' Canopy strucutral Traits derived using the forestr 1.0.1 package from
#' 2D portable canopy lidar
#' Return hemispherical camera data
#'
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Year` (integer): Year in which measurement was taken
#' - `mean.height' (numeric): mean height of vai
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
#'
#' @examples
#' fd_canopy_structure()
fd_canopy_structure <- function() {
  cst <- read_csv_file("canopy_structural_traits.CSV")

  #renaming columns that need it
  names(cst)[names(cst) == "subplotID"] <- "SubplotID"
  names(cst)[names(cst) == "year"] <- "Year"

  # Split the SubplotID column into more useful individual columns
  cst$Replicate <- substr(cst$SubplotID, 1, 1)
  cst$Plot <- as.integer(substr(cst$SubplotID, 2, 3))
  cst$Subplot <- substr(cst$SubplotID, 4, 4)


  # reorders columns
  cst <- cst[c(1, 31, 32, 33, 2, 3:30 )]
  cst
}


