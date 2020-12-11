# Remote sensing data


#' Hemispherical camera data collected using a 24 Megapixel
#' Sony 6000 DSLR Compact 2571 camera (Regent Instruments; Quebec, QU, Canada) with a
#' 180° hemispherical lens. The blue channel of the camera is replaced with a near-infrared
#' channel, which allows direct calculation of plant greenness as the normalized difference
#' vegetation index (NDVI). See `fd_remote_sensing_vignette` for more details.
#'
#' @note Data were collected by Jeff W. Atkins (2018, 2019, 2020) and Evan Paris (2019)
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @importFrom stats na.omit
#' @export
#' @examples
#' fd_hemi_camera()
fd_hemi_camera <- function() {
  cam <- NULL
  cam <- read_csv_file("fd_hemi_camera.csv")

  # Format columns
  cam$date <- as.Date(cam$date, format = "%m/%d/%Y")
  names(cam)[names(cam) == "SubplotID"] <- "subplot_id"
  #names(cam)[names(cam) == "nsp"] <- "nested_subplot"
  names(cam)[names(cam) == "ndvi"] <- "ndvi"
  names(cam)[names(cam) == "gf"] <- "gap_fraction"
  names(cam)[names(cam) == "open"] <- "openness"
  names(cam)[names(cam) == "lai"] <- "lai_cam"
  names(cam)[names(cam) == "ci"] <- "clumping_index"
  names(cam)[names(cam) == "year"] <- "year"

  cam <- split_subplot_id(cam)

  # Filter to just FoRTE data
  cam <- subset(cam, cam$project == "forte")

  cam$nested_subplot <- NA
  # Replace NSP data with numbers
  cam$nested_subplot[cam$nsp == "C"] <- 0
  cam$nested_subplot[cam$nsp == "N"] <- 1
  cam$nested_subplot[cam$nsp == "E"] <- 3
  cam$nested_subplot[cam$nsp == "S"] <- 5
  cam$nested_subplot[cam$nsp == "W"] <- 7
  cam$nested_subplot[cam$nsp == "X"] <- NA
  cam$nested_subplot[cam$nsp == ""] <- NA

  cam$nested_subplot <- as.integer(cam$nested_subplot)
  # Remove the images that were retained as placemarkers
  # (if there are any that missed being culled)
  cam <- na.omit(cam)

  # Reorder columns, dropping ones we don't need
  cam <- cam[c("subplot_id", "replicate", "plot", "subplot", "nested_subplot", "date",
               "ndvi", "gap_fraction", "openness", "lai_cam", "clumping_index")]

  weak_as_tibble(cam)
}

#' Summary data for hemispherical camera data.
#'
#' @details The columns are as follows:
#' - `replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `date` (date): Date of measurement
#' - `lai_cam` (numeric): mean of leaf area index
#' - `lai_cam_sd` (numeric): sd of leaf area index
#' - `lai_cam_n` (integer): number of observations per sample
#' - `lai_cam_se` (numeric): se of leaf area index
#' - `ndvi` (numeric): mean of plot ndvi
#' - `ndvi_sd` (numeric): sd of plot ndvi
#' - `ndvi_n` (numeric): number of observations per sample
#' - `ndvi_se` (numeric): se of plot ndvi
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic. More detailed summaries could be made,
#' e.g. by live/dead, species, etc.
#' @export
#' @importFrom stats aggregate sd na.omit
#' @author Measurements by Jeff Atkins at the University of Michigan Biological Station.
#' @examples
#' fd_hemi_camera_summary()
fd_hemi_camera_summary <- function() {
  # Load the inventory and subplot tables and merge them
  # vsubplots <- fd_hemi_camera()[c("SubplotID", "Replicate", "Plot", "Subplot", "NestedPlot")]
  df <- fd_hemi_camera()

  # Calculate rugosity means and SD
  lai <- aggregate(lai_cam ~ replicate + plot + subplot + date, data = df, FUN = mean)
  lai_sd <- aggregate(lai_cam ~ replicate + plot + subplot + date, data = df, FUN = sd)
  lai_n <- aggregate(lai_cam ~ replicate + plot + subplot + date, data = df, FUN = length)

  # Merge and munge
  names(lai_sd)[names(lai_sd) == "lai_cam"] <- "lai_cam_sd"
  names(lai_n)[names(lai_n) == "lai_cam"] <- "lai_cam_n"

  lai <- merge(lai, lai_sd)
  lai <- merge(lai, lai_n)
  lai$lai_cam_se <- lai$lai_cam_sd / sqrt(lai$lai_cam_n)  # based on the SD /sqrt(n)

  # VAI means and SD
  ndvi <- aggregate(ndvi ~ replicate , data = df, FUN = mean)
  ndvi_sd <- aggregate(ndvi ~ replicate , data = df, FUN = sd)

  # Merge and munge
  names(ndvi_sd)[names(ndvi_sd) == "ndvi"] <- "ndvi_sd"
  ndvi <- merge(ndvi, ndvi_sd)
  ndvi$ndvi_se <- ndvi$ndvi_sd / sqrt(8)

  weak_as_tibble(merge(lai, ndvi))
}



#' Canopy structural traits from 2D portable canopy lidar collected using a
#' Riegl VHS3100FLP upward facing pulsed-laser system. Lidar point cloud data were converted to
#' canopy strcuturla trait data using `forestr` version 1.0.1
#' See `fd_remote_sensing_vignette` for more details.
#'
#'
#' @note Data were collected by Jeff W. Atkins (2018, 2019, 2020) and Brandon Alveshare (2019)
#'
#' \code{forestr} 1.0.1 package from 2D portable canopy lidar
#' @return A `data.frame` or `tibble` of hemispherical camera data.
#' Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_canopy_structure()
fd_canopy_structure <- function() {
  cst <- read_csv_file("canopy_structural_traits.csv")

  #
  cst <- split_subplot_id(cst)

  names(cst) <- gsub(x = names(cst), pattern = "\\.", replacement = "_")

  # Reorder columns
  cst <- cst[c(1, 54, 55, 56, 2, 3:53 )]

  weak_as_tibble(cst)
}

#' Summary data for canopy structural data including canopy complexity and leaf area
#' by replicate by year for 2018 to 2020
#'
#' @details The columns are as follows:
#' - `replicate` (character): Replicate code, extracted from `subplot_id`.
#' - `plot` (integer): Plot ID number, extracted from `subplot_id`.
#' - `subplot` (character): Subplot code, extracted from `subplot_id`.
#' - `year` (integer): Year in which measurement was taken
#' - `rugosity` (numeric): mean of leaf area index
#' - `rugosity_sd` (numeric): sd of leaf area index
#' - `rugosity_n` (integer): number of observations per sample
#' - `rugosity_se` (numeric): se of leaf area index
#' - `vai_mean` (numeric): mean of leaf area index
#' - `vai_mean_sd` (numeric): sd of leaf area index
#' - `vai_mean_n` (integer): number of observations per sample
#' - `vai_mean_se` (numeric): se of leaf area index
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note This summary reports only canopy structural complexity (as canopy rugosity)
#' and leaf area (as VAI, or vegetation area index--akin to PAI or LAI).
#'
#' @author Measurements by Jeff Atkins at the University of Michigan Biological Station.
#' @export
#' @importFrom stats aggregate sd na.omit
#' @examples
#' fd_canopy_structure_summary()
fd_canopy_structure_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_canopy_structure()[c("subplot_id", "replicate", "plot", "subplot")]
  csc <- merge(fd_canopy_structure(), subplots)

  # Calculate rugosity means and SD
  r_c <- aggregate(rugosity ~ replicate + year, data = csc, FUN = mean)
  r_c.sd <- aggregate(rugosity ~ replicate + year , data = csc, FUN = sd)
  r_c.n <- aggregate(rugosity ~ replicate + year, data = csc, FUN = length)

  # Merge and munge
  names(r_c.sd)[names(r_c.sd) == "rugosity"] <- "rugosity_sd"
  names(r_c.n)[names(r_c.n) == "rugosity"] <- "rugosity_n"

  r_c <- merge(r_c, r_c.sd)
  r_c <- merge(r_c, r_c.n)

  r_c$rugosity_se <- r_c$rugosity_sd / sqrt(r_c$rugosity_n)  # based on the SD /sqrt(n)

  # VAI means and SD
  vai <- aggregate(vai_mean ~ replicate + year , data = csc, FUN = mean)
  vai_sd <- aggregate(vai_mean ~ replicate + year , data = csc, FUN = sd)
  vai_n <- aggregate(vai_mean ~ replicate + year, data = csc, FUN = length)

  # Merge and munge
  names(vai_sd)[names(vai_sd) == "vai_mean"] <- "vai_mean_sd"
  names(vai_n)[names(vai_n) == 'vai_mean'] <- "vai_mean_n"

  vai <- merge(vai, vai_sd)
  vai <- merge(vai, vai_n)

  vai$vai_mean_se <- vai$vai_mean_sd / sqrt(vai$vai_mean_n)

  weak_as_tibble(merge(r_c, vai))
}

#' Ceptometer data.
#'
#' @note Data were collected by Jeff W. Atkins and Brandon Alveshare using
#' a Decagon LP-80 Handheld Ceptometer
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_ceptometer()
fd_ceptometer <- function() {
  cept <- read_csv_file("fd_ceptometer.csv")

  # Rename that weird column and filter to just FoRTE data
  colnames(cept)[1] <- "project"
  cept <- subset(cept, cept$project == "forte")

  # Rename columns
  names(cept)[names(cept) == "Average.Above.PAR"] <- "a_par"
  names(cept)[names(cept) == "Average.Below.PAR"] <- "b_par"
  names(cept)[names(cept) == "Leaf.Area.Index..LAI."] <- "lai_cept"

  # Make DateTime column as datetime object
  cept$timestamp <- as.POSIXct(cept$DateTime, format = "%m/%d/%Y %H:%M", tz = "America/Detroit")

  # Remove erroneous entires
  cept$Annotation <- gsub("2019", "", cept$Annotation)

  # Create the subplot_id column now that it's clean
  cept$subplot_id <- cept$Annotation
  cept <- split_subplot_id(cept)

  # faPAR
  cept$fapar <- (1 - (cept$b_par / cept$a_par))

  # Reorder columns, dropping ones we don't need
  cept <- cept[c("subplot_id", "replicate", "plot", "subplot", "timestamp", "a_par", "b_par", "fapar", "lai_cept")]

  weak_as_tibble(cept)
}




