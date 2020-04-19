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
  cam$date <- as.Date(cam$date, format = "%m/%d/%Y")
  names(cam)[names(cam) == "SubplotID"] <- "subplot_id"
  names(cam)[names(cam) == "nsp"] <- "nested_subplot"
  names(cam)[names(cam) == "ndvi"] <- "ndvi"
  names(cam)[names(cam) == "gf"] <- "gap_fraction"
  names(cam)[names(cam) == "open"] <- "openness"
  names(cam)[names(cam) == "lai"] <- "lai_cam"
  names(cam)[names(cam) == "ci"] <- "clumping_index"
  names(cam)[names(cam) == "year"] <- "year"

  cam <- split_subplot_id(cam)

  # Filter to just FoRTE data
  cam <- subset(cam, cam$project == "forte")

  # Replace NSP data with numbers
  cam$nested_subplot[cam$nested_subplot == "C"] <- 0
  cam$nested_subplot[cam$nested_subplot == "N"] <- 1
  cam$nested_subplot[cam$nested_subplot == "E"] <- 3
  cam$nested_subplot[cam$nested_subplot == "S"] <- 5
  cam$nested_subplot[cam$nested_subplot == "W"] <- 7
  cam$nested_subplot[cam$nested_subplot == "X"] <- NA
  cam$nested_subplot[cam$nested_subplot == ""] <- NA

  cam$nested_subplot <- as.integer(cam$nested_subplot)
  # Remove the images that were retained as placemarkers
  # (if there are any that missed being culled)
  cam <- na.omit(cam)

  # Reorder columns, dropping ones we don't need
  cam <- cam[c("subplot_id", "replicate", "plot", "subplot", "nested_subplot", "date",
        "ndvi", "gap_fraction", "openness", "lai_cam", "clumping_index")]

  #weak_as_tibble(cam)
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
  names(cst)[names(cst) == "subplotID"] <- "subplot_id"
  names(cst)[names(cst) == "year"] <- "year"

  cst <- split_subplot_id(cst)

  # Reorder columns
  cst <- cst[c(1, 31, 32, 33, 2, 3:30 )]

  weak_as_tibble(cst)
}


#' Ceptometer data.
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' Call \code{\link{fd_metadata}} for field metadata.
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
  names(cept)[names(cept) == "Average.Above.PAR"] <- "a_par"
  names(cept)[names(cept) == "Average.Below.PAR"] <- "b_par"
  names(cept)[names(cept) == "Leaf.Area.Index..LAI."] <- "lai_cept"

  # Make DateTime column as datetime object
  cept$timestamp <- as.POSIXct(cept$DateTime, format = "%m/%d/%Y %H:%M")

  # Remove erroneous entires
  cept$Annotation <- gsub("2019", "", cept$Annotation)

  # Create the subplot_id column now that it's clean
  cept$subplot_id <- cept$Annotation
  cept <- split_subplot_id(cept)

  # faPAR
  cept$fapar <- cept$b_par / cept$a_par

  # Reorder columns, dropping ones we don't need
  cept <- cept[c("subplot_id", "replicate", "plot", "subplot", "timestamp", "a_par", "b_par", "fapar", "lai_cept")]

  weak_as_tibble(cept)
}
