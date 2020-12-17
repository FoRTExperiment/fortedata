# Leaf physiology data


#' Leaf spectrometery data
#'
#' These data were collected from repeated measures with a CI 710 Leaf Spectrometer from CID instruments on leaves in  canopy trees in FoRTE plots. Each individual tree can be identified by the `tree_id` and each leaf is id'ed as `leaf_id`. These data include several vegetation indices `index` derived from the instrument hyperspectral data. The data value for each index is in the `index_value` column and are unitless. Further information is availble in the `fd_ecophysiology_vignette`.
#'
#' @note Data were collected by Lisa Haber, Laura Hickey, Alexandra Barry, and Autym Shafer
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_leaf_spectrometry()
fd_leaf_spectrometry <- function() {
  leaf_spec <- read_csv_file("fd_leaf_spec.csv")

  # adjust data set
  leaf_spec$date <- as.Date(leaf_spec$date)
  #leaf_spec <-leaf_spec[grepl('([A-Za-z])', leaf_spec$index_value), ]    # if I comment this out, it works
  leaf_spec <- leaf_spec[!grepl('\\.$', leaf_spec$index_value), ]
  leaf_spec$index_value <- iconv(leaf_spec$index_value, from = "latin1", to = "ASCII", "")
  leaf_spec$index_value <- as.numeric(as.character(leaf_spec$index_value))

  # split subplot_id
  leaf_spec <- split_subplot_id(leaf_spec)

  # Reorder columns, dropping unneeded FilePath
  leaf_spec <- leaf_spec[c("subplot_id", "replicate", "plot", "subplot", "date", "tree_id", "leaf_id", "species", "index", "index_value", "id")]

  leaf_spec


  # Data creation and authorship information
  contact_person <- "Lisa Haber"
  citation <- "ESSD"

  # data conditions
  data_conditions(leaf_spec, published = FALSE, contact_person, citation)
}


#' Leaf photosynthesis measurements.
#'
#' This data set includes measures of leaf-level photosynthesis etc. from LiCor 6400 data taken during the growing season.
#'
#' @note Data were collected by Lisa Haber, Laura Hickey, Alexandra Barry, and Autym Shafer
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_photosynthesis()
fd_photosynthesis <- function() {
  leaf_photo <- read_csv_file("fd_photosynthesis.csv")

  # meet all naming conventions
  names(leaf_photo) <- tolower(names(leaf_photo))

  # Clean up Species and EBal columns
  leaf_photo$species <- toupper(leaf_photo$species)

  names(leaf_photo)[names(leaf_photo) == "ebal."] <- "ebal"

  # Create a new Timestamp column that uses the information in Filename and HHMMSS
  # See Lisa's explanation in issue #23
  leaf_photo$timestamp <- as.POSIXct(paste(substr(leaf_photo$filename, 14, 22),
                                           leaf_photo$hhmmss),
                                     format = "%m%d%Y %H:%M:%S",
                                     tz = "America/Detroit")

  # Extract the SubplotID column
  leaf_photo$subplot_id <- substr(leaf_photo$filename, 1, 4)
  leaf_photo <- split_subplot_id(leaf_photo)

  # Adjust types
  leaf_photo$area <- as.numeric(leaf_photo$area)
  leaf_photo$stmrat <- as.numeric(leaf_photo$stmrat)
  leaf_photo$f_parin <- as.numeric(leaf_photo$f_parin)
  leaf_photo$f_parout <- as.numeric(leaf_photo$f_parout)

  # Drop a few unneeded fields
  leaf_photo$filename <- leaf_photo$filename_date <- leaf_photo$hhmmss <- NULL

  # Reorder and return
  first_cols <- c("subplot_id", "replicate", "plot", "subplot", "timestamp")
  other_cols <- setdiff(names(leaf_photo), first_cols)
  leaf_photo[c(first_cols, other_cols)]


  # Data creation and authorship information
  contact_person <- "Lisa Haber"
  citation <- "ESSD"

  # data conditions
  data_conditions(leaf_photo, published = FALSE, contact_person, citation)
}
