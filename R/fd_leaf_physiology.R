# Leaf physiology data


#' Leaf spectrometry data
#'
#' @note Data were collected by Lisa Haber, Laura Hickey, Alexandra Barry, and Autym Shafer
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_leaf_spectrometry()
fd_leaf_spectrometry <- function() {
  leaf_spec <- read_csv_file("fd_leaf_spectral_indices.csv")

  # Change column name
  names(leaf_spec)[names(leaf_spec) == "calculation"] <- "index"
  names(leaf_spec)[names(leaf_spec) == "value"] <- "index_value"
  names(leaf_spec)[names(leaf_spec) == "v1"] <- "filepath"

  # Adjust column data
  leaf_spec$index <- gsub("[^[:alnum:]]", "", leaf_spec$index)
  leaf_spec$tree_id <- as.character(substr(leaf_spec$filepath, 11, 15))
  leaf_spec$species <- as.character(substr(leaf_spec$filepath, 6, 9))
  leaf_spec$date <- as.character(substr(leaf_spec$filepath, 17, 24))
  leaf_spec$date <- as.Date(as.character(leaf_spec$date), format = "%m%d%Y")
  leaf_spec <- leaf_spec[grepl('([A-Za-z])', leaf_spec$index_value), ]
  leaf_spec <- leaf_spec[!grepl('\\.$', leaf_spec$index_value), ]
  leaf_spec$index_value <- iconv(leaf_spec$index_value, from = "latin1", to = "ASCII", "")
  leaf_spec$index_value <- as.numeric(as.character(leaf_spec$index_value))

  # Extract the SubplotID
  leaf_spec$subplot_id <- substr(leaf_spec$filepath, 1, 4)
  leaf_spec <- split_subplot_id(leaf_spec)

  # Reorder columns, dropping unneeded FilePath
  leaf_spec <- leaf_spec[c("subplot_id", "replicate", "plot", "subplot", "date", "tree_id", "species", "index", "index_value")]

  leaf_spec
}


#' Leaf photosynthesis measurements.
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
}
