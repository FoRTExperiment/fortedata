# Leaf physiology data


#' Leaf spectrometry data
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Lisa Haber at the University of Michigan Biological Station.
#' @examples
#' fd_leaf_spectrometry()
fd_leaf_spectrometry <- function() {
  leaf_spec <- read_csv_file("fd_leaf_spectral_indices.csv")

  # Change column name
  names(leaf_spec)[names(leaf_spec) == "V1"] <- "FilePath"
  names(leaf_spec)[names(leaf_spec) == "Calculation"] <- "Index"
  names(leaf_spec)[names(leaf_spec) == "Value"] <- "Index_Value"

  # Adjust column data
  leaf_spec$Index <- as.character(stringr::str_replace_all(leaf_spec$Index, "[^[:alnum:]]", ""))
  leaf_spec$Species <- as.character(substr(leaf_spec$FilePath, 6, 9))
  leaf_spec$Date <- as.character(substr(leaf_spec$FilePath, 17, 24))
  leaf_spec$Date <- as.Date(as.character(leaf_spec$Date), format = "%m%d%Y")
  leaf_spec <- leaf_spec[!stringr::str_detect(leaf_spec$Index_Value, '([A-Za-z])'), ]
  leaf_spec <- leaf_spec[!stringr::str_detect(leaf_spec$Index_Value, '\\.$'), ]
  leaf_spec$Index_Value <- iconv(leaf_spec$Index_Value, from = "latin1", to = "ASCII", "")
  leaf_spec$Index_Value <- as.numeric(as.character(leaf_spec$Index_Value))

  # Extract the SubplotID
  leaf_spec$SubplotID <- substr(leaf_spec$FilePath, 1, 4)
  leaf_spec <- split_subplot_id(leaf_spec)

  # Reorder columns, dropping unneeded FilePath
  leaf_spec <- leaf_spec[c("SubplotID", "Replicate", "Plot", "Subplot", "Date", "Species", "Index", "Index_Value")]

  leaf_spec
}


#' Leaf photosynthesis measurements.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Lisa Haber at the University of Michigan Biological Station.
#' @examples
#' fd_photosynthesis()
fd_photosynthesis <- function() {
  leaf_photo <- read_csv_file("fd_photosynthesis.csv")

  # Clean up Species and EBal columns
  leaf_photo$Species <- toupper(leaf_photo$Species)

  names(leaf_photo)[names(leaf_photo) == "EBal."] <- "EBal"

  # Create a new Timestamp column that uses the information in Filename and HHMMSS
  # See Lisa's explanation in issue #23
  leaf_photo$Timestamp <- as.POSIXct(paste(substr(leaf_photo$Filename, 14, 22),
                                           leaf_photo$HHMMSS),
                                     format = "%m%d%Y %H:%M:%S",
                                     tz = "America/Detroit")

  # Extract the SubplotID column
  leaf_photo$SubplotID <- substr(leaf_photo$Filename, 1, 4)
  leaf_photo <- split_subplot_id(leaf_photo)

  # Adjust types
  leaf_photo$Area <- as.numeric(leaf_photo$Area)
  leaf_photo$StmRat <- as.numeric(leaf_photo$StmRat)
  leaf_photo$f_parin <- as.numeric(leaf_photo$f_parin)
  leaf_photo$f_parout <- as.numeric(leaf_photo$f_parout)

  # Drop a few unneeded fields
  leaf_photo$Filename <- leaf_photo$Filename_date <- leaf_photo$HHMMSS <- NULL

  # Reorder and return
  first_cols <- c("SubplotID", "Replicate", "Plot", "Subplot", "Timestamp")
  other_cols <- setdiff(names(leaf_photo), first_cols)
  leaf_photo[c(first_cols, other_cols)]
}
