# Leaf Spectrometry data


#' Return the Leaf Spectrometry Index table
#'
#' @details The columns are as follows:
#' - `SubplotID` (integer): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Date` (date): Inventory entry date.
#' - `Species` (character): Species code from the USDA Plants Database; see
#' \url{https://plants.sc.egov.usda.gov/java/}.
#' - `Index` (character): Spectral index measured from the CID 710, but we do need an internal table
#' to point to.
#' - `Index_Value` (numeric): Measured index value corresponding to the index
#'
#' - `FilePath` (character): Notes.
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_leaf_spectrometry()

fd_leaf_spectrometry <- function() {
  leaf_spec <- read_csv_file("fd_leaf_spectral_indices.csv")

  # change column name
  names(leaf_spec)[names(leaf_spec) == "V1"] <- "FilePath"
  names(leaf_spec)[names(leaf_spec) == "Calculation"] <- "Index"
  names(leaf_spec)[names(leaf_spec) == "Value"] <- "Index_Value"

  # adjusting column data
  leaf_spec$SubplotID <- substr(leaf_spec$FilePath, 0, 4)
  leaf_spec$Index <- as.factor(stringr::str_replace_all(leaf_spec$Index, "[^[:alnum:]]", ""))
  leaf_spec$Species <- as.character(substr(leaf_spec$FilePath, 6, 9))
  leaf_spec$Date <- as.character(substr(leaf_spec$FilePath, 17, 24))
  leaf_spec$Date <- as.Date(as.character(leaf_spec$Date), format = "%m%d%Y")
  leaf_spec <- leaf_spec[!stringr::str_detect(leaf_spec$Index_Value, '([A-Za-z])'), ]
  leaf_spec <- leaf_spec[!stringr::str_detect(leaf_spec$Index_Value, '\\.$'), ]
  leaf_spec$Index_Value <- as.numeric(as.character(leaf_spec$Index_Value))

  # Split the SubplotID column into more useful individual columns
  leaf_spec$Replicate <- substr(leaf_spec$SubplotID, 1, 1)
  leaf_spec$Plot <- as.integer(substr(leaf_spec$SubplotID, 2, 3))
  leaf_spec$Subplot <- substr(leaf_spec$SubplotID, 4, 4)

  # reorders columns
  leaf_spec <- leaf_spec[c("SubplotID", "Replicate", "Plot", "Subplot", "Date", "Species", "Index", "Index_Value", "FilePath")]
  leaf_spec
}
