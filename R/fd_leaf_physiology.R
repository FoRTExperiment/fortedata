# Leaf Spectrometry data


#' Return the Leaf Spectrometry Index table
#'
#' @details The columns are as follows:
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
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
  leaf_spec$Index <- as.character(stringr::str_replace_all(leaf_spec$Index, "[^[:alnum:]]", ""))
  leaf_spec$Species <- as.character(substr(leaf_spec$FilePath, 6, 9))
  leaf_spec$Date <- as.character(substr(leaf_spec$FilePath, 17, 24))
  leaf_spec$Date <- as.Date(as.character(leaf_spec$Date), format = "%m%d%Y")
  leaf_spec <- leaf_spec[!stringr::str_detect(leaf_spec$Index_Value, '([A-Za-z])'), ]
  leaf_spec <- leaf_spec[!stringr::str_detect(leaf_spec$Index_Value, '\\.$'), ]
  leaf_spec$Index_Value[leaf_spec$Index_Value == " âˆž"] <- NA
  leaf_spec$Index_Value <- as.numeric(leaf_spec$Index_Value)

  # Split the SubplotID column into more useful individual columns
  leaf_spec$Replicate <- substr(leaf_spec$SubplotID, 1, 1)
  leaf_spec$Plot <- as.integer(substr(leaf_spec$SubplotID, 3, 3))
  leaf_spec$Subplot <- substr(leaf_spec$SubplotID, 4, 4)

  # reorders columns
  leaf_spec <- leaf_spec[c("SubplotID", "Replicate", "Plot", "Subplot", "Date", "Species", "Index", "Index_Value", "FilePath")]
  leaf_spec
}

# Leaf Photosynthesis Data


#' Return the Leaf Spectrometry Index table
#'
#' @details The columns are as follows:
#' - `Obs` (numeric) Observation number within file.
#' - `HHMMSS` (character) Time of day.
#' - `FTime` (integer) Number of seconds since logging began.
#' - `EBal.` (integer) Energy balance on?
#' - `Photo` (numeric) Photosynthetic rate in mu  mol CO2 m^-2 s^-1.
#' - `Cond` (numeric) Stomatal conductance mol H2O m^-2 s^-1.
#' - `Ci` (numeric) Intercellular CO2 concentration umol CO2  mol^-1.
#' - `Trmmol` (numeric) Transpiration rate mmol H2O m^-2 s^-1.
#' - `VpdL` (numeric) Leaf-level vapor pressure deficit kPa.
#' - `CTleaf` (numeric) Leaf temperature from energy balance calculation (degrees C).
#' - `Area` (numeric) In-chamber leaf area (cm2; note that this area is not the true sampled tissue area for needleleaf samples).
#' - `BLC_1` (numeric) One sided boundary layer conductance (mol/m2/s)
#' - `StmRat` (integer) Stomatal ratio estimate
#' - `BLCond`(numeric) Boundary layer conductance (mol/m2/s).
#' - `Tair` (numeric) Chamber air temperature (degrees C).
#' - `Tleaf` (numeric) Leaf surface temperature (degrees C).
#' - `TBlk` (numeric) IRGA block temperature (degrees C)
#' - `CO2R` (integer) Reference CO2 concentration (mu  mol/mol)
#' - `CO2S` (integer) Sample CO2 concentration (mu mol/mol)
#' - `H2OR` (numeric) Reference H20 concentration (mu  mol/mol)
#' - `H2OS` (numeric) Sample H20 concentration (mu  mol/mol)
#' - `RH_R` (numeric) Reference relative humidity (%)
#' - `RH_S` (numeric) Sample relative humiditiy (%)
#' - `Flow`  (numeric) Flow rate (mu mol/mol)
#' - `PARi` (numeric) in-chamber PAR (mu mol/m2/s)
#' - `PARo` (numeric) external PAR (mu  mol/m2/s)
#' - `Press` (numeric) Atmospheric pressure (kPa)
#' - `CsMch` (numeric) standard deviation of CO2S during averaging time of most recent match
#' - `HsMch` (numeric) standard deviation of H2OS during averaging time of most recent match
#' - `CsMchSD` (numeric) standard deviation of CO2R during averaging time of most recent match
#' - `HsMchSD` (numeric) standard deviation of H2OS during averaging time of most recent match
#' - `CrMchSD` (numeric) standard deviation of CO2R during averaging time of most recent match
#' - `HrMchSD` (numeric) standard deviation of H2OR during averaging time of most recent match
#' - `StableF` (numeric) stable/total as a fraction
#' - `BLCslope` (numeric) Slope term used in calculating boundary layer conductance
#' - `BLCoffst` (numeric) Intercept term using in calculating boundary layer conductance
#' - `f_parin` (integer) fraction of ParIn_um to use for energy balance
#' - `f_parout` (integer) fraction of ParOut_um to use for energy balance
#' - `alphaK` (numeric) used in conversion of umol/mol to W/m2
#' - `Status` (character) Status variable
#' - `Filename` (character) Name of original file.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Species` (character) Species code from the USDA Plants Database; see
#' - `Sample` (character) Sample number--varies by species by plot
#' - `Filename_date` (character) Calendar date in format MDDYYYY
#' - `Timestamp` (character) Calendar date and time in format M/DD/YYYY HH:MM:SS
#' - `Comments` (character) ID of closest vegetation survey plot (NE, SE, SW, NW) to
#'  the stem measured, plus any additional comments
#' - `DateTime` (POSIXlt) Format in "%Y-%m-%d %H:%M:%S"
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#`
#`
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_photosynthesis()

fd_photosynthesis <- function() {
  leaf_photo <- read_csv_file("fd_photosynthesis.csv")

  # clean original data
  leaf_photo$Plot <- NULL

  # adjusting column data
  leaf_photo$Species <- toupper(leaf_photo$Species)
  leaf_photo$DateTime <- as.POSIXlt(leaf_photo$Timestamp, format = "%Y-%m-%d %H:%M:%S")

  # Split the SubplotID column into more useful individual columns
  leaf_photo$SubplotID <- substr(leaf_photo$Filename, 0, 4)
  leaf_photo$Replicate <- substr(leaf_photo$SubplotID, 1, 1)
  leaf_photo$Plot <- as.integer(substr(leaf_photo$SubplotID, 3, 3))
  leaf_photo$Subplot <- substr(leaf_photo$SubplotID, 4, 4)

  # reorders columns
  #leaf_photo <- leaf_photo[c("SubplotID", "Replicate", "Plot", "Subplot", "Date", "photoies", "Index", "Index_Value", "FilePath")]
  leaf_photo
}


