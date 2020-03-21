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
  leaf_spec[c("Replicate", "Plot", "Subplot", "Date", "Species", "Index", "Index_Value")]
}


#' Leaf photosynthesis measurements.
#'
#' @details The columns are as follows:
#' - `Obs` (numeric) Observation number within file.
#' - `HHMMSS` (character) Time of day.
#' - `FTime` (integer) Number of seconds since logging began.
#' - `EBal.` (integer) Energy balance on?
#' - `Photo` (numeric) Photosynthetic rate in µmol CO2 m^-2 s^-1.
#' - `Cond` (numeric) Stomatal conductance mol H2O m^-2 s^-1.
#' - `Ci` (numeric) Intercellular CO2 concentration µmol CO2  mol^-1.
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
#' - `CO2S` (integer) Sample CO2 concentration (µmol/mol)
#' - `H2OR` (numeric) Reference H20 concentration (µmol/mol)
#' - `H2OS` (numeric) Sample H20 concentration (µmol/mol)
#' - `RH_R` (numeric) Reference relative humidity (%)
#' - `RH_S` (numeric) Sample relative humiditiy (%)
#' - `Flow`  (numeric) Flow rate (µmol/mol)
#' - `PARi` (numeric) in-chamber PAR (µmol/m2/s)
#' - `PARo` (numeric) external PAR (µmol/m2/s)
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
#' - `alphaK` (numeric) used in conversion of µmol/mol to W/m2
#' - `Status` (character) Status variable
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Species` (character) Species code from the USDA Plants Database; see
#' - `Sample` (character) Sample number--varies by species by plot
#' - `Comments` (character) ID of closest vegetation survey plot (NE, SE, SW, NW) to
#'  the stem measured, plus any additional comments
#' - `Timestamp` (POSIXlt) Timestamp of measurement
#' - `Replicate` (character): Replicate code
#' - `Plot` (integer): Plot ID number
#' - `Subplot` (character): Subplot code
#`
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#' @author Measurements by Lisa Haber at the University of Michigan Biological Station.
#' @examples
#' fd_photosynthesis()
fd_photosynthesis <- function() {
  leaf_photo <- read_csv_file("fd_photosynthesis.csv")

  # Clean up Species column
  leaf_photo$Species <- toupper(leaf_photo$Species)

  # Create a new Timestamp column that uses the information in Filename and HHMMSS
  # See Lisa's explanation in issue #23
  leaf_photo$Timestamp <- as.POSIXct(paste(substr(leaf_photo$Filename, 14, 22),
                                           leaf_photo$HHMMSS),
                                     format = "%m%d%Y %H:%M:%S",
                                     tz = "America/Detroit")

  # Extract the SubplotID column
  leaf_photo$SubplotID <- substr(leaf_photo$Filename, 1, 4)
  leaf_photo <- split_subplot_id(leaf_photo)

  # Drop a few unneeded fields
  leaf_photo$Filename <- leaf_photo$Filename_date <- leaf_photo$HHMMSS <- NULL

  # Reorder and return
  first_cols <- c("SubplotID", "Replicate", "Plot", "Subplot", "Timestamp")
  other_cols <- setdiff(names(leaf_photo), first_cols)
  leaf_photo[c(first_cols, other_cols)]
}
