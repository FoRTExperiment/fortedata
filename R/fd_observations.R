#' Number of observations in the FoRTE Dataset
#'
#' @note No. of observations for many, but not all datasets
#'
#' #' @return A `data.frame` or `tibble` of the observation tally.
#' Call \code{\link{fd_metadata}} for field metadata.
#' @export
#'
#' @examples
#' fd_observations()
fd_observations <- function() {

  begin_date <- as.Date("2018-01-01")
  current_date <- Sys.Date()

  # create length of dates
  timeframe <- seq.Date(from = begin_date, to = current_date, by = "month", format = "%Y-%m")

  # Empty data frame
  timeframe <- data.frame(timeframe)

  timeframe$month <- as.numeric(format(timeframe$timeframe, "%m"))
  timeframe$year <- as.numeric(format(timeframe$timeframe, "%Y"))

  #####################
  # CEPTOMETER!
  a <- suppressMessages(suppressWarnings(fd_ceptometer()))

  # this makes the year and month column
  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$timestamp), "%m"))
  a$year <- as.numeric(format(as.Date(a$timestamp), "%Y"))

  # count it up
  a.tally <- aggregate(lai_cept ~ month + year, data = a, FUN = length)
  #a.tally$Table <- "fd_ceptometer"
  names(a.tally)[names(a.tally) == "lai_cept"] <- "no_of_obs"

  # make time composite
  no_par <- data.frame(timeframe, Table = "fd_ceptometer")
  no_par <- merge(no_par, a.tally, by = c("month", "year"), all = TRUE)

  #####################
  # SOIL RESPIRATION!
  a <- suppressWarnings(fd_soil_respiration())

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(soil_co2_efflux ~ month + year, data = a, FUN = length)
  #a.tally$Table <- "fd_ceptometer"
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_soil_respiration")
  no_soil_r <- merge(x, a.tally, by = c("month", "year"), all = TRUE)


  #####################
  # LEAF SPECTROMETRY!
  b <- suppressMessages(suppressWarnings(fd_leaf_spectrometry()))

  # this makes the year and month column
  # this makes the year and month column
  b$month <- as.numeric(format(as.Date(b$date), "%m"))
  b$year <- as.numeric(format(as.Date(b$date), "%Y"))

  # count it up
  b.tally <- aggregate(index ~ month + year, data = b, FUN = length)
  names(b.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_leaf_spectrometry")
  no_leaf_spec <- merge(x, b.tally, by = c("month", "year"), all = TRUE)

  #####################
  # PHOTOSYNTHESIS
  a <- suppressMessages(suppressWarnings(fd_photosynthesis()))

  # this makes the year and month column
  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$timestamp), "%m"))
  a$year <- as.numeric(format(as.Date(a$timestamp), "%Y"))

  # count it up
  a.tally <- aggregate(photo ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_photosynthesis")
  no_photo <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  #####################
  # hemi camera
  a <- suppressMessages(suppressWarnings(fd_hemi_camera()))

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(lai_cam ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_hemi_camera")
  no_cam <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  ##############################
  no_of_records <- rbind(no_soil_r, no_leaf_spec, no_photo, no_cam, no_par)

  # change the table column to be character in line w/ package convention
  no_of_records$Table <- as.character(no_of_records$Table)
  names(no_of_records)[names(no_of_records) == "Table"] <- "table"

  #####################
  # pcl lidar data
  a <- suppressMessages(suppressWarnings(fd_canopy_structure()))

  # this makes the year and month column
  a$date <- as.Date(paste(a$year, 7, 1, sep = "-"), "%Y-%m-%d") # this line is added b/c the data set does not have a date column, but rather just a year column. All data are gathered in July.
  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(rugosity ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_canopy_structure")
  no_csc <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  #####################
  # forest inventory
  a <- suppressMessages(suppressWarnings(fd_inventory()))

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(dbh_cm ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_inventory")
  no_inv <- merge(x, a.tally, by = c("month", "year"), all = TRUE)


  #####################
  # litter
  a <- suppressMessages(suppressWarnings(fd_litter()))

  a$date <- as.Date("2018-11-15")
  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(species ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_litter")
  no_leaf <- merge(x, a.tally, by = c("month", "year"), all = TRUE)


  ##########
  # Dendro data
  a <- suppressMessages(suppressWarnings(fd_dendro()))

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(band_cm ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_dendro")
  no_dendro <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  ###########################
  # Subcanopy diameter data

  a <- suppressMessages(suppressWarnings(fd_subcanopy_diameter()))

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(dbh_mm ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_subcanopy_diameter")
  no_sc_d <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  #############################
  # subcanopy density data

  a <- suppressMessages(suppressWarnings(fd_subcanopy_density()))

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(count ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_subcanopy_density")
  no_sc_dens <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  #############################
  # seedling & sapling data

  a <- suppressMessages(suppressWarnings(fd_seedling_sapling()))

  # this makes the year and month column
  a$month <- as.numeric(format(as.Date(a$date), "%m"))
  a$year <- as.numeric(format(as.Date(a$date), "%Y"))

  # count it up
  a.tally <- aggregate(height_total_cm ~ month + year, data = a, FUN = length)
  names(a.tally)[3] <- "no_of_obs"

  # make time composite
  x <- data.frame(timeframe, Table = "fd_seedling_sapling")
  no_ss <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

  ##############################
  no_of_records <- rbind(no_soil_r, no_leaf_spec, no_photo, no_cam, no_par, no_csc,
                         no_inv, no_leaf, no_dendro, no_sc_d, no_sc_dens, no_ss)

  # no_of_records$month <- as.numeric(no_of_records$month)
  # no_of_records$year <- as.numeric(no_of_records$year)

  # change the table column to be character in line w/ package convention
  no_of_records$Table <- as.character(no_of_records$Table)
  names(no_of_records)[names(no_of_records) == "Table"] <- "table"
  #########################
  weak_as_tibble(no_of_records)
}

