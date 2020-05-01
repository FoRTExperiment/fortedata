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

#
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
a <- fd_ceptometer()

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
b <- fd_soil_respiration()

# this makes the year and month column
b$month <- format(as.Date(b$timestamp), "%m")
b$year <- format(as.Date(b$timestamp), "%Y")

# count it up
b.tally <- aggregate(soil_co2_efflux ~ month + year, data = b, FUN = length)
#a.tally$Table <- "fd_ceptometer"
names(b.tally)[3] <- "no_of_obs"

# make time composite
x <- data.frame(timeframe, Table = "fd_soil_respiration")
no_soil_r <- merge(x, b.tally, by = c("month", "year"), all = TRUE)

#####################
# LEAF SPECTROMETRY!
b <- fd_leaf_spectrometry()

# this makes the year and month column
b$month <- format(as.Date(b$date), "%m")
b$year <- format(as.Date(b$date), "%Y")

# count it up
b.tally <- aggregate(index ~ month + year, data = b, FUN = length)
names(b.tally)[3] <- "no_of_obs"

# make time composite
x <- data.frame(timeframe, Table = "fd_leaf_spectrometry")
no_leaf_spec <- merge(x, b.tally, by = c("month", "year"), all = TRUE)

#####################
# PHOTOSYNTHESIS
a <- fd_photosynthesis()

# this makes the year and month column
a$month <- format(as.Date(a$timestamp), "%m")
a$year <- format(as.Date(a$timestamp), "%Y")

# count it up
a.tally <- aggregate(photo ~ month + year, data = a, FUN = length)
names(a.tally)[3] <- "no_of_obs"

# make time composite
x <- data.frame(timeframe, Table = "fd_photosynthesis")
no_photo <- merge(x, a.tally, by = c("month", "year"), all = TRUE)

#####################
# hemi camera
a <- fd_hemi_camera()

# this makes the year and month column
a$month <- format(as.Date(a$date), "%m")
a$year <- format(as.Date(a$date), "%Y")

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

weak_as_tibble(no_of_records)
}
