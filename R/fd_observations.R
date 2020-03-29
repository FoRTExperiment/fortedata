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
BeginDate <- as.Date("2018-01-01")
CurrentDate <- Sys.Date()

# create length of dates
TimeFrame <- seq.Date(from = BeginDate, to = CurrentDate, by = "month", format = "%Y-%m")

# Empty data frame
timeframe <- data.frame(TimeFrame)

timeframe$Month <- format(timeframe$TimeFrame, "%m")
timeframe$Year <- format(timeframe$TimeFrame, "%Y")

#####################
# CEPTOMETER!
a <- fd_ceptometer()

# this makes the year and month column
a$Month <- format(as.Date(a$Timestamp), "%m")
a$Year <- format(as.Date(a$Timestamp), "%Y")

# count it up
a.tally <- aggregate(LAI_cept ~ Month + Year, data = a, FUN = length)
#a.tally$Table <- "fd_ceptometer"
names(a.tally)[names(a.tally) == "LAI_cept"] <- "NoRecs"

# make time composite
no_par <- data.frame(timeframe, Table = "fd_ceptometer")
no_par <- merge(no_par, a.tally, by = c("Month", "Year"), all = TRUE)

#####################
# SOIL RESPIRATION!
b <- fd_soil_respiration()

# this makes the year and month column
b$Month <- format(as.Date(b$Date), "%m")
b$Year <- format(as.Date(b$Date), "%Y")

# count it up
b.tally <- aggregate(soilCO2Efflux ~ Month + Year, data = b, FUN = length)
#a.tally$Table <- "fd_ceptometer"
names(b.tally)[3] <- "NoRecs"

# make time composite
x <- data.frame(timeframe, Table = "fd_soil_respiration")
no_soilR <- merge(x, b.tally, by = c("Month", "Year"), all = TRUE)

#####################
# LEAF SPECTROMETRY!
b <- fd_leaf_spectrometry()

# this makes the year and month column
b$Month <- format(as.Date(b$Date), "%m")
b$Year <- format(as.Date(b$Date), "%Y")

# count it up
b.tally <- aggregate(Index ~ Month + Year, data = b, FUN = length)
names(b.tally)[3] <- "NoRecs"

# make time composite
x <- data.frame(timeframe, Table = "fd_leaf_spectrometry")
no_leafSpec <- merge(x, b.tally, by = c("Month", "Year"), all = TRUE)

#####################
# PHOTOSYNTHESIS
a <- fd_photosynthesis()

# this makes the year and month column
a$Month <- format(as.Date(a$Timestamp), "%m")
a$Year <- format(as.Date(a$Timestamp), "%Y")

# count it up
a.tally <- aggregate(Photo ~ Month + Year, data = a, FUN = length)
names(a.tally)[3] <- "NoRecs"

# make time composite
x <- data.frame(timeframe, Table = "fd_photosynthesis")
no_photo <- merge(x, a.tally, by = c("Month", "Year"), all = TRUE)

#####################
# hemi camera
a <- fd_hemi_camera()

# this makes the year and month column
a$Month <- format(as.Date(a$Date), "%m")
a$Year <- format(as.Date(a$Date), "%Y")

# count it up
a.tally <- aggregate(LAI_cam ~ Month + Year, data = a, FUN = length)
names(a.tally)[3] <- "NoRecs"

# make time composite
x <- data.frame(timeframe, Table = "fd_hemi_camera")
no_cam <- merge(x, a.tally, by = c("Month", "Year"), all = TRUE)

##############################
no_of_records <- rbind(no_soilR, no_leafSpec, no_photo, no_cam, no_par)

# change the table column to be character in line w/ package convention
no_of_records$Table <- as.character(no_of_records$Table)

weak_as_tibble(no_of_records)
}
