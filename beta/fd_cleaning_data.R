##### This script includes package specific data preparation functions/code snippets to convert field data to package data

# dependencies
require(tidyverse)

# functions
read_csv_file <- function(...) {
  weak_as_tibble(
    read.csv(
      system.file("extdata", ..., package = "fortedata", mustWork = TRUE),
      # Empty strings, and ONLY empty strings, should be interpreted as missing values.
      na.strings = "",
      stringsAsFactors = FALSE
    )
  )
}

#########################################
# seedling and sapling data

#### cleaning
df <- read.csv("./junk/seedling_sapling.csv")

# reorganize
x <- df[, c("subplot", "vegplot_direction", "species", "baseD_cm", "height_2018", "height_2019", "notes", "date", "initial")]

x %>%
  gather(year, height_cm, height_2018:height_2019) -> x

# gathering height to one column
x$year[x$year == "height_2018"] <- 2018
x$year[x$year == "height_2019"] <- 2019
x$year <- as.integer(x$year)
x$height_cm <- as.numeric(x$height_cm)


# fixing date to NA since it was not recorded for 2018
x %>%
  mutate(date = ifelse(year == 2018, NA, date)) -> x

# replacing the fucked up dbh class
x$baseD_cm[x$baseD_cm == "0-1"]  <- "0-1 cm"
x$baseD_cm[x$baseD_cm == "2-Jan"] <- "1-2 cm"
x$baseD_cm[x$baseD_cm == "3-Feb"] <- "2-3 cm"



#####CHANGE NSP!



# fd_soil_respiration

################################
# stuff to import
rs <- read.csv("./beta/junk/Rs_2018.csv")

# good stuff
x <- read.csv("./inst/extdata/fd_soil_efflux.csv")

# compare headers
head(rs)
head(x)

# change names
names(rs)[names(rs) == "nestedSubplot"] <- "nestedPlot"
names(rs)[names(rs) == "Run"] <- "run"
names(rs)[names(rs) == "SoilCO2Efflux"] <- "soilCO2Efflux"
names(rs)[names(rs) == "SoilTemp"] <- "soilTemp"
names(rs)[1] <- "Rep_ID"

# formating date
# Retaining date
rs$date <- gsub("18", "2018", rs$date)
rs$time <- NA
rs$dateTime <- NA

# check again
head(rs)
head(x)

rs <- rs[c("Rep_ID", "Plot_ID",  "Subplot",  "nestedPlot",  "run",  "soilCO2Efflux", "soilTemp", "VWC",
            "time", "notes", "date" ,  "dateTime"  )]

x$X <- NULL

# bring together
z <- rbind(x, rs)
y <- merge(x, rs)

# now we write. LAST WRITE to FIL WAS 2021-03-11
# write.csv(z, "./inst/extdata/fd_soil_efflux.csv")
