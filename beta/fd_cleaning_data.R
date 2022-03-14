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





# photosynthesis
####################################
## Lisa T. Haber                  ##
## 2018.10.25                     ##
## FoRTE Subcanopy Physiology     ##
####################################

# This code is my first attempt to extract and utilize subcanopy physiology (LICOR 6400XT) files from the shared FoRTE data drive.

# I am building this off Jeff Atkins' code from the FoRTE Canopy repo.

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)

# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology"
as_id("https://drive.google.com/drive/folders/1Q2k5eSuk0Gr6d-lPECksonbP6FbNwmjz") %>%
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

# Download data
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with eight digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{8}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line

  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>%
      gsub('\"', "", .)

    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>%
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>%
  bind_rows %>%
  as_tibble %>%
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) ->
  licordata


##############################
# get means of 5 observations per leaf to use in analysis

licordata_means <- licordata %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo))
licordata_means

write.table(licordata_means,"Mean_Photo_Cond_2018.txt",sep="\t",row.names=FALSE)
write.csv(licordata_means, "Mean_Photo_Cond_2018.csv", row.names = FALSE)
