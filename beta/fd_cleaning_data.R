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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####################################
## Cameron Clay                   ##
## Initiated 2021.10.7            ##
## FoRTE Leaf Litter Data         ##
####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#The intention of this code is to take the current 2018, 2019 and 2020
#leaf litter data and put it into a format where it can all be stored as
#a master file, in the format of the leaf litter file found in the github repo
#at fortedata/inst/extdata/fd_littertrap.csv

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Libraries

library(dplyr)
library(readr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Pulling in Data

#Reading in the files:

leaf_2018 <- read.csv("data/littertrap_leaf_mass_master - Sheet1.csv")
leaf_2019 <- read.csv("data/Litter_Trap_Masses_FoRTE_November_2019.xlsx - Sheet1 (1).csv")
leaf_2020 <- read.csv("data/Litter_Trap_Masses_FoRTE_2020 - Sheet1.csv")

#For reference, pulling in the github master sheet that I'm trying to match:

leaf_ref <- read_csv(url("https://raw.githubusercontent.com/camerclay/fortedata/master/inst/extdata/fd_littertrap.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2018 Data

#Since the data stored in this master folder for 2019 seems flawed, and
#there is another data sheet dedicated to 2019 data, I will pull just 2018
#data from this file:

leaf_2018 <- leaf_2018 %>% filter(Year == 2018)


#Now, checking the different categories for Species so I can match them up:

unique(leaf_2018$Species)

#Two levels of issues: consistency of spelling a given species and consistency
#with the master sheet format.


#First level: mispelling species

#fagre was written in sometimes, and this will be replaced with fagr.

#New column being created:

leaf_2018$species <- ifelse(leaf_2018$Species == "fagre", "fagr", leaf_2018$Species)

unique(leaf_2018$species)

#Second level: mismatched codes
# swd was used once, and looking back at the hard copy it represents sticks.
#This will be changed to "cwd" for now to match the usual stick designation in this code.

#mix was used once to represent misc leaf bits based on the hard copy, and this will
#be switched to match misc like the rest of the sheet.

leaf_2018$species <- ifelse(leaf_2018$species == "swd", "cwd",
                            ifelse(leaf_2018$species == "mix", "misc", leaf_2018$species))

unique(leaf_2018$species)

#Now, to make it match the reference.

#"cwd" should be switched to "fwd"
#A new column should be assigned, called fraction, where all leaf categories are
#labeled "leaf", misc leaf bits as "misc", and all fwd bits as "fwd"
#Then, all "fwd", "misc" can be submitted as blanks in the species column.

#cwd to fwd:

leaf_2018$species <- ifelse(leaf_2018$species == "cwd", "fwd", leaf_2018$species)

#New column:

leaf_2018$fraction <- ifelse(leaf_2018$species == "quru" | leaf_2018$species == "bepa" |
                               leaf_2018$species == "fagr" | leaf_2018$species == "pogr" |
                               leaf_2018$species == "acsa" | leaf_2018$species == "acru" |
                               leaf_2018$species == "potr" | leaf_2018$species == "acpe" |
                               leaf_2018$species == "amel" | leaf_2018$species == "pire" |
                               leaf_2018$species == "pist", "leaf",
                             ifelse(leaf_2018$species == "misc", "misc",
                                    ifelse(leaf_2018$species == "fwd", "fwd", "check_error")))

#Now, print blanks for the fwd, misc in species column:

leaf_2018$species_new <- ifelse(leaf_2018$species == "misc" | leaf_2018$species == "fwd", "", leaf_2018$species)

#Select desired columns and rename to match master:

leaf_2018 <- leaf_2018 %>% mutate(bag_no = NA)

leaf_2018 <- leaf_2018 %>% select(SubplotID, fraction, species_new, bag_no, BagTare_g, BagMass_g, Year, Notes) %>%
  rename(Species = species_new)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2019 Data

#Checking the categories under species in this sheet:

unique(leaf_2019$SPECIES)

#2019, 2020 and future data sheets have fruit as its own category. I will go ahead
#and label these as "fruit".
#"sticks" will be "fwd", "leaf_bits" will be "misc"
#OSVA will be OSVI, as this is a known species in northern MI. Will need to check
#to make sure this is correct, but someone might have put OSVA just because
#Its Os-virginica.

#First, make them lowercase:

leaf_2019$SPECIES <- tolower(leaf_2019$SPECIES)

#Now, the above fixes:

leaf_2019$Species <- ifelse(leaf_2019$SPECIES == "leaf_bits", "misc",
                            ifelse(leaf_2019$SPECIES == "seeds_fruit", "fruit",
                                   ifelse(leaf_2019$SPECIES == "sticks", "fwd",
                                          ifelse(leaf_2019$SPECIES == "osva", "osvi", leaf_2019$SPECIES))))

#New fraction column:

#New column:

leaf_2019$fraction <- ifelse(leaf_2019$Species == "quru" | leaf_2019$Species == "bepa" |
                               leaf_2019$Species == "fagr" | leaf_2019$Species == "pogr" |
                               leaf_2019$Species == "acsa" | leaf_2019$Species == "acru" |
                               leaf_2019$Species == "potr" | leaf_2019$Species == "acpe" |
                               leaf_2019$Species == "amel" | leaf_2019$Species == "pire" |
                               leaf_2019$Species == "pist" | leaf_2019$Species == "osvi", "leaf",
                             ifelse(leaf_2019$Species == "misc", "misc",
                                    ifelse(leaf_2019$Species == "fwd", "fwd",
                                           ifelse(leaf_2019$Species == "fruit", "fruit", "check_error"))))

#Now, print blanks for the fwd, misc in species column:

leaf_2019$Species<- ifelse(leaf_2019$Species == "misc" | leaf_2019$Species == "fwd" | leaf_2019$Species == "fruit",
                           "", leaf_2019$Species)

#Combining the two notes columns:

leaf_2019$notes_comb <- paste(leaf_2019$notes, leaf_2019$notes.1)

#Select desired columns and rename to match master:

leaf_2019 <- leaf_2019 %>% select(SUBPLOT, fraction, Species, bag_number,
                                  bag_weight_initial_g, bag_weight_final_g, year, notes_comb) %>% rename(
                                    SubplotID = SUBPLOT, bag_no = bag_number, BagTare_g = bag_weight_initial_g,
                                    BagMass_g = bag_weight_final_g, Year = year, Notes = notes_comb)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2020 Data

#2020 processing procedes very similar to 2019:

unique(leaf_2020$SPECIES)

#Making it lowercase:

leaf_2020$SPECIES <- tolower(leaf_2020$SPECIES)

#Now the same fixes as before:

leaf_2020$Species <- ifelse(leaf_2020$SPECIES == "leaf_bits", "misc",
                            ifelse(leaf_2020$SPECIES == "seeds_fruit", "fruit",
                                   ifelse(leaf_2020$SPECIES == "sticks", "fwd",
                                          leaf_2020$SPECIES)))

#New fraction column:

leaf_2020$fraction <- ifelse(leaf_2020$Species == "quru" | leaf_2020$Species == "bepa" |
                               leaf_2020$Species == "fagr" | leaf_2020$Species == "pogr" |
                               leaf_2020$Species == "acsa" | leaf_2020$Species == "acru" |
                               leaf_2020$Species == "potr" | leaf_2020$Species == "acpe" |
                               leaf_2020$Species == "amel" | leaf_2020$Species == "pire" |
                               leaf_2020$Species == "pist", "leaf",
                             ifelse(leaf_2020$Species == "misc", "misc",
                                    ifelse(leaf_2020$Species == "fwd", "fwd",
                                           ifelse(leaf_2020$Species == "fruit", "fruit", "check_error"))))

leaf_2020$Species <- ifelse(leaf_2020$Species == "misc" | leaf_2020$Species == "fwd" | leaf_2020$Species == "fruit",
                            "", leaf_2020$Species)

#Selecting columns and renaming to match master:

leaf_2020 <- leaf_2020 %>% select(SUBPLOT, fraction, Species, bag_number,
                                  bag_weight_initial_g, bag_weight_final_g, year, notes) %>% rename(
                                    SubplotID = SUBPLOT, bag_no = bag_number, BagTare_g = bag_weight_initial_g,
                                    BagMass_g = bag_weight_final_g, Year = year, Notes = notes)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#COMBINING

#These 3 sheets will be combined to make the new leaf_litterfall_master file,
#which can be the sheet that is added to from now on.

littertrap_biomass_master <- rbind(leaf_2018, leaf_2019, leaf_2020)

