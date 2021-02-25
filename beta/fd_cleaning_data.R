##### This script includes package specific data preparation functions/code snippets to convert field data to package data

# dependencies
require(tidyverse)

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
x$baseD_cm[x$baseD_cm == "0-1"] <- "0-1 cm"
x$baseD_cm[x$baseD_cm == "2-Jan"] <- "1-2 cm"
x$baseD_cm[x$baseD_cm == "3-Feb"] <- "2-3 cm"



#####CHANGE NSP!
