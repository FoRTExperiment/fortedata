# this is just updating table metadata with the fd_double_dendro() column names.
# I create df with the new metadata and then append it to the existing metadata using rbind(). Lastly,
# I overwrite the existing forte_table_metadata.csv

df <- data.frame(table = "fd_double_dendro",
                 field = c("tag", "species", "band_in_bottom", "band_in_top", "date", "notes"),
                 description = c("Tree tag ID number", "Species code from the USDA Plants Database",
                                   "Measurement of lower (bottom) dendroband in inches",
                                   "Measurement of upper (top) dendroband in inches",
                                   "Date of measurement", "Field notes"),
                 class = c("integer", "character", "numeric", "numeric", "character", "character"),
                 units = c("","","inches","inches","",""))

#md <- read.csv("inst/extdata/forte_table_metadata.csv")

#md_update <- rbind(md, df)

#write.csv(md_update, "inst/extdata/forte_table_metadata.csv", row.names = FALSE, quote = FALSE)
