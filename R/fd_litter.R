# Litterfall data from litter trap collections

#' Litter mass data from litter trap collections
#'
#' These data include leaf litter mass sorted by species as well as fine wood debris (`fwd` column) in grams collected annually every ~November from the litter traps located in each FoRTE  subplot. The `fraction` column informs the user is each row is either leaves which are broken down by species in the `species` column; misc which are small, unidentifiable leaf fragements or organic material; or fwd, for fine woody debris which are small twigs and sticks. For 2018-2020, there are four litter traps in each subplot. Each littertrap is circular (1.82 m circumference, 0.29 m radius, 0.26 m^2 area).See `fd_experimental_design_vignette` for more information. Within the dataset, the `bagtare_g` column is the mass of the measuring bag or tare and `bagmass_g` is the total mass of bag + sample.
#'
#' @note Data were collected by multiple Gough Lab team members
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_litter()
fd_litter <- function() {
  litter <- read_csv_file("fd_littertrap.csv")

  # make lower case
  names(litter) <- tolower(names(litter))

  # reformat subplot_id
  names(litter)[names(litter) == "subplotid"] <- "subplot_id"

  # format species names
  litter$species <- toupper(litter$species)

  # add subplot_id information . . . plot, replicate, subplot
  litter <- split_subplot_id(litter)

  # Reorder columns, dropping unneeded ones
  litter <- litter[c("subplot_id", "replicate", "plot", "subplot", "year", "fraction", "species", "bagtare_g", "bagmass_g")]
  weak_as_tibble(litter)


  # Data creation and authorship information
  contact_person <- "Jeff Atkins"
  citation <- "ESSD"

  # data conditions
  data_conditions(litter, published = FALSE, contact_person, citation)
}

# Coarse woody debris survey data

#' Coarse wood debris survey data
#'
#' These data include (`fwd` column) in grams collected annually every ~November from the litter traps located in each FoRTE  subplot. The `fraction` column informs the user is each row is either leaves which are broken down by species in the `species` column; misc which are small, unidentifiable leaf fragements or organic material; or fwd, for fine woody debris which are small twigs and sticks. For 2018-2020, there are four litter traps in each subplot. Each littertrap is circular (1.82 m circumference, 0.29 m radius, 0.26 m^2 area).See `fd_experimental_design_vignette` for more information. Within the dataset, the `bagtare_g` column is the mass of the measuring bag or tare and `bagmass_g` is the total mass of bag + sample.
#'
#' @note Data were collected by multiple Gough Lab team members
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @examples
#' fd_litter()
fd_cwd <- function() {
  cwd <- read_csv_file("fd_cwd.csv")

  # remove the weird empty lines
  cwd <- cwd[stats::complete.cases(cwd), ]

  # make it pretty
  cwd$subplot_id <-  paste(cwd$plot, cwd$subplot, sep = "")

  # remove errant stuff
  cwd$plot <- NULL
  cwd$subplot <- NULL

  # add subplot_id information . . . plot, replicate, subplot
  cwd <- split_subplot_id(cwd)

  # format date
  cwd$date <- as.Date(cwd$date, format = "%m/%d/%Y")

  # remove weirdness in tag number
  cwd$tag_number[cwd$tag_number == "na"] <- NA

  # Reorder columns, dropping unneeded ones
  cwd <- cwd[c("subplot_id", "replicate", "plot", "subplot", "date", "width_end1_cm", "width_mid_cm", "width_end2_cm",
               "length_m", "decay_class", "tag_number", "initials")]
  weak_as_tibble(cwd)


  # Data creation and authorship information
  contact_person <- "Jeff Atkins"
  citation <- "ESSD"

  # data conditions
  data_conditions(cwd, published = FALSE, contact_person, citation)
}
