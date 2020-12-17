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


