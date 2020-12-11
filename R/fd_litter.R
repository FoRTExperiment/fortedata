# Litterfall data from litter trap collections

#' Litter trap data.
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
}


