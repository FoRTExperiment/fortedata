# Litterfall data from litter trap collections

#' Litter trap data.
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_litter()
fd_litter <- function() {
  litter <- read_csv_file("fd_littertrap.csv")

  litter$Species <- toupper(litter$Species)
  litter <- split_subplot_id(litter)

  # Reorder columns, dropping unneeded ones
  litter <- litter[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "Species", "BagTare_g", "BagMass_g")]
  weak_as_tibble(litter)
}


