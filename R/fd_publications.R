# FoRTE bibliography


#' Return a list of all FoRTE related publications.
#'
#' @note A list of all FoRTE related publications.
#'
#' @details The columns are as follows:
#' - `Entry` (character): Abbreviated in-text entry in the form of author last name and year
#' - `PubYear` (integer): Year of publication
#' - `Title` (character): Title of publication
#' - `Journal` (character): Journal article was published in
#' - `DOI` (character): Digital Object Identifier
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_publications()
fd_publications <- function() {
  read_csv_file("fd_pubs.csv")
}
