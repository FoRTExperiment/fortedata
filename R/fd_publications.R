# FoRTE bibliography


#' FoRTE related publications
#'
#' Table of publications produced as part of the NSF Forest Resilience Threshold Experiment
#' Project.
#' @format A data frame with
#' \describe{
#' \item{entry}{Abbreviated in-text entry in the form of author last name and year}
#' \item{pub_year}{integer year of publication}
#' \item{title}{character, publication title}
#' \item{journal}{character, publishing journal}
#' \item{doi}{character, the Digital Object Identifier}
#' }
#' @export
#' @examples
#' \dontrun{
#' fd_publications()
#' }
fd_publications <- function() {
  read_csv_file("fd_pubs.csv")
}
