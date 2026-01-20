
#' FoRTE contributors
#'
#' Table of the people and their roles that have contributed to the
#'  NSF Forest Resilience Threshold Experiment Project.
#' @format A data frame with
#' \describe{
#' \item{name}{character, full name}
#' \item{affiliation}{character, affiliation at time contributed to FoRTE}
#' \item{role}{character, brief description of their role in FoRTE}
#' \item{notes}{character, additional notes}
#' }
#' @export
#' @examples
#' \dontrun{
#' fd_contributors()
#' }
fd_contributors <- function() {
  read_csv_file("fd_contributors.csv")
}
