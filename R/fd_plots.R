# Plot tables


#' Plots table.
#'
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_plots()
fd_plots <- function() {
  read_csv_file("fd_plots.csv")
}


#' Subplots table.
#'
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_subplots()
fd_subplots <- function() {
  read_csv_file("fd_subplots.csv")
}


#' Nested subplots table.
#'
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_nested_subplots()
fd_nested_subplots <- function() {
  read_csv_file("fd_nested_subplots.csv")
}

#' Treatment assignments
#'
#' @return A `data.frame` or `tibble`. Call \code{\link{fd_metadata}} for field metadata.
#' @export
#' @author Measurements by Gough Lab at the University of Michigan Biological Station.
#' @examples
#' fd_treatments()
fd_treatments <- function() {
  x <- read_csv_file("forte_plot_metadata.csv")

  x <- x[c("replicate", "plot", "subplot","disturbance_severity", "treatment")]

  weak_as_tibble(x)
}
