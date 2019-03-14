# Plot tables


#' Return the plots table.
#'
#' @details The columns are as follows:
#' - `Replicate` (character): Replicate code. Each replicate contains
#'   multiple plots with different disturbance treatments.
#' - `Plot` (integer): Plot ID number. Plots are nested within replicates.
#' - `Longitude_plot`, `Latitude_plot` (double): Plot coordinates, in decimal
#'   degrees.
#' - `Plot_area_m2` (double): Area of the plot, in square meters.
#'
#' @return A `data.frame`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_plots()
fd_plots <- function() {
  read_csv_file("fd_plots.csv")
}


#' Return the subplots table.
#'
#' @details The columns are as follows:
#' - `Replicate` (character): Replicate code. Each replicate contains
#'   multiple plots with different disturbance treatments.
#' - `Plot` (integer): Plot ID number. Plots are nested within replicates.
#' - `Subplot` (character): Subplot code. Subplots are nested within plots:
#' one in the eastern part, one in the western.
#' - `Longitude_subplot`, `Latitude_subplot` (double): Subplot coordinates, in decimal
#'   degrees.
#' - `Plot_area_m2` (double): Area of the subplot, in square meters.
#'
#' @return A `data.frame`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_subplots()
fd_subplots <- function() {
  read_csv_file("fd_subplots.csv")
}


#' Return the nested subplots table.
#'
#' @details The columns are as follows:
#' - `Subplot` (character): Subplot code. Subplots are nested within plots:
#' one in the eastern part, one in the western.
#' - `Nested_subplot` (integer): Nested subplot code. Nested subplots are nested subplots,
#' which are in turn are within plots:
#' one in the eastern part, one in the western.
#' - `Nested_subplot_area_m2` (double): Area of the nested subplot, in square meters.
#'
#' @return A `data.frame`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_nested_subplots()
fd_nested_subplots <- function() {
  read_csv_file("fd_nested_subplots.csv")
}
