# Plot table


#' Return the plots table.
#'
#' @details The columns are as follows:
#' - `Replicate` (character): Replicate code. Each replicate contains
#'   multiple plots with different disturbance treatments.
#' - `Plot` (integer): Plot ID number. Plots are nested within replicates.
#' - `Latitude`, `Longitude` (double): Plot coordinates, in decimal
#'   degrees.
#' - `Disturbance_severity` (double): Level of disturbance severity,
#'   expressed as a percent (0 - 100) reduction in the target variable (TBD).
#'
#' @return A `data.frame`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_plots()
fd_plots <- function() {
  read.csv(
    system.file("extdata", "fd_plots.csv",
                package = "fortedata", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}
