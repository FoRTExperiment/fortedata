# Plot table


#' Return the plots table.
#'
#' @return A `data.frame`.
#' @export
#'
#' @examples
#' fd_plots()
fd_plots <- function() {
  read.csv(
    system.file("extdata", "fd_plots.csv", package = "fortedata", mustWork = TRUE)
  )
}
