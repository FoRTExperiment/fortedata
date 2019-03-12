# Utilities

#' Read an internal package CSV file.
#'
#' @param ... Filename, character
#'
#' @return The read-in dataframe.
#' @keywords internal
#' @importFrom utils read.csv
#' @details \code{...} is passed to \code{\link{system.file}},
#' so can be vectors specifying
#' subdirectory and file(s) within the package.
#' @note This is an internal function and not intended to be called directly.
read_csv_file <- function(...) {
  read.csv(
    system.file("extdata", ..., package = "fortedata", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}
