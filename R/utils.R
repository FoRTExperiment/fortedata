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
  weak_as_tibble(
    read.csv(
      system.file("extdata", ..., package = "fortedata", mustWork = TRUE),
      stringsAsFactors = FALSE
    )
  )
}


# weak_tibble - use tibble() if available but fall back to
# data.frame() if necessary
# not a user-facing function; document via roxygen?
weak_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  out <- data.frame(..., stringsAsFactors = FALSE)
  if (!(.force_df || no_tibble)) out <- weak_as_tibble(out)
  out
}

# weak_as_tibble - use as_tibble() if available but fall back to
# as.data.frame() if necessary
weak_as_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    as.data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::as_tibble(...)
  }
}
