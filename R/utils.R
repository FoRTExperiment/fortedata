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


#' Data tables' metadata.
#'
#' @param table Name of table to return, character, optional
#' @return A `data.frame` or `tibble` holding field metadata.
#' @export
fd_metadata <- function(table = NULL) {
  md <- read_csv_file("forte_table_metadata.csv")

  if(!is.null(table) && table %in% md$Table) {
    md <- md[md$Table == table,]
  }
  weak_as_tibble(md)
}


#' Split the SubplotID column into more useful individual columns
#'
#' @param df A data.frame with a four-character \code{SubplotID} column
#'
#' @return The data frame with new columns \code{Replicate}, \code{Plot},
#' and \code{Subplot}.
#' @keywords internal
#' @examples
#' fortedata:::split_subplot_id(data.frame(SubplotID = "A01E"))
split_subplot_id <- function(df) {
  stopifnot("SubplotID" %in% names(df))
  df$Replicate <- substr(df$SubplotID, 1, 1)
  df$Plot <- as.integer(substr(df$SubplotID, 3, 3))
  df$Subplot <- substr(df$SubplotID, 4, 4)
  df
}

#' FoRTE color palette

#' The FoRTE color palettte
#'
#' These are a handful of color palettes pulled from photographs of US National Parks.
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#'
#' pal <- forte_colors()
forte_colors <- function() {
  pal <- c("#000000", "#009E73", "#0072B2", "#D55E00")
  pal <- structure(pal, class = "palette", name = 'forte')
}



#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
