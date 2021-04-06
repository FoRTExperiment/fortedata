## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata)
)

# include utils.R functions that are not exported from fortedata
read_csv_file <- function(...) {
  weak_as_tibble(
    read.csv(
      system.file("extdata", ..., package = "fortedata", mustWork = TRUE),
      # Empty strings, and ONLY empty strings, should be interpreted as missing values.
      na.strings = "",
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
fd_metadata <- function(table = NULL) {
  md <- read_csv_file("forte_table_metadata.csv")

  if (!is.null(table)) {
    md <- md[md$table == table,]
    if (nrow(md) < 1) {
      stop("Table ", table, " is not present in metadata.")
    }
  }
  weak_as_tibble(md)
}


#' Split the SubplotID column into more useful individual columns
split_subplot_id <- function(df) {
  stopifnot("subplot_id" %in% names(df))
  df$replicate <- substr(df$subplot_id, 1, 1)
  df$plot <- as.integer(substr(df$subplot_id, 3, 3))
  df$subplot <- substr(df$subplot_id, 4, 4)
  df
}




data_conditions <- function(x, published = FALSE, contact_person, citation) {

  if(!published) {
    message("These data are unpublished. Please contact ", contact_person, " to ask about using")
  }

  message("Data citation: ", citation)
  message("Contact person: ", contact_person)

  # add the above information to `x` as attributes...

  invisible(x)
}



## ----cst, echo = TRUE---------------------------------------------------------
#set random seed
  cst <- read_csv_file("canopy_structural_traits.csv")

  # show the top of
  str(cst)

