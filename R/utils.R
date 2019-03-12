# Utilities

read_csv_file <- function(fn) {
  read.csv(
    system.file("extdata", fn, package = "fortedata", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}
