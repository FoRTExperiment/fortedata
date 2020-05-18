# fd_make_release

#' Write files for a GitHub data release
#'
#' @param path Path to write files to, character
#' @param zip_release Zip files into a single archive? Logical
#' @return Fully-qualified filename of release file
#' @importFrom utils write.csv packageVersion
#' @export
fd_make_release <- function(path, zip_release = TRUE) {
  stopifnot(is.character(path))
  stopifnot(is.logical(zip_release))

  fmd <- fd_metadata()

  if(!dir.exists(path)) {
    stop("Output directory doesn't exist!")
  }

  # for each table in the metadata, call function to get it
  for(tab in unique(fmd$table)) {
    message("Loading ", tab, "...", appendLF = FALSE)
    x <- do.call(tab, args = list())
    message("writing")
    write.csv(x, file.path(path, paste0(tab, ".csv")))
  }

  # Write the metadata file
  write.csv(fmd, file.path(path, "fortedata_metadata.csv"))

  # Almost done! Zip everything up into a single file
  release_file <- paste0("fortedata-", packageVersion("fortedata"), ".zip")
  if(zip_release) {
    message("Zipping...")
    wd <- getwd()
    setwd(path)
    utils::zip(release_file,
               list.files("./", full.names = TRUE, recursive = TRUE))
    setwd(wd)
  }

  file.path(path, release_file)
}
