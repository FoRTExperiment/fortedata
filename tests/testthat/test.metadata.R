context("metadata")

test_that("Metadata", {

  # Read the metadata file
  mdf <- system.file("extdata", "forte_table_metadata.csv", package = "fortedata", mustWork = TRUE)
  md <- read.csv(mdf, stringsAsFactors = FALSE)
  md$class <- tolower(md$class)

  # For each table listed in the metadata file, ensure that the column names and
  # classes listed match what's returned by the corresponding package function
  for(tab in unique(md$table)) {
    mdtab <- md[md$table == tab,]

    dat <- do.call(tab, list())
    datclass <- tolower(sapply(dat, class))
    names(datclass) <- names(dat)

    expect_identical(sort(mdtab$field), sort(names(dat)), label = tab)
    # loop for better error message?
    for(i in seq_len(nrow(mdtab))) {
      dc <- unname(datclass[mdtab$field[i]])  # data class
      # we use grepl here because timestamp fields have multiple classes
      # and we want to keep the metadata table clean
      expect_true(grepl(mdtab$class[i], dc),
                   label = paste(tab, mdtab$field[i], "class", mdtab$class[i], dc))
    }
  }
})

test_that("Metadata function", {
  dat <- fd_metadata()
  # Make sure empty strings are correctly converted to NA
  expect_true(!any(na.omit(dat$units) == ""))
})
