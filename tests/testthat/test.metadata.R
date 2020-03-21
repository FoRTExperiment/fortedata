context("metadata")

test_that("Metadata", {

  # Read the metadata file
  mdf <- system.file("extdata", "forte_table_metadata.csv", package = "fortedata", mustWork = TRUE)
  md <- read.csv(mdf, stringsAsFactors = FALSE)
  md$Class <- tolower(md$Class)

  # For each table listed in the metadata file, ensure that the column names and
  # classes listed match what's returned by the corresponding package function
  for(tab in unique(md$Table)) {
    mdtab <- md[md$Table == tab,]

    dat <- do.call(tab, list())
    datclass <- tolower(sapply(dat, class))
    names(datclass) <- names(dat)

    expect_identical(sort(mdtab$Field), sort(names(dat)), label = tab)
    # loop for better error message?
    for(i in seq_len(nrow(mdtab))) {
      dc <- unname(datclass[mdtab$Field[i]])  # data class
      # we use grepl here because timestamp fields have multiple classes
      # and we want to keep the metadata table clean
      expect_true(grepl(mdtab$Class[i], dc),
                   label = paste(tab, mdtab$Field[i], "class", mdtab$Class[i], dc))
    }
  }
})
