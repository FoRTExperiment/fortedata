context("metadata")

test_that("Metadata function", {
  dat <- fd_metadata()
  # Make sure empty strings are correctly converted to NA
  expect_true(!any(na.omit(dat[["units"]]) == ""))
  # Make sure that units are parseable
  parseable <- vapply(
    dat[["units"]],
    function(x)
      is.na(x) ||
        # Units with chemicals in them are not recognized. Consider exceptions
        # explicitly to air on the side of caution
        x %in% c("umol H2O/m2/s", "mmol H2O/m2/s", "umol CO2/m2/s") ||
        udunits2::ud.is.parseable(x),
    logical(1)
  )
  expect_equal(
    length(parseable), sum(parseable),
    label = "Parseable units",
    expected.label = "Units"
  )

  # Make sure everything in tables column refers to a `fortedata` function,
  # and that subsetting metadata works correctly.
  tables <- unique(dat[["table"]])
  for (tab in tables) {
    expect_true(exists(!!tab))
    target_dat <- do.call(tab, list())
    target_meta <- fd_metadata(tab)
    expect_true(all(colnames(target_dat) %in% target_meta[["field"]]))
    expect_true(all(target_meta[["field"]] %in% colnames(target_dat)))
    expect_true(is.data.frame(target_dat), label = tab)
  }

  expect_error(
    fd_metadata("notatable"),
    "Table notatable is not present in metadata"
  )

})

test_that("Metadata CSV file", {

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
