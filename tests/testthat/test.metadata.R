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
    suppressMessages({target_dat <- do.call(tab, list())})
    target_meta <- fd_metadata(tab)


    missing <- setdiff(target_meta[["field"]], colnames(target_dat))
    expect_true(length(missing) == 0, info = paste0(tab, " missing column(s): ", paste0(missing, ", ")))

    extra <- setdiff( colnames(target_dat), target_meta[["field"]])
    expect_true(length(extra) == 0, info = paste0(" extra column(s) in ", tab, ": ", paste0(extra, ", ")))

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

    suppressMessages({dat <- do.call(tab, list())})
    datclass <- tolower(sapply(dat, class))
    names(datclass) <- names(dat)

    expect_identical(sort(mdtab$field), sort(names(dat)), label = tab)


    for(f in mdtab$field){

      # extract the data class from the meta data
      meta_dc <- tolower(mdtab$class[mdtab$field == f])

      # get the class from the data
      data_class <- paste0(tolower(class(dat[[f]])), collapse = " ")

      # helper message in case an error is thrown
      msg <- paste0("data class miss match in ", tab, " column ", f)
      expect_true(grepl(x = data_class, pattern = meta_dc), info = msg)

    }


  }
})
