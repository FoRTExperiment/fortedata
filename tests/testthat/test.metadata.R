context("Table columns match their metadata")

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

    expect_identical(sort(mdtab$Field), sort(names(dat)), label = tab)
    # loop for better error message?
    for(i in seq_len(nrow(mdtab))) {
      expect_equal(unname(datclass[mdtab$Field[i]]),
                   mdtab$Class[i],
                   label = paste(tab, mdtab$Field[i]))
    }
  }
})
