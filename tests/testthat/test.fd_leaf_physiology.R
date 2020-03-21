context("leaf physiology")

test_that("Leaf Physiology", {
  dat <- fd_leaf_spectrometry()

  expect_is(dat$Index_Value, "numeric")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})

test_that("Photosynthesis", {
  dat <- fd_photosynthesis()

  expect_is(dat$Timestamp, "POSIXlt")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
