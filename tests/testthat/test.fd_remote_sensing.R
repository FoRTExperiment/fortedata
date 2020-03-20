context("Loading internal package data")

test_that("Canopy Structure", {
  dat <- fd_canopy_structure()

  expect_is(dat$Year, "integer")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})

test_that("Hemispherical Imagery", {
  dat <- fd_hemi_camera()

  expect_is(dat$Year, "integer")
  expect_is(dat$NDVI, "numeric")
  expect_is(dat$GapFraction, "numeric")
  expect_is(dat$Openness, "numeric")
  expect_is(dat$ClumpingIndex, "numeric")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})


test_that("ceptometer", {
  dat <- fd_par()

  expect_is(dat$Year, "integer")
  expect_is(dat$DateTime, "POSIXlt")
  expect_is(dat$aPAR, "numeric")
  expect_is(dat$bPAR, "numeric")
  expect_is(dat$faPAR, "numeric")
  expect_is(dat$LAI_cept, "numeric")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
