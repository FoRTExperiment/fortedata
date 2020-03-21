context("remote sensing")

test_that("Canopy Structure", {
  dat <- fd_canopy_structure()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})

test_that("Hemispherical Imagery", {
  dat <- fd_hemi_camera()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})


test_that("ceptometer", {
  dat <- fd_par()

  expect_is(dat$Timestamp, "POSIXlt")
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
