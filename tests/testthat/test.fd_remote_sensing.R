context("remote sensing")

test_that("Canopy structure", {
  dat <- fd_canopy_structure()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})

test_that("Hemispherical imagery", {
  dat <- fd_hemi_camera()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})

test_that("Ceptometer", {
  dat <- fd_ceptometer()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})
