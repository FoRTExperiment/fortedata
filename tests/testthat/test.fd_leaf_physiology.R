context("leaf physiology")

test_that("Leaf Physiology", {
  dat <- fd_leaf_spectrometry()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})

test_that("Photosynthesis", {
  dat <- fd_photosynthesis()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})
