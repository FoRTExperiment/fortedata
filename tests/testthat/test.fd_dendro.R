context("dendro")

test_that("fd_subcanopy_diameter", {

  dat <- fd_subcanopy_diameter()

  # Subplot_id should have four characters
  expect_true(all(nchar(dat$subplot_id) == 4))

  # All the replicate/plot/subplot codes should exist
  dat <- split_subplot_id(dat)
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})

test_that("fd_subcanopy_density", {

  dat <- fd_subcanopy_density()

  # Subplot_id should have four characters
  expect_true(all(nchar(dat$subplot_id) == 4))

  # All the replicate/plot/subplot codes should exist
  dat <- split_subplot_id(dat)
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})

test_that("fd_seedling_sapling", {

  dat <- fd_seedling_sapling()

  # Subplot_id should have four characters
  expect_true(all(nchar(dat$subplot_id) == 4))

  # All the replicate/plot/subplot codes should exist
  dat <- split_subplot_id(dat)
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})
