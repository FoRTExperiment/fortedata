context("belowground")

test_that("soil respiration", {
  dat <- fd_soil_respiration()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})
