context("belowground")

test_that("soil respiration", {
  dat <- fd_soil_respiration()

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
