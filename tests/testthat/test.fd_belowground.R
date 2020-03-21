context("belowground")

test_that("soil respiration", {
  dat <- fd_soil_respiration()

  expect_is(dat$Timestamp, "POSIXlt")
  expect_is(dat$soilCO2Efflux, "numeric")
  expect_is(dat$soilTemp, "numeric")
  expect_is(dat$VWC, "numeric")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
