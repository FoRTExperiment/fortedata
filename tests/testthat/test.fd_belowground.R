context("Loading internal package data")

test_that("Soil CO2 Efflux", {
  dat <- fd_soilCO2()

  expect_is(dat$DateTime, "POSIXlt")
  expect_is(dat$soilCO2Efflux, "numeric")
  expect_is(dat$soilTemp, "numeric")
  expect_is(dat$VWC, "numeric")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
