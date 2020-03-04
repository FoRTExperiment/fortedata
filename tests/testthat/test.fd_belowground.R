context("Loading internal package data")

test_that("Soil CO2 Efflux", {
  dat <- fd_soilCO2()

  expect_is(dat$DateTime, "POSIXlt")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
