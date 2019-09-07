context("Loading internal package data")

test_that("Inventory table", {
  dat <- fd_inventory()

  expect_is(dat$DBH_cm, "numeric")
  expect_true(all(dat$DBH_cm > 0 | is.na(dat$DBH_cm)))

  # Health_status should be either L or D, or M
  expect_true(all(dat$Health_status %in% c("L", "D", "M")))
  # Canopy status has to be one of our four recognized values, or blank
  expect_true(all(dat$Canopy_status %in% c("OD", "UN", "OS", "SA", "")))
  # Once we standardize on an 'unknown' code, we'll add a four-letter test

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})
