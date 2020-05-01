context("inventory")

test_that("Inventory table", {
  dat <- fd_inventory()

  expect_true(all(dat$dbh_cm > 0 | is.na(dat$dbh_cm)))

  # Health_status should be either L or D, or M
  expect_true(all(dat$health_status %in% c("L", "D", "M")))
  # Canopy status has to be one of our four recognized values, or blank
  expect_true(all(dat$canopy_status %in% c("OD", "UN", "OS", "SA", "")))
  # Once we standardize on an 'unknown' code, we'll add a four-letter test

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$replicate %in% subplots$replicate))
  expect_true(all(dat$plot %in% subplots$plot))
  expect_true(all(dat$subplot %in% subplots$subplot))
})
