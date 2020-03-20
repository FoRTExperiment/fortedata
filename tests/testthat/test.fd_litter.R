context("Loading internal package data")

test_that("Litter Trap Data", {
  dat <- fd_lai()

  expect_is(dat$Year, "integer")

  # All the replicate/plot/subplot codes should exist
  subplots <- fd_subplots()
  expect_true(all(dat$Replicate %in% subplots$Replicate))
  expect_true(all(dat$Plot %in% subplots$Plot))
  expect_true(all(dat$Subplot %in% subplots$Subplot))
})

