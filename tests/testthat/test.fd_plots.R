context("Loading internal package data")

test_that("Plots table", {
  dat <- fd_plots()
  expect_s3_class(dat, "data.frame")
})
