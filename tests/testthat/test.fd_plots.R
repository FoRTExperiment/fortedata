context("Loading internal package data")

test_that("Plots table", {
  dat <- fd_plots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    # UMBS is around 45, -85. This sets a very comfortable bounding
    # box around that, which we should shrink later.
    expect_true(all(Latitude > 40))
    expect_true(all(Latitude < 50))
    expect_true(all(Longitude > -180))
    expect_true(all(Longitude < 0))

    expect_true(all(Disturbance_severity >= 0))
    expect_true(all(Disturbance_severity <= 100))

    expect_type(Plot, "integer")
    expect_type(Replicate, "character") # factor?
  })

  # Plot x Replicate should be unique
  expect_equal(nrow(unique(dat[, c("Plot", "Replicate")])), nrow(dat))
})
