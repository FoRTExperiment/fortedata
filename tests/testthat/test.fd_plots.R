context("Loading internal package data")

# UMBS is around 45, -85. This sets a very comfortable bounding
# box around that, which we should shrink later.
umbs_lon <- c(-84, -86)
umbs_lat <- c(45, 46)

test_that("Plots table", {
  dat <- fd_plots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    expect_true(all(Latitude > min(umbs_lat)))
    expect_true(all(Latitude < max(umbs_lat)))
    expect_true(all(Longitude > min(umbs_lon)))
    expect_true(all(Longitude < max(umbs_lon)))
    expect_true(all(Plot_area_m2 > 0))

    expect_type(Replicate, "character") # factor?
    expect_type(Plot, "integer")
  })

  # Replicate x Plot should be unique
  expect_equal(nrow(unique(dat[, c("Replicate", "Plot")])), nrow(dat))
})

test_that("Sublots table", {
  dat <- fd_subplots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    expect_true(all(Latitude > min(umbs_lat)))
    expect_true(all(Latitude < max(umbs_lat)))
    expect_true(all(Longitude > min(umbs_lon)))
    expect_true(all(Longitude < max(umbs_lon)))
    expect_true(all(Subplot_area_m2 > 0))

    expect_type(Replicate, "character") # factor?
    expect_type(Plot, "integer")
    expect_type(Subplot, "character") # factor?
  })

  # Replicate x Plot x Subplot should be unique
  expect_equal(nrow(unique(dat[, c("Replicate", "Plot", "Subplot")])), nrow(dat))
})

test_that("Nested sublots table", {
  dat <- fd_nested_subplots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    expect_true(all(Nested_subplot_area_m2 > 0))

    expect_type(Nested_subplot, "integer") # factor?
  })

})
