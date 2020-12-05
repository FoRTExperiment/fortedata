context("plots")

# UMBS is around 45, -85. This sets a very comfortable bounding
# box around that, which we should shrink later.
umbs_lon <- c(-84, -86)
umbs_lat <- c(45, 46)

test_that("Plots table", {
  dat <- fd_plots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    expect_true(all(latitude_plot > min(umbs_lat)))
    expect_true(all(latitude_plot < max(umbs_lat)))
    expect_true(all(longitude_plot > min(umbs_lon)))
    expect_true(all(longitude_plot < max(umbs_lon)))
    expect_true(all(plot_area_m2 > 0))
  })

  # Replicate x Plot should be unique
  expect_equal(nrow(unique(dat[, c("replicate", "plot")])), nrow(dat))
})

test_that("Sublots table", {
  dat <- fd_subplots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    expect_true(all(latitude_subplot > min(umbs_lat)))
    expect_true(all(latitude_subplot < max(umbs_lat)))
    expect_true(all(longitude_subplot > min(umbs_lon)))
    expect_true(all(longitude_subplot < max(umbs_lon)))
    expect_true(all(subplot_area_m2 > 0))
  })

  # Replicate x Plot x Subplot should be unique
  expect_equal(nrow(unique(dat[, c("replicate", "plot", "subplot")])), nrow(dat))
})

test_that("Nested sublots table", {
  dat <- fd_nested_subplots()
  expect_s3_class(dat, "data.frame")

  with(dat, {
    expect_true(all(nested_subplot_area_m2 > 0))
  })

  # Replicate x Plot x Subplot x Nested_subplot should be unique
  expect_equal(nrow(unique(dat[, c("replicate", "plot", "subplot", "nested_subplot")])), nrow(dat))
})
