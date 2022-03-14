context("release")

test_that("fd_make_release", {
  # Handles bad input
  expect_error(fd_make_release(1))
  expect_error(fd_make_release("", 1))

  td <- tempdir()
  td <- file.path(td, "test_release_dir")
  expect_error(fd_make_release(td), regexp = "Output directory doesn't exist")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  fd_make_release(td, zip_release = FALSE)
})
