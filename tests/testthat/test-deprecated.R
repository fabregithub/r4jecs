test_that("deprecated functions stop with lifecycle deprecation errors", {
  skip_if_not_installed("lifecycle")

  expect_error(mjvote(), class = "lifecycle_error_deprecated")
  expect_error(tidyMice(), class = "lifecycle_error_deprecated")
})
