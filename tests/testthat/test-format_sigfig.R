test_that("format_sigfig returns a character vector with the same length", {
  x <- c(1.2345, NA_real_, 12.345, 0)

  out <- format_sigfig(x, sigfig = 3)

  expect_type(out, "character")
  expect_length(out, length(x))
  expect_true(is.na(out[2]))
})

test_that("format_sigfig rejects non-numeric input", {
  expect_error(format_sigfig(c("1", "2")), "`x` must be numeric", fixed = TRUE)
})

test_that("format_sigfig validates formatting arguments", {
  expect_error(format_sigfig(1.23, sigfig = 0), "sigfig", fixed = TRUE)
  expect_error(format_sigfig(1.23, sigfig = 2.5), "sigfig", fixed = TRUE)
  expect_error(format_sigfig(1.23, digits = -1), "digits", fixed = TRUE)
  expect_error(format_sigfig(1.23, nsmall = -1), "nsmall", fixed = TRUE)
})

test_that("nsf and nsf2 wrappers delegate to format_sigfig", {
  x <- c(1534, 153.4, 15.34, 1.534, 0.1534, NA_real_)

  expect_equal(
    nsf(x, NSF = 3, NBD = 2),
    format_sigfig(x, sigfig = 3, digits = 2, nsmall = 2)
  )

  expect_equal(
    nsf2(x, digits = 3),
    format_sigfig(x, sigfig = 3)
  )
})
