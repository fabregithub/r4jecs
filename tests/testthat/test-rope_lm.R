test_that("rope_lm returns plus/minus scale times the outcome SD", {
  y <- c(1, 2, 3, 4, 5)

  expect_equal(
    rope_lm(y),
    c(lower = -0.1 * stats::sd(y), upper = 0.1 * stats::sd(y))
  )
})

test_that("rope_lm handles missing values by default", {
  y <- c(1, 2, 3, NA_real_)

  expect_false(anyNA(rope_lm(y)))
  expect_equal(
    rope_lm(y),
    c(lower = -0.1 * stats::sd(y, na.rm = TRUE),
      upper = 0.1 * stats::sd(y, na.rm = TRUE))
  )
})

test_that("rope_lm can retain missing values when requested", {
  y <- c(1, 2, 3, NA_real_)

  expect_true(anyNA(rope_lm(y, na.rm = FALSE)))
})

test_that("rope_lm validates inputs", {
  expect_error(rope_lm("not numeric"), "`y` must be numeric", fixed = TRUE)
  expect_error(rope_lm(1:3, scale = -1), "`scale`", fixed = TRUE)
  expect_error(rope_lm(1:3, na.rm = NA), "`na.rm`", fixed = TRUE)
})
