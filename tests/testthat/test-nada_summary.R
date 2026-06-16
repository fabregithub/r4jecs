test_that("nada_summary validates the paired-column layout", {
  expect_error(
    nada_summary(data.frame(value = c(1, 2, 3))),
    "alternating measurement and censoring columns",
    fixed = TRUE
  )
})

test_that("nada_summary validates measurement and censoring columns before fitting", {
  expect_error(
    nada_summary(data.frame(value = c("1", "2"), censored = c(FALSE, TRUE))),
    "must be numeric",
    fixed = TRUE
  )

  expect_error(
    nada_summary(data.frame(value = c(1, 2), censored = c("no", "yes"))),
    "must be logical or numeric 0/1 values",
    fixed = TRUE
  )

  expect_error(
    nada_summary(data.frame(value = c(0, 1), censored = c(FALSE, TRUE))),
    "contains values <= 0",
    fixed = TRUE
  )
})

test_that("nada_summary runs on a simple paired-column data set", {
  skip_if_not_installed("NADA")

  dat <- data.frame(
    chemical_a = c(0.4, 0.5, 0.8, 1.2, 2.1, 3.0, NA_real_, 4.2),
    chemical_a_censored = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    chemical_b = c(1.1, 1.4, 1.8, 2.0, 2.4, 3.1, 3.5, NA_real_),
    chemical_b_censored = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )

  out <- nada_summary(dat)

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("Summary", "chemical_a", "chemical_b"))
  expect_true(all(c("N", "N.censored", "Median", "Mean", "GM", "GSD") %in% out$Summary))
})

test_that("nada.sum wrapper delegates to nada_summary", {
  skip_if_not_installed("NADA")

  dat <- data.frame(
    chemical_a = c(0.4, 0.5, 0.8, 1.2, 2.1, 3.0, 4.2),
    chemical_a_censored = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_equal(nada.sum(dat), nada_summary(dat))
})
