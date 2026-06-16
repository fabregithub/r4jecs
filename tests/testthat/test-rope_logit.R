test_that("rope_logit returns a symmetric named interval", {
  out <- rope_logit(0.2)

  expect_named(out, c("lower", "upper"))
  expect_equal(unname(out[1]), -unname(out[2]))
})

test_that("rope_logit matches the expected relative-change calculation", {
  prevalence <- 0.3
  change <- 0.05
  p_low <- prevalence * (1 - change)
  p_high <- prevalence * (1 + change)
  half_width <- (stats::qlogis(p_high) - stats::qlogis(p_low)) / 4

  expect_equal(
    rope_logit(prevalence, change = change),
    c(lower = -half_width, upper = half_width)
  )
})

test_that("rope_logit supports absolute probability changes", {
  prevalence <- 0.3
  change <- 0.02
  p_low <- prevalence - change
  p_high <- prevalence + change
  half_width <- (stats::qlogis(p_high) - stats::qlogis(p_low)) / 4

  expect_equal(
    rope_logit(prevalence, change = change, relative = FALSE),
    c(lower = -half_width, upper = half_width)
  )
})

test_that("rope_logit validates probabilities and arguments", {
  expect_error(rope_logit(0), "`prevalence`", fixed = TRUE)
  expect_error(rope_logit(1), "`prevalence`", fixed = TRUE)
  expect_error(rope_logit(0.99, change = 0.05), "outside", fixed = TRUE)
  expect_error(rope_logit(0.3, change = -0.01), "`change`", fixed = TRUE)
  expect_error(rope_logit(0.3, relative = NA), "`relative`", fixed = TRUE)
})
