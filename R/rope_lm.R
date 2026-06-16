#' ROPE range for a linear model
#'
#' Calculate a region of practical equivalence (ROPE) for coefficients from a
#' linear model as +/- `scale` times the standard deviation of the outcome.
#'
#' @param y Numeric outcome vector.
#' @param scale Width multiplier. The default, `0.1`, gives +/- 0.1 SD.
#' @param na.rm Logical. Should missing values be removed before calculating the
#'   standard deviation?
#'
#' @return A named numeric vector with elements `lower` and `upper`.
#'
#' @examples
#' y <- c(1, 2, 3, 4, 5)
#' rope_lm(y)
#'
#' @export
rope_lm <- function(y, scale = 0.1, na.rm = TRUE) {
  if (!is.numeric(y)) {
    stop("`y` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(scale) || length(scale) != 1 || is.na(scale) || scale < 0) {
    stop("`scale` must be a single non-negative number.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1 || is.na(na.rm)) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  s <- stats::sd(y, na.rm = na.rm)

  c(lower = -scale * s, upper = scale * s)
}
