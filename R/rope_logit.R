#' ROPE range for a logistic model
#'
#' Calculate a region of practical equivalence (ROPE) on the logit scale from a
#' baseline prevalence and a tolerable change in prevalence.
#'
#' By default, `change` is interpreted as a relative change. For example,
#' `prevalence = 0.3` and `change = 0.05` use the probability range
#' `0.3 * (1 - 0.05)` to `0.3 * (1 + 0.05)`.
#'
#' @param prevalence Baseline prevalence. Must be a single number between 0 and
#'   1, exclusive.
#' @param change Tolerable change in prevalence. The default is `0.05`.
#' @param relative Logical. If `TRUE`, `change` is interpreted as a relative
#'   change. If `FALSE`, `change` is interpreted as an absolute probability
#'   difference.
#'
#' @return A named numeric vector with elements `lower` and `upper`.
#'
#' @examples
#' rope_logit(0.3, 0.05)
#' rope_logit(0.3, 0.02, relative = FALSE)
#'
#' @export
rope_logit <- function(prevalence, change = 0.05, relative = TRUE) {
  if (!is.numeric(prevalence) || length(prevalence) != 1 ||
      is.na(prevalence) || prevalence <= 0 || prevalence >= 1) {
    stop("`prevalence` must be a single number between 0 and 1, exclusive.",
         call. = FALSE)
  }

  if (!is.numeric(change) || length(change) != 1 || is.na(change) || change < 0) {
    stop("`change` must be a single non-negative number.", call. = FALSE)
  }

  if (!is.logical(relative) || length(relative) != 1 || is.na(relative)) {
    stop("`relative` must be TRUE or FALSE.", call. = FALSE)
  }

  if (relative) {
    p_low <- prevalence * (1 - change)
    p_high <- prevalence * (1 + change)
  } else {
    p_low <- prevalence - change
    p_high <- prevalence + change
  }

  if (p_low <= 0 || p_high >= 1) {
    stop("The requested change gives probabilities outside (0, 1).",
         call. = FALSE)
  }

  half_width <- (stats::qlogis(p_high) - stats::qlogis(p_low)) / 4

  c(lower = -half_width, upper = half_width)
}
