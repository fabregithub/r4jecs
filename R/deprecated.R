#' Majority voting
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and is no longer maintained.
#' Use the generic MI/brms pipeline instead:
#' <https://github.com/fabregithub/generic-mi-brms-pipeline>.
#'
#' @param ... Deprecated. Ignored.
#'
#' @export
mjvote <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.6.0",
    what = "mjvote()",
    details = paste(
      "This function is no longer maintained.",
      "Use the generic MI/brms pipeline instead:",
      "https://github.com/fabregithub/generic-mi-brms-pipeline"
    )
  )
}

#' Tidy miceRanger results
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and is no longer maintained.
#' Use the generic MI/brms pipeline instead:
#' <https://github.com/fabregithub/generic-mi-brms-pipeline>.
#'
#' @param ... Deprecated. Ignored.
#'
#' @export
tidyMice <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.6.0",
    what = "tidyMice()",
    details = paste(
      "This function is no longer maintained.",
      "Use the generic MI/brms pipeline instead:",
      "https://github.com/fabregithub/generic-mi-brms-pipeline"
    )
  )
}
