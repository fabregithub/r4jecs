#' Format numeric values using significant figures
#'
#' `format_sigfig()` formats numeric vectors with a target number of
#' significant figures. When `digits` is supplied, values are first rounded
#' using half-up rounding to that number of decimal places.
#'
#' `nsf()` is kept as a backward-compatible wrapper around
#' `format_sigfig()`.
#'
#' @param x Numeric vector.
#' @param sigfig Number of significant figures.
#' @param digits Optional number of decimal places used before formatting.
#' @param nsmall Minimum number of digits to the right of the decimal point for
#'   values with absolute value less than 1. This is mainly used to preserve the
#'   historical behaviour of `nsf()`.
#' @param na Character value used for missing values.
#'
#' @return A character vector with the same length as `x`.
#'
#' @examples
#' x <- c(1534, 153.4, 15.34, 1.534, 0.1534, 0.01534, 0.001534, NA)
#' format_sigfig(x, sigfig = 3, digits = 2)
#' nsf(x, 3, 2)
#'
#' @export
format_sigfig <- function(x, sigfig = 3, digits = NULL, nsmall = 0,
                          na = NA_character_) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  .check_whole_number(sigfig, "sigfig", lower = 1)
  .check_whole_number(nsmall, "nsmall", lower = 0)

  if (!is.null(digits)) {
    .check_whole_number(digits, "digits", lower = 0)
  }

  out <- rep(na, length(x))
  ok <- !is.na(x)

  if (!any(ok)) {
    return(out)
  }

  y <- x[ok]

  if (!is.null(digits)) {
    y <- .round_half_up(y, digits = digits)
  }

  y <- signif(y, digits = sigfig)

  out[ok] <- vapply(
    y,
    .format_one_sigfig,
    character(1),
    sigfig = sigfig,
    nsmall = nsmall,
    USE.NAMES = FALSE
  )

  out
}

#' @rdname format_sigfig
#' @param data Numeric vector.
#' @param NSF Number of significant figures.
#' @param NBD Number of decimal places used before formatting.
#'
#' @export
nsf <- function(data, NSF, NBD) {
  format_sigfig(data, sigfig = NSF, digits = NBD, nsmall = NBD)
}

.round_half_up <- function(x, digits = 0) {
  shift <- 10^digits
  floor(x * shift + 0.5) / shift
}

.format_one_sigfig <- function(x, sigfig, nsmall) {
  if (x == 0) {
    return("0")
  }

  if (abs(x) >= 1) {
    return(sub(
      "\\.$",
      "",
      formatC(x, digits = sigfig, flag = "#", format = "fg")
    ))
  }

  format(x, digits = sigfig, nsmall = nsmall, trim = TRUE, scientific = FALSE)
}

.check_whole_number <- function(x, arg, lower = NULL) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x != floor(x)) {
    stop("`", arg, "` must be a single whole number.", call. = FALSE)
  }

  if (!is.null(lower) && x < lower) {
    stop("`", arg, "` must be greater than or equal to ", lower, ".",
         call. = FALSE)
  }

  invisible(x)
}
#' Format numeric values using significant figures only
#'
#' `nsf2()` is kept as a backward-compatible wrapper around
#' [format_sigfig()]. Prefer [format_sigfig()] in new code.
#'
#' @param data Numeric vector.
#' @param digits Number of significant figures.
#'
#' @return A character vector with the same length as `data`.
#'
#' @examples
#' x <- c(0.00991, 0.00998, 0.28999998, 1.98, 0.01235, 10.2, 1999)
#' nsf2(x, 2)
#'
#' @export
nsf2 <- function(data, digits) {
  format_sigfig(data, sigfig = digits)
}
