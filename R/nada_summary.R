#' Summary statistics for left-censored data using NADA
#'
#' `nada_summary()` summarises alternating measurement and censoring columns
#' using `NADA::cenfit()` and `NADA::censummary()`.
#'
#' The expected data layout is the historical `r4jecs` layout:
#'
#' measurement 1, censoring indicator 1, measurement 2, censoring indicator 2,
#' and so on.
#'
#' `nada.sum()` is kept as a backward-compatible wrapper around
#' `nada_summary()`.
#'
#' @param data A data frame containing alternating measurement columns and
#'   censoring indicator columns. Censoring indicators may be logical or numeric
#'   0/1 values, where `TRUE` or `1` means censored.
#' @param sigfig Number of significant figures used for formatted statistics.
#' @param digits Number of decimal places used before formatting statistics.
#'
#' @return A data frame with one row per summary statistic and one column per
#'   measurement variable.
#'
#' @examples
#' \dontrun{
#' nada_summary(dat)
#' nada.sum(dat)
#' }
#'
#' @export
nada_summary <- function(data, sigfig = 3, digits = 2) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  if (ncol(data) == 0 || ncol(data) %% 2 != 0) {
    stop("`data` must contain alternating measurement and censoring columns.",
         call. = FALSE)
  }

  .check_whole_number(sigfig, "sigfig", lower = 1)
  .check_whole_number(digits, "digits", lower = 0)

  summary_rows <- c(
    "N",
    "N.censored",
    "Percent.censored",
    "Percent.detected",
    "Min",
    "P25",
    "Median",
    "P75",
    "P95",
    "Max",
    "Mean",
    "SD",
    "GM",
    "GSD"
  )

  out <- data.frame(Summary = summary_rows, stringsAsFactors = FALSE,
                    check.names = FALSE)

  n_pairs <- ncol(data) / 2

  for (i in seq_len(n_pairs)) {
    value_name <- names(data)[2 * i - 1]
    value <- data[[2 * i - 1]]
    censored <- data[[2 * i]]

    if (!is.numeric(value)) {
      stop("Measurement column `", value_name, "` must be numeric.",
           call. = FALSE)
    }

    censored <- .normalise_censoring(censored, names(data)[2 * i])

    ok <- stats::complete.cases(value, censored)
    value <- value[ok]
    censored <- censored[ok]

    if (length(value) == 0) {
      stop("Measurement column `", value_name, "` has no complete observations.",
           call. = FALSE)
    }

    if (any(value <= 0)) {
      stop("Measurement column `", value_name,
           "` contains values <= 0; GM and GSD require positive values.",
           call. = FALSE)
    }

    cf <- .nada_cenfit(value, censored)
    cf_summary <- NADA::censummary(value, censored)
    log_cf <- .nada_cenfit(log10(value), censored)

    cf_quantile <- .nada_method(
      "quantile",
      "cenfit",
      cf,
      probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
    )
    cf_mean <- .nada_method("mean", "cenfit", cf)
    cf_sd <- .nada_method("sd", "cenfit", cf)
    log_mean <- .nada_method("mean", "cenfit", log_cf)
    log_sd <- .nada_method("sd", "cenfit", log_cf)

    out[[value_name]] <- c(
      cf_summary$all[1],
      cf_summary$all[2],
      round(cf_summary$all[3], 1),
      round(100 - cf_summary$all[3], 1),
      format_sigfig(cf_summary$all[4], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_quantile[3], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_quantile[4], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_quantile[5], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_quantile[7], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_summary$all[5], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_mean[1], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(cf_sd, sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(10^log_mean[1], sigfig = sigfig, digits = digits,
                    nsmall = digits),
      format_sigfig(10^log_sd, sigfig = sigfig, digits = digits,
                    nsmall = digits)
    )
  }

  out
}

#' @rdname nada_summary
#' @export
nada.sum <- function(data, sigfig = 3, digits = 2) {
  nada_summary(data, sigfig = sigfig, digits = digits)
}

.normalise_censoring <- function(x, column_name) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    values <- unique(stats::na.omit(x))

    if (all(values %in% c(0, 1))) {
      return(as.logical(x))
    }
  }

  stop("Censoring column `", column_name,
       "` must be logical or numeric 0/1 values.", call. = FALSE)
}

.nada_cenfit <- function(value, censored) {
  nada_env <- new.env(parent = asNamespace("NADA"))
  nada_env$value <- value
  nada_env$censored <- censored

  formula <- stats::as.formula("Cen(value, censored) ~ 1", env = nada_env)

  NADA::cenfit(formula)
}

.nada_method <- function(generic, signature, object, ...) {
  method <- methods::selectMethod(generic, signature, optional = TRUE)

  if (is.null(method)) {
    stop("Could not find NADA method `", generic, "` for signature `",
         signature, "`.", call. = FALSE)
  }

  method(object, ...)
}
