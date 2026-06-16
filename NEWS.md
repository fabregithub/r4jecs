# r4jecs 0.6.0

## Package modernization

* Modernized the package around a smaller supported API:
  * `nada_summary()`
  * `format_sigfig()`
  * `rope_lm()`
  * `rope_logit()`
* Added backward-compatible wrappers for old function names:
  * `nada.sum()`
  * `nsf()`
  * `nsf2()`
* Deprecated retired workflow helpers with `lifecycle::deprecate_stop()`:
  * `mjvote()`
  * `tidyMice()`
* Removed the planned `roperange()` / `rope_range()` API because it is no longer needed.

## Function changes

* Added `nada_summary()` as the modern replacement for `nada.sum()`.
* Fixed NA handling in censored-data summaries by removing rows only when either the value or censoring indicator is missing, rather than removing missing values from each vector independently.
* Added validation for paired value/censoring columns in `nada_summary()`.
* Added `format_sigfig()` as the shared formatting helper for significant figures.
* Reimplemented `nsf()` and `nsf2()` as wrappers around `format_sigfig()`.
* Updated `rope_lm()` to:
  * return named lower and upper bounds,
  * support configurable ROPE scaling with `scale`,
  * handle missing values by default with `na.rm = TRUE`,
  * validate numeric inputs.
* Updated `rope_logit()` to:
  * use `stats::qlogis()` instead of `gtools::logit()`,
  * validate prevalence and probability bounds,
  * support either relative or absolute probability changes.

## Package infrastructure

* Simplified package-level roxygen documentation in `R/r4jecs-package.R`.
* Removed stale imports from package-level documentation so `roxygen2` can regenerate a cleaner `NAMESPACE`.
* Added tests for the modernized API and deprecated functions.
* Updated the README to describe current usage and migration from old function names.
* Removed obsolete implementation files for retired functions from the recommended package layout.

# r4jecs 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release of the r4jecs package.
