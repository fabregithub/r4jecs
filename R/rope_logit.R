#' ROPE range calculation for a logistic model
#'
#' Calculation of ROPE range for a logistic model.
#'
#' This function calculates the ROPE range for a logistic model.
#'
#' @author Shoji F. Nakayama
#'
#' @param prevalence prevalence of binomial y variance = 1
#' @param change tolerable range
#'
#' @examples
#' \dontrun{
#' rope.logit <- rope_llogit(0.3, 0.05)
#' }
#'
#' @export
#'
#'

rope_logit <- function(prevalence, change = 0.05) {
  res <- c(-(gtools::logit(prevalence + prevalence * change)-gtools::logit(prevalence - prevalence * change))/4,(gtools::logit(prevalence + prevalence * change)-gtools::logit(prevalence - prevalence * change))/4)
  return(res)
}
