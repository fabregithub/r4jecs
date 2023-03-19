#' ROPE range calculation for a linear model
#'
#' Calculation of ROPE range for a linear model.
#'
#' This function calculates the ROPE range for a linear model.
#'
#' @author Shoji F. Nakayama
#'
#' @param y continuous y variable
#'
#' @examples
#' \dontrun{
#' rope.lm <- rope_lm(y)
#' }
#'
#' @export
#'
#'

rope_lm <- function(y) {
  res <- c((-0.1 * sd(y)), (0.1 * sd(y)))
  return(res)
}
