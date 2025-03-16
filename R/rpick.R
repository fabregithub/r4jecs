#' Randomly pick values from fitted distribution
#'
#' Random picking of values from fitted distribution.
#'
#' This function picks a value randomly from a fitted distribution.
#'
#' @author Shoji F. Nakayama
#'
#' @param N Number of iteration
#' @param data data
#' @param model fitted model
#'
#' @examples
#' \dontrun{
#' for (i in 1:nrow(df)) {
#'   df$column[i] <- rpick(N, df$column[i], model)
#' }}
#'
#' @export
#'
#'

rpick <- function(N,data,model) {
  z <- rnorm(n = N, mean = model$estimate[1], sd = model$estimate[2])
  z <- z[z < data]
  z <- sample(z, 1)
  return(z)
}
