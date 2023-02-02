#' Number of significant figures and number below decimal point
#'
#' Set the number of significant figures.
#'
#' This sets the number of significant figures.
#'
#' @author Shoji F. Nakayama
#'
#' @param data data
#' @param NSF Number of significant figures
#' @param NBD Number below decimal point
#'
#' @examples
#' \dontrun{
#' x <- c(0.00991, 0.00998, 0.28999998, 1.98, 0.01235, 0.0125, 10.2, 12, 11.2999, 19.9, 99.9, 109, 124, 129, 1999)
#' nsf2(x, 2)
#'
#'
#' @export
#'
#'

nsf2 <- function(data, digits) {
  z <- ifelse(data >= 10,
         formatC(signif(data, digits), digits = digits,
                 format = 'fg', flag = '#', drop0trailing = TRUE), formatC(data, digits = digits, format = 'g', flag = '#'))
  return(z)
}
