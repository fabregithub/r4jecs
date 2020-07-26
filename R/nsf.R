#' Number of significant figures and number below decimal point
#'
#' Take care of the number of significant figures (NSF) and number below decimal point with the following criteria (NBD).
#'
#' This sets the number of significant figures as NSF and number below decimal point as NBD. It removes trailing '.' and '0.'
#'
#' @author Shoji F. Nakayama
#'
#' @param data data
#' @param NSF Number of significant figures
#' @param NBD Number below decimal point
#'
#' @examples
#' x <- c(1534, 153.4, 15.34, 1.534, 0.1534, 0.01534, 0.001534)
#' nsf(x, 3, 2)
#' nsf(x, 2, 2)
#' nsf(x, 3, 3)
#' nsf(x, 1, 1)
#'
#' @export
#'
#'

nsf <- function(data, NSF, NBD){
  x <- round(x, digits = NBD)
  x <- signif(x, digits = NSF)
  x <- ifelse(x > 1, formatC(x, digits = NSF, flag = '#', format = 'fg'), x)
  x <- ifelse(grepl('\\.$', x), str_replace(x, '\\.', ''), x)
  return(x)
}
