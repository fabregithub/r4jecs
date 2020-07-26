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
#' x <- c(1534, 153.4, 15.34, 1.534, 0.1534, 0.01534, 0.001534, 0.190, 0.200)
#' nsf(x, 3, 2)
#' nsf(x, 2, 2)
#' nsf(x, 3, 3)
#' nsf(x, 1, 1)
#'
#' y <- c(5432, 543.2, 54.32, 5.432, 0.5432, 0.05432, 0.005432, NA, NA)
#' z <- data.frame(X = x, Y = y)
#' apply(z, 2, nsf, NSF = 3, NBD = 3)
#'
#' @export
#'
#'

nsf <- function(data, NSF, NBD){
  res <- round(data, digits = NBD)
  res <- signif(res, digits = NSF)
  res <- ifelse(res > 1, formatC(res, digits = NSF, flag = '#', format = 'fg'), format(res, digits = NSF, nsmall = NBD, trim = TRUE))
  res <- ifelse(grepl('\\.$', res), str_replace(res, '\\.', ''), res)
  return(res)
}
