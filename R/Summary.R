# Calculation of cummary statistics
#' Calculating summary statistics
#'
#' @param data data
#'
#' @importFrom psych describe
#' @export
#'


Summary <- function (data) {
  des <- describe(data)
  l <- log10(x)
  gm <- 10^mean(l)
  gsd <- 10^sd(l)
  per <- quantile(x, c(0.25, 0.75, 0.95))
  res.0 <- data.frame(N = des[2], Min = des[8], P25 = per[1], Median = des[5], P75 = per[2],
                      P95 = per[3], Max = des[9], Mean = des[3], SD = des[4],
                      GM = gm, GSD = gsd
  )
  res <- t(res.0)
  colnames(res) <- 'Summary'
  return(res)
}

