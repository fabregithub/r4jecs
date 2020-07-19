#' NADA summary
#'
#' This is a function to calculate NADA summary statistics
#'
#' @author Shoji F. Nakayama
#'
#' @param data data
#' @param cen indicator for censor
#'
#'
#' @export
#'
#'

nada.sum <- function (data, cen) {
  cf.res <- cenfit(data, cen)
  cf.summary <- censummary(data, cen)
  l.cf.res <- cenfit(log10(data), cen)
  result <- list(summary = cf.summary, mean = mean(cf.res), sd = sd(cf.res),
                 quantile = quantile(cf.res), gm = 10^mean(l.cf.res)[1],
                 gsd = 10^sd(l.cf.res))
  res.0 <- data.frame(N = result$summary$all[1], N.cencored = result$summary$all[2],
                      Percent.censored = result$summary$all[3],
                      Min = result$summary$all[4], P25 = result$quantile[3],
                      Median = result$quantile[4], P75 = result$quantile[5],
                      P95 = result$quantile[7], Max = result$summary$all[5],
                      Mean = result$mean[1], SD = result$sd,
                      GM = result$gm, GSD = result$gsd
  )
  res <- t(res.0)
  colnames(res) <- 'Summary'
  return(res)
}

