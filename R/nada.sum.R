#' NADA summary
#'
#' This is a function to calculate NADA summary statistics
#'
#' @author Shoji F. Nakayama
#'
#' @param data data only containing measurement data and censoring information
#'
#'
#' @export
#'
#'

nada.sum <- function (data) {
  N <- ncol(data)/2
  res <- list()
  for (i in 1:N) {
    clname <- names(data)[2 * i - 1]
    cn.cen <- paste(clname, 'cen', sep = '.')
    cf.res <- cenfit(data[, clname], data[, cn.cen])
    cf.summary <- censummary(data[, clname], data[, cn.cen])
    l.cf.res <- cenfit(log10(data[, clname]), data[, cn.cen])
    result <- list(summary = cf.summary, mean = mean(cf.res),
                   sd = sd(cf.res), quantile = quantile(cf.res), gm = 10^mean(l.cf.res)[1],
                   gsd = 10^sd(l.cf.res))
    res.0 <- data.frame(N = result$summary$all[1],
                        N.censored = result$summary$all[2],
                        Percent.censored = round(result$summary$all[3], 1),
                        Percent.detected = round(100 - result$summary$all[3], 1),
                        Min = nsf(result$summary$all[4], 3, 2),
                        P25 = nsf(result$quantile[3], 3, 2),
                        Median = nsf(result$quantile[4], 3, 2),
                        P75 = nsf(result$quantile[5], 3, 2),
                        P95 = nsf(result$quantile[7], 3, 2),
                        Max = nsf(result$summary$all[5], 3, 2),
                        Mean = nsf(result$mean[1], 3, 2),
                        SD = nsf(result$sd, 3, 2),
                        GM = nsf(result$gm, 3, 2),
                        GSD = nsf(result$gsd, 3, 2))
    res[[i]] <- t(res.0)
    colnames(res[[i]]) <- clname
  }
  result <- as.data.frame(res)
  result <- result %>% rownames_to_column(var = 'Summary')
  return(result)
}
