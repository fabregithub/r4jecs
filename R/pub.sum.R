#' Generate summary statistics for publication
#' Generating summary statistics for publication
#'
#' @author Shoji F. Nakayama
#'
#' @param data data
#' @export
#'


## Summary statistics for publication
pub.sum <- function (data) {
  if (is.numeric(data)) {
    res <- paste(round(median(data), 2), ' (', round(quantile(data, 0.25)[[1]], 2), ', ', round(quantile(data, 0.75)[[1]], 2), ')', sep = '')
    res <- data.frame(Summary = res)
    res <- rownames_to_column(res)
    return(res)
  }
  else
    if (is.factor(data)) {
      res.1 <- table(data)
      res.2 <- round(table(data) * 100 / length(data), 1)
      res <- paste(res.1, ' (', res.2, ')', sep = '')
      res <- data.frame(Summary = res)
      res <- rownames_to_column(res)
      return(res)
    }
  else print('The variable is neither numeric nor factor.')
}



# dat.15 <- subset(mi_pm2, Year == 1.5)
# dat.30 <- subset(mi_pm2, Year == 3)
#
# # Delete csv file if it exists
# fn15 <- 'temp15.csv'
# if (file.exists(fn15)) file.remove(fn15)
# # Calculate 1.5 year summary for publication
# for (i in 1:length(dat.15)) {
#   a <- pub.summary(dat.15[, i])
#   colnames(a) <- c(names(dat.15[i]), 'Summary')
#   write.table(a, file = fn15, sep = ',', row.names = FALSE, col.names = TRUE, append = TRUE)
# }
#
# # Delete csv file if it exists
# fn30 <- 'temp30.csv'
# if (file.exists(fn30)) file.remove(fn30)
# # Calculate 3 year summary for publication
# for (i in 1:length(dat.30)) {
#   b <- pub.summary(dat.30[, i])
#   colnames(b) <- c(names(dat.30[i]), 'Summary')
#   write.table(b, file = fn30, sep = ',', row.names = FALSE, col.names = TRUE, append = TRUE)
# }
#
# d15 <- read.csv('temp15.csv')
# d30 <- read.csv('temp30.csv')
#
# pub.sum <- cbind(d15, d30[,2])
# write.csv(pub.sum, 'publication-summary2.csv', row.names = FALSE)
