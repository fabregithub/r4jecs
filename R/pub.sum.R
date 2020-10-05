#' Generate summary statistics for publication
#'
#' This is a function to generate publication ready summary statistics
#'
#' @author Shoji F. Nakayama
#'
#' @param data data
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(10, 13, 14, 8, 9, 12), y = c('boy', 'girl', 'girl', 'girl', 'boy', 'girl'))
#' dat$y <- as.factor(dat$y)
#'
#' fname <- 'filename.csv' # Set output file name
#' if (file.exists(fname)) file.remove(fname) # Delete the file if exists
#'
#' for (i in 1:ncol(dat)) {
#'   a <- pub.sum(dat[, i])
#'   colnames(a) <- c(names(dat[i]), 'Summary')
#'   write.table(a, file = fname, sep = ',', row.names = FALSE, col.names = TRUE, append = TRUE)
#' }
#'


## Summary statistics for publication
pub.sum <- function (data) {
  if (is.numeric(data)) {
    res <- paste(round(median(data, na.rm = TRUE), 2),
                 ' (', round(quantile(data, 0.25, na.rm = TRUE)[[1]], 2), ', ',
                 round(quantile(data, 0.75, na.rm = TRUE)[[1]], 2), ')', sep = '')
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
