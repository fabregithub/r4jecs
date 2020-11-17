#' Generate summary statistics for publication
#'
#' This is a function to generate publication ready summary statistics
#'
#' @author Shoji F. Nakayama
#'
#' @param data data only containing measurement data and censoring information
#' @param useNA optional argument to determine whether NA is counted in the table function
#'
#' @export
#'


## Summary statistics for publication
pub.sum <- function (data, useNA = 'ifany') {
  result <- list()
  N <- ncol(data)
  for(i in 1:N) {
    if (is.numeric(data[, i])) {
      res <- paste(round(median(data[, i], na.rm = TRUE), 2),
                   ' (', round(quantile(data[, i], 0.25, na.rm = TRUE)[[1]], 2), ', ',
                   round(quantile(data[, i], 0.75, na.rm = TRUE)[[1]], 2), ')', sep = '')
      res <- data.frame(res)
      res <- rownames_to_column(res)
      colnames(res) <- c('Variable', 'Median (95% CI)')
      res[1, 1] <- names(data)[i]
      result[[i]] <- res
    }
    else
      if (is.factor(data[, i])) {
        res.1 <- table(data[, i], useNA = useNA)
        res.2 <- round(table(data[, i], useNA = useNA) * 100 / sum(table(data[, i], useNA = useNA)), 1)
        res <- paste(res.1, ' (', res.2, ')', sep = '')
        res <- data.frame(res)
        res <- rownames_to_column(res)
        colnames(res) <- c(names(data)[i], 'Count (%)')
        res[, 1] <- names(res.1)
        result[[i]] <- res
      }
    else
      result[[i]] <- 'The variable is neither numeric nor factor.'
  }
  return(result)
}
