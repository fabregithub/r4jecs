#' Merge multiple dataframes in list with a common key into one dataframe
#'
#' This function can be feed by a list that contains multiple dataframes with a common key. It merges each dataframe in the list using the commong key into a single dataframe.
#'
#' @author Shoji F. Nakayama
#'
#' @param dfs dataframe
#' @param ... other dataframe
#'
#' @export
#'
#'

merge2 <- function(dfs, ...){
  base <- dfs[1]
  lapply(dfs[-1], function(i) base <<-  merge(base, i, ..., all.x = TRUE))
  return(base)
}
