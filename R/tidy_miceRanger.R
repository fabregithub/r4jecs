#' Tidy miceRanger results
#'
#' The function for tidy miceRanger imputation results
#'
#' This function is used to tidy miceRanger imputation results
#'
#' @author Shoji F. Nakayama
#'
#' @param X data
#'
#' @examples
#' \dontrun{
#' library(miceRange)
#' n.core <- detectCores()
#' cl <- makeCluster(n.core / 2)
#' registerDoParallel(cl)
#'
#' df <- readRDS("data.rds")
#' set.seed(12345)
#' mice.res <- miceRanger(
#'   df,
#'   m = 17,
#'   maxiter = 10,
#'   parallel = TRUE,
#'   returnModels = TRUE,
#'   verbose = FALSE
#'   )
#'
#' stopCluster(cl)
#' registerDoSEQ()
#'
#' imp <- completeData(mice.res)
#'
#' df2 <- tidyMice(imp)
#' df3 <- df2 %>% group_by(.id) %>% summarise_at(.vars = names(.)[3:ncols(df2)], .funs = 'getmode')
#' df3 <- cbind(data.frame(df[, 1]), df3)
#' colnames(df3)[1] <- "ID"
#' df3 <- df3[, -2]
#' }
#'
#' @export
#'
#'

tidyMice <- function(X) {
  for (i in 1:length(X)) {
    X[[i]] <- X[[i]] %>% add_column(.before = 1, .imp = i)
    X[[i]] <- X[[i]] %>% add_column(.after = 1, .id = 1:nrow(X[[i]]))
  }
  Z <- X %>% rbindlist()
  return(Z)
}
