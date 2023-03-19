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
#' require(r4jecs)
#' require(tidyverse)
#' require(miceRanger)
#' require(doParallel)
#'
#' set.seed(9973)
#'
#' # Load data
#' data(iris)
#'
#' # Ampute the data. iris contains no missing values by default.
#' ampIris <- amputeData(iris,perc=0.25)
#' head(ampIris,10)
#'
#' # Set up back ends.
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' # Perform mice
#' miceObjPar <- miceRanger(
#'   ampIris,
#'   m = 6,
#'   parallel = TRUE,
#'   verbose = FALSE
#' )
#' stopCluster(cl)
#' registerDoSEQ()
#'
#' imp <- completeData(miceObjPar)
#' imp <- tidyMice(imp)
#' df.imp <- imp %>% group_by(.id) %>% summarise_at(.vars = names(.)[3:ncol(imp)], .funs = 'mjvote')
#'
#' df.imp <- cbind(data.frame(ampIris[, 1]), df.imp)
#' colnames(df.imp)[1] <- "ID"
#' df.imp <- df.imp[, -2]
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
