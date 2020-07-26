#' Random forest regression model 2
#'
#' The function conducts random forest with the recursive feature elimination algorithm to fit the data and perform 10-fold cross-validation.
#'
#' The random forest algorithm is a collection of classification and regression trees that grow from each bootstrap dataset drew randomly from the original data.
#' In each tree, variables are randomly selected for splitting at each node and the best split among those variables are chosen.
#' The algorithm aggregates predictions from trees by majority voting for classification or averaging for regression, and calculate an out-of-bag (OOB, the data not drew in the bootstrap samples) error rate (Liaw and Wiener, 2002).
#'
#' The rndom2 function combines the random forest (a fast Implementation of random forests for high dimensional data by ranger package, Wright and Ziegler, 2017) with the recursive feature elimination (RFE) algorithm to deal with the collinearity problem and remove less relevant predictors (Gregorutti et al., 2013).
#' The RFE algorithm consisted of: (1) training the random forest, and (2) calculating the permutation importance scores of variables and coefficient of determination (\eqn{R^2}{R2}) by OOB data, and (3) removing the less important variable.
#' Step (1) to (3) were repeated until no further variable remained.
#' The random forest with the RFE algorithm is first trained by including all predictors and set the number of predictors that randomly sampled for splitting at each node (mtry) as one third of the total number of predictors.
#' The RFE algorithm iteratively removes predictors and calculates corresponding OOB \eqn{R^2}{R2}.
#' The appropriate set of predictors was determined based on the highest OOB \eqn{R^2}{R2}.
#' Once the set of predictors is determined, the mtry will be tuned from 1 to the total number of predictors and the mtry with the highest OOB \eqn{R^2}{R2} will be selected as the final model.
#' Lastly the rndom2 function validates the model performance by using 10-fold cross validation, outputs a dataset combines observed values and predicted values.
#'
#' @author Jung Chau-Ren
#'
#' @param data Training data
#' @param dp Dependent variable
#' @param Formula Defining the model to fit
#' @param N Number of trees
#'
#' @return The function returns a list includes
#'
#' \item{\code{$best_vn}}{Selected variable through the recursive feature elimination algorithm}
#' \item{\code{$best_mtry}}{mtry with the highest OOB \eqn{R^2}{R2}}
#' \item{\code{$rf.af}}{Saved random forest model}
#' \item{\code{$rp}}{Relative importance (%) derived from permutation importance}
#' \item{\code{$cv.r1}}{Dataset combined observed values and predicted values derived from the 10-fold cross-validation.}
#'
#' @references
#' Liaw, A., Wiener, M., 2002. Classification and Regression by randomForest. R News 2/3, 18–22.
#'
#' Wright, M.N., Ziegler, A., 2017. ranger: A fast implementation of random forest for high dimensional data in C++ and R. Journal of Statistical Software 77, 1–17.
#'
#' Gregorutti, B., Michel, B., Saint-Pierre, P., 2013. Correlation and variable importance in random forests. Statistics and Computing 27, 1–31. DOI: 10.1007/s11222-016-9646-1
#'
#' @export
#'

## Random forest regression with 10-fold cross validation
rndom2 <- function(data, dp, Formula, N){
  data <- data[!is.na(data[,1])&!is.na(data[,2]),]
  entry_data <- data
  n.r <- matrix(ncol=4,nrow=0)

  #feature selection
  #step 1: determine the number of variable, recursively feature elimination
  repeat {
    if(length(entry_data)<3) break
    set.seed(123)
    rf <-  ranger(Formula, data=entry_data, mtry=ceiling((length(entry_data))/3), num.trees=N, importance="permutation")
    #drop the least important variable
    vn <- names(sort(rf$variable.importance)[2:length(rf$variable.importance)])
    entry_data <- entry_data[,c(dp,vn)]
    set.seed(123)
    rf_test <-  ranger(Formula, data=entry_data, mtry=ceiling((length(entry_data))/3), num.trees=N, importance="permutation")
    r <- matrix(NA, ncol=4, nrow=1)
    r[,1] <- length(entry_data)
    r[,2] <- ceiling(length(entry_data)/3)
    r[,3] <- rf_test$r.squared
    r[,4] <- list(names(rf_test$variable.importance))
    n.r <- rbind(n.r,r)
  }

  best_vn <- unlist(n.r[,4][which(unlist(n.r[,3], use.names=F)==max(unlist(n.r[,3], use.names=F)))], use.names = FALSE)

  #step 2: determine mtry
  test_data <- data[,c(dp,best_vn)]

  CPU <- detectCores() / 2

  myCluster <- makeCluster(CPU, type="FORK")
  registerDoParallel(myCluster)

  mtry_b <-  foreach(i = 1:(length(test_data)-1), .packages=c("ranger"),  .combine=rbind) %dopar% {
    set.seed(123)
    rf_test <-  ranger(Formula, data=test_data, mtry=i, num.trees=N, importance="permutation")
    r <- matrix(NA, ncol=2, nrow=1)
    r[,1] <- i
    r[,2] <- rf_test$r.squared
    r
  }
  stopCluster(myCluster)

  colnames(mtry_b) <- c("mtry","R^2")
  best_mtry <- mtry_b[,1][mtry_b[,2]==max(mtry_b[,2])]

  #step 3: ten fold cross validation
  #randomly shuffle the data
  data.f <- data[,c(dp,best_vn)]
  set.seed(123)
  rf.af <-  ranger(Formula, data=data.f, mtry=best_mtry, num.trees=N, importance="permutation")

  set.seed(123)
  data.f <- data.f[sample(nrow(data.f)),]

  folds <- cut(seq(1,nrow(data.f)),breaks=10, labels=FALSE)
  for (i in 1:10){
    testIndexs <- which(folds==i, arr.ind=TRUE)
    test <- data.f[testIndexs,]
    train <- data.f[-testIndexs,]
    set.seed(123)
    cv <-  ranger(Formula, data=train, mtry=best_mtry, num.trees=N, importance="permutation")
    pred <- predict(cv,test, allow.new.levels=TRUE)
    cv.r <- as.data.frame(cbind(test[,dp],pred$predictions))
    if (i==1){
      cv.r1 <- cv.r
    }
    else {
      cv.r1 <- rbind(cv.r1, cv.r)
    }
  }
  colnames(cv.r1) <- c("Observed","Predicted")

  rp <- data.frame(rev(sort(rf.af$variable.importance, decreasing = T)))
  colnames(rp) <- "Importance"
  rp$Importance <- round(rp$Importance/sum(rp$Importance)*100,2)
  rp$Feature <- row.names(rp)
  rp <- tail(rp,30)

  newlist <- list(best_vn, best_mtry, rf.af, rp, cv.r1)
  names(newlist) <- c("best_vn", "best_mtry", "rf.af","rp", "cv.r1")

  return(newlist)

}
