# Random forest regression model

#' Random forest regreesion model with 10-fold cross validation and external validation
#'
#' @param data data
#' @param val data for external validation
#' @param dp dependent variable
#' @param Formula regression formula
#' @param N number of trees
#'
#' @importFrom ranger ranger
#' @importFrom doParallel detectCores
#' @importFrom doParallel makeCluster
#' @importFrom doParallel stopCluster
#' @importFrom foreach foreach
#'
#' @export
#'


## Random forest regression with 10-fold cross and external validations
rndom <- function(data, val, dp, Formula, N){
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

  #step 4: external validation
  val <- val[!is.na(val[,1])&!is.na(val[,2]),]

  pre <- predict(rf.af, val, allow.new.levels=TRUE)

  ev <- as.data.frame(cbind(val[,dp],pre$predictions))
  colnames(ev) <- c("Observed","Predicted")


  rp <- data.frame(rev(sort(rf.af$variable.importance, decreasing = T)))
  colnames(rp) <- "Importance"
  rp$Importance <- round(rp$Importance/sum(rp$Importance)*100,2)
  rp$Feature <- row.names(rp)
  rp <- tail(rp,30)

  newlist <- list(best_vn, best_mtry, rf.af, rp, cv.r1, ev)
  names(newlist) <- c("best_vn", "best_mtry", "rf.af","rp", "cv.r1", "ev")

  return(newlist)

}
