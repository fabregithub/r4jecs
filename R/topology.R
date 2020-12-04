#' Determination of the number of nodes for SOM
#'
#' Determination of the number of nodes for SOM
#'
#' @author Shoji F. Nakayama
#'
#' @param data data
#'
#' @export
#'
#'

topology <- function(data) {
  # Determines the number of neurons (munits) and their arrangement (msize) for hexagonal lattice
  D <- data
  # munits: number of hexagons
  # dlen: number of subjects
  dlen <- dim(data)[1]
  dim <- dim(data)[2]
  munits <- ceiling(5 * dlen ^ 0.5) # Heuristic Formula matlab
  # munits = 100
  # size=c(round(sqrt(munits)),round(munits/(round(sqrt(munits)))))
  A <- matrix(Inf, nrow = dim, ncol = dim)
  for (i in 1:dim)
  {
    D[,i] <- D[,i] - mean(D[is.finite(D[,i]),i])
  }

  for (i in 1:dim){
    for (j in i:dim){
      c <- D[,i]*D[,j]
      c <- c[is.finite(c)];
      A[i,j] <- sum(c)/length(c)
      A[j,i] <- A[i,j]
    }
  }

  VS <- eigen(A)
  eigval <- sort(VS$values)

  if (eigval[length(eigval)]==0 | eigval[length(eigval)-1]*munits<eigval[length(eigval)]){
    ratio=1
  }else{
    ratio <- sqrt(eigval[length(eigval)]/eigval[length(eigval)-1])}

  size1 <- min(munits, round(sqrt(munits/ratio*sqrt(0.75))))
  size2 <- round(munits/size1)

  return(list(munits = munits, msize = sort(c(size1, size2), decreasing = TRUE)))
}
