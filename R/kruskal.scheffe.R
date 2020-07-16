# Extended Kruskal-Wallis test followed by Scheffe's test
#' Extended Kruskal-Wallis test followed by Scheffe's test
#'
#' @param data data
#' @param group group
#'
#' @export
#'

kruskal.scheffe <-
function(   data,
            group=NULL)                                     # Group vector (ignored if data is list)
{
    if (is.list(data)) {                                    # If data is list
                group <- factor(rep(1:length(data), sapply(data, length)))      # Make a group vector
                data <- unlist(data)                        # Make list as a vector
        }
#        DNAME <- paste(deparse(substitute(data), "and",
#                       deparse(substitute(group)))          # Data name
        OK <- complete.cases(data, group)                   # Remove a case with NA
        data <- data[OK]
        group <- group[OK]
        ni<- table(group)                                   # Number of data in each group
        n <- sum(ni)                                        # Number of all data
        r <- rank(data)                                     # Ranking
        Ri <- tapply(r, group, sum)                         # Sum in each group
        S <- 12*sum(Ri^2/ni)/(n*(n+1))-3*(n+1)              # Estimated statistics
        if (length(unique(data)) != n) {                    # If there are ties
                tie <- table(data)
                S <- S/(1-sum(tie^3-tie)/(n^3-n))           # Estimates with ties
        }
        a <- length(ni)                                     # Number of groups
        df <- a-1                                           # Demension of freedum
        p <- pchisq(S, df, lower.tail=FALSE)                # P Value
        names(S) <- "Kruskal-Wallis chi-squared"
        names(df) <- "df"
        names(p) <- "P value"
		kruskal <- c(S, df, p)
        a.mean <- (n+1)/2                                   # Mean rank
        R.mean <- Ri/ni                                     # Mean rank sum
        V <- sum((r-a.mean)^2)/(n-1)                        # Variance
        Si <- combn(a, 2, function(ij) diff(R.mean[ij])^2/sum(V/ni[ij]) )   # Individual comparison
        Pi <- pchisq(Si, df, lower.tail=FALSE)              # P Values
		scheffe <- cbind(Si, Pi)
		colnames(scheffe) <- c("Scheffe's TS", "P value")
		rownames(scheffe) <- combn(names(ni), 2, paste, collapse=":")
        RESULT <- list(Kruskal = kruskal, Scheffe = scheffe)
        return(RESULT)
}

