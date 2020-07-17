#' Statistics for data with k independent variables
#' Compute mean and standard deviation for each group in data with k independent variables. Statistical analysis may be applied for data.
#'
#' @param i Column number(s) of variable(s) for analysis
#' @param j Column number of group
#' @param df Data frame
#' @param latex LaTeX output
#' @param test Choose statistical test to be performed from "parametric" or "non-parametric". For parametric, t.test or oneway.test is called for 2 groups or 3 groups, respectively. For non-parametric, wilcox.test or kruskal.test is called for 2 groups or 3 groups, respectively. No statistical test is performed as default.
#' @param var.equal Used for augment var.equal in t.test and oneway.test. By default (NULL), this is set as TRUE if P-value for equal variance test is greater than 0.2, otherwise set as FALSE. This can be know by checking t-value (F-value). If freedom is less than 1, var.equal is set as FALSE. FALSE is a default for t.test.
#' @param digits Number of digits under decimal point for mean and standard deviation
#' @param output Output connection. Console as default.
#'
#' @importFrom stats complete.cases var.test t.test wilcox.test bartlett.test oneway.test kruskal.test complete.cases
#'
#' @examples
#' Sex <- c(1,1,1,2,1,2,1,2,1,2,2,2,1,1,2,1,2,2,2,2,1,2,1,1,1,1,2,2,2,2)
#' BS1 <- c(101.3,101.8,122.6,114.4,89.7,83.4,93.2,99.6,127.5,104.7,84.2,90.6,90,97.6,97.4,92.8,
#'     103.4,101.2,87.8,95.5,89,94.3,69.9,79.4,105.7,95.2,113.7,106.8,112.1,91.7)
#' BS2 <- c(135.8,165.7,178.9,155.8,143,123.7,151.3,108.8,101.6,156.1,173.7,
#'     122.6,102.2,176.7,125,172.8,146.5,159,148,140.5,171.8,121.2,114.9,160,
#'     98.1,133.5,86.5,148.9,206.1,163)
#' df <- as.data.frame(cbind(Sex, BS1, BS2))
#' df[,1] <- factor(df[,1], levels=1:2, labels=c("M", "F"))
#' colnames(df) <- c("Sex", "Blood Sugar before meal", "Blood Sugar after meal")
#' ind.stats(2:3, 1, df, test="parametric")
#' ind.stats(2:3, 1, df, test="non-parametric")
#'
#' @keywords nonparametric
#'
#' @author Shoji F. Nakayama
#'
#' @export
#'

ind.stats <-
function(i,                                                     # Column number for valuables
                         j,                                                     # Column number for group
                         df,                                                    # data frame
                         latex=FALSE,                                           # Output by LaTeX format (default: FALSE)
                         test=c("none", "parametric", "non-parametric"),        # Statistical test (default: none)
                         var.equal=NULL,                                        # For parametric test
                         digits=3,                                              # Decimal point for mean and SD
                         output="")                                             # Output file name (default: console)
{

# subfunction
        ind.stats.sub <- function(ii, jj)
        {
                group <- colnames(df)[jj]                                       # Group name
                df2 <- df[, c(ii, jj)]                                          # Extract 2 valuables from column ii and jj from data frame
                df2 <- subset(df2, complete.cases(df2))                         # Remove NA
                x <- df2[, 1]                                                   # Valuable for analysis
                g <- df2[, 2]                                                   # Group valuable
                lst <- list(g)                                                  # Make list from each group to apply by function
                nt <- length(x)                                                 # Sample size of the population
                mt <- mean(x)                                                   # Mean of the population
                st <- sd(x)                                                     # SD of the population
                n <- by(x, lst, length)                                         # Sample size of each group
                m <- by(x, lst, mean)                                           # mean of each group
                s <- by(x, lst, sd)                                             # SD of each group
                nr <- length(table(lst))                                        # Number of groups
                if (latex) {                                                    # Output by LaTeX format
                        cat("\n\\begin{table}[htbp]\n", file=output)            #
                        cat(sprintf("\\caption{%s by %s}\n", colnames(df2)[1], group), file=output) #
                        cat("\\begin{center}\n", file=output)                   #
                        cat("\\begin{tabular}{lccc} \\hline\n",file=output)     #
                        cat(sprintf("& \\multicolumn{3}{c}{%s}\\\\ \\cline{2-4}\n", colnames(df2)[1]), file=output) #
                        cat(group, "N", "Mean", "SD", sep=" & ", file=output)   #
                        cat("\\\\ \\hline\n", file=output)                      #
                        for (l in 1:nr) {                                       #
                                cat(names(n)[l], n[l], sprintf(format, m[l]), sprintf(format, s[l]), sep=" & ", file=output) #
                                cat("\\\\", file=output)                        #
                                if (l == nr) cat("\\hline\n", file=output) else cat("\n", file=output)  #
                        }
                        cat("Tatal", nt, sprintf(format, mt), sprintf(format, st), sep=" & ", file=output)#
                        cat("\\\\ \\hline\n", file=output)                      #
                        cat("\\end{tabular}\n", file=output)                    #
                }
                else {                                                          #
                        cat("\nTable: ", colnames(df2)[1], " by ", group, sep="", file=output)
                        #cat("\n", colnames(df2)[1], sep="\t", file=output, fill=TRUE)       #
                        cat(group, "N", "Mean", "SD", sep="\t", file=output, fill=TRUE)     # group N mean SD
                        for (l in 1:nr) {                                                   # by each group
                                cat(names(n)[l], n[l], sprintf(format, m[l]), sprintf(format, s[l]), sep="\t", file=output, fill=TRUE)  #
                        }
                        cat("Ttl", nt, sprintf(format, mt), sprintf(format, st), sep="\t", file=output, fill=TRUE)             # Total value value value
                }
                if (nr == 2) {                                                  # 2 groups
                        if (latex && test != "none") {                          # For LaTeX output, line break
                                cat("\\\\ \\noindent\n", file=output)
                        }
                        if (test == "parametric") {                             # T-test using t.test
                                if (is.null(var.equal)) {                       # var.equal
                                        var.equal <- var.test(x~g)$p.value > 0.2# If P > 0.2, assume equal variance
                                }
                                res <- t.test(x~g, var.equal=var.equal)         # Call t.test
                                cat(sprintf(if (latex) "$t$ value = %.3f, dF = %.3f, $P$ value = %.3f\n"
                                            else "t value = %.3f, dF = %.3f, P value = %.3f\n",
                                            res$statistic, res$parameter, res$p.value), file=output)
                        }
                        else if (test == "non-parametric") {                    # Wilcoxon's U test
                                res <- wilcox.test(x~g)                         # Call wilcox.test
                                cat(sprintf(if (latex) "$U$ = %.3f, $P$ value = %.3f\n"
                                            else "U = %.3f, P value = %.3f\n",
                                            res$statistic, res$p.value), file=output)
                        }
                }
                else if (nr >= 3) {
                        if (test == "parametric") {                             # ANOVA
                                if (is.null(var.equal)) {                       # Set default of var.equal for opneway.test
                                        var.equal <- bartlett.test(x ~ g)$p.value > 0.2 # If P > 0.2, assume equal variance
                                }
                                res <- oneway.test(x ~ g, var.equal=var.equal)
                                cat(sprintf(if (latex) "$F$ value = %.3f, 1st dF = %i, 2nd dF = %.3f, $P$ value = %.3f\n"
                                            else "F value = %.3f, 1st dF = %i, 2n dF = %.3f, P value = %.3f\n",
                                            res$statistic, res$parameter[1], res$parameter[2], res$p.value), file=output)
                        }
                        else if (test == "non-parametric") {                    # Kruskal-Wallis test
                                res <- kruskal.test(x~g)
                                cat(sprintf(if (latex) "$\\chi_{kw}^2$ = %.3f, dF = %i, $P$ value = %.3f\n"
                                            else "Chi-square = %.3f, dF = %i, P value = %.3f\n",
                                            res$statistic, res$parameter, res$p.value), file=output)
                        }
                }
                if (latex) {                                                    # For LaTeX output
                        cat("\\end{center}\n", file=output)                     # \end{center}
                        cat("\\end{table}\n", file=output)                      # \end{table}
                }
        }

# main function
        test <- match.arg(test)                                                 #
        format <- paste("%.", digits, "f", sep="")                              #
        for (jj in j) {                                                         #
                for (ii in i) {                                                 #
                        if (ii != jj) {                                         #
                                ind.stats.sub(ii, jj)                        #
                        }
                }
        }
}

