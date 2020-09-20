#' r4jecs
#'
#' This is a package used in JECS project.
#'
#' @docType package
#' @name r4jecs
#'
#' @import  NADA ranger foreach parallel tibble doParallel stringr grDevices ggplot2
#'
#' @importFrom psych describe
#' @importFrom stats complete.cases var.test t.test wilcox.test bartlett.test oneway.test kruskal.test pchisq
#' @importFrom utils combn tail
#' @importFrom NADA cenfit censummary
#' @importFrom tibble rownames_to_column
#' @importFrom ranger ranger
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom stringr str_replace
#' @importFrom ggplot2 discrete_scale scale_color_gradientn discrete_scale scale_fill_gradientn
#'
#'
"_PACKAGE"

