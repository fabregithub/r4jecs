% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nsf.R
\name{nsf}
\alias{nsf}
\title{Number of significant figures and number below decimal point, general}
\usage{
nsf(data, NSF, NBD)
}
\arguments{
\item{data}{data}

\item{NSF}{Number of significant figures}

\item{NBD}{Number below decimal point}
}
\description{
Take care of the number of significant figures (NSF) and number below decimal point with the following criteria (NBD).
}
\details{
This sets the number of significant figures as NSF and number below decimal point as NBD. It removes trailing '.' and '0.'
}
\examples{
\dontrun{
x <- c(1534, 153.4, 15.34, 1.534, 0.1534, 0.01534, 0.001534, 0.190, 0.200,NA)
nsf(x, 3, 2)
nsf(x, 2, 2)
nsf(x, 3, 3)
nsf(x, 1, 1)

y <- c(5432, 543.2, 54.32, 5.432, 0.5432, 0.05432, 0.005432, 1.00, 0.0000, 0.00001)
z <- data.frame(X = x, Y = y)
apply(z, 2, nsf, NSF = 3, NBD = 2)
}

}
\author{
Shoji F. Nakayama
}
