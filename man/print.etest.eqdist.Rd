\name{print.etest.eqdist}
\alias{print.etest.eqdist}
\title{ Print Multisample E-test (Energy Test) for Equal Distributions}
\description{
 Print method for \code{etest.eqdist} object returned by the 
 \code{eqdist.test} function. 
}
\usage{
print.etest.eqdist(x, ...)
}
\arguments{
  \item{x}{ an object of class \code{etest.eqdist} }
  \item{...}{ extra arguments passed to or from other methods }
}
\seealso{ \code{\link{eqdist.etest}} }
\examples{
## print test if the 3 varieties of iris data (d=4) have equal distributions
 data(iris)
 e <- eqdist.etest(iris[,1:4], c(50,50,50))
 print.etest.eqdist(e)
 }
\keyword{ print }
