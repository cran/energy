\name{print.etest.poisson}
\alias{print.etest.poisson}
\title{ Print Mean Distance Test for Poisson Distribution }
\description{
 Print method for \code{etest.poisson} object returned by the 
 \code{poisson.mtest} function. 
}
\usage{
print.etest.poisson(x, ...)
}
\arguments{
  \item{x}{ an object of class \code{etest.poisson} }
  \item{...}{ extra arguments passed to or from other methods }  
}
\seealso{ \code{\link{poisson.mtest}} }
\examples{
 x <- rpois(20, 1)
 e <- poisson.mtest(x)
 print.etest.poisson(e)
 }
\keyword{ print }
