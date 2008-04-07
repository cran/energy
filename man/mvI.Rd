\name{mvI}
\alias{mvI}
\title{ Multivariate Independence Coefficient I}
\description{
 Computes a multivariate coefficient of independence.
 }
\usage{
    mvI(x, y)
}
\arguments{
  \item{x}{ matrix: first sample, observations in rows}
  \item{y}{ matrix: second sample, observations in rows}
}
\details{
 Computes the coefficient \eqn{\mathcal I_n}{I_n}. See \code{mvI.test}
 and the reference below for more details. 
}
\value{
 Returns the scalar statistic, independence coefficient \eqn{\mathcal I_n}{I_n}.
 }
\note{As of energy-1.1-0,
\code{indep.e} is deprecated and replaced by \code{mvI}. 
 } 
 \seealso{
 \code{ \link{indep.test} } 
 \code{ \link{mvI.test} } 
 \code{ \link{dcov.test} } 
 \code{ \link{dcov} }
 }
\references{
 Bakirov, N.K., Rizzo, M.L., and Szekely, G.J. (2006), A Multivariate
 Nonparametric Test of Independence, \emph{Journal of Multivariate Analysis} 
 93/1, 58-80,
 \cr \url{http://dx.doi.org/10.1016/j.jmva.2005.10.005}
 }
\author{ Maria L. Rizzo \email{mrizzo @ bgnet.bgsu.edu} and
Gabor J. Szekely \email{gabors @ bgnet.bgsu.edu}
}
\keyword{ multivariate }
\concept{ independence }
\concept{ energy statistics }

