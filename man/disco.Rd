\name{disco}
\alias{disco}
\alias{print.disco}
\title{ distance components (DISCO)}
\description{
  E-statistics DIStance COmponents and tests, analogous to variance components
}
\usage{
disco(x, factors, distance, index=1.0, R=0) 
}
\arguments{
  \item{x}{ data matrix or distance matrix}
  \item{factors}{ matrix of factor labels or integers (not design matrix)}
  \item{distance}{ logical, TRUE if x is distance matrix}
  \item{index}{ exponent on Euclidean distance in (0,2]}
  \item{R}{ number of replicates for a permutation test} 
}
\details{
  \code{disco} calculates the distance components decomposition of 
  total dispersion and if R > 0 tests for significance using the 
  disco F ratio implemented by permutation test.
  
  In the current release \code{disco} computes the decomposition for one-way models
  only.   
}
\value{
\code{disco} returns a class disco object, which is a list containing
\item{call}{call}
\item{method}{method}
\item{statistic}{vector of observed statistics}
\item{p.value}{vector of p-values}
\item{k}{number of factors}
\item{N}{number of observations}
\item{between}{between-sample distance components}
\item{withins}{one-way within-sample distance components}
\item{within}{within-sample distance component}
\item{total}{total dispersion}
\item{Df.trt}{degrees of freedom for treatments}
\item{Df.e}{degrees of freedom for error}
\item{index}{index (exponent on distance)}
\item{factor.names}{factor names}
\item{factor.levels}{factor levels}
\item{sample.sizes}{sample sizes}
\item{stats}{matrix containing decomposition}
}
\references{ 
M. L. Rizzo and G. J. Szekely (2010).
DISCO Analysis: A Nonparametric Extension of
Analysis of Variance, Annals of Applied Statistics, 
Vol. 4, No. 2, 1034-1055. 
\cr \url{http://dx.doi.org/10.1214/09-AOAS245}
}
\note{
The current version does all calculations via matrix arithmetic and 
boot function; the C library is not used. The faster C version will
be added in the next release. Support for more general additive models
and a formula interface is also under development.

\code{disco} methods have been added to the cluster distance summary
function \code{edist}, and energy tests for equality of distribution
(see \code{eqdist.etest}). 
}
\seealso{
 \code{ \link{edist} } 
 \code{ \link{eqdist.e} }
 \code{ \link{eqdist.etest} }
 \code{ \link{ksample.e} }
 }
 \author{ Maria L. Rizzo \email{mrizzo @ bgsu.edu} and Gabor J. Szekely 
 }
\examples{
      ## warpbreaks one-way decompositions
      data(warpbreaks)
      attach(warpbreaks)
      disco(breaks, factors=wool, R=199)
      
      ## When index=2 for univariate data, we get ANOVA
      disco(breaks, factors=tension, index=2.0, R=199)
      anova(lm(breaks ~ tension))
      
      ## Multivariate response
      ## Example on producing plastic film from Krzanowski (1998, p. 381)
      tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
                6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
      gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
                 9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
      opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
                   2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
      Y <- cbind(tear, gloss, opacity)
      rate <- factor(gl(2,10), labels=c("Low", "High"))  
      disco(Y, factors=rate, R=199)     
      
      ## Just extract the decomposition table
      disco(Y, factors=rate)$stats      
}
\keyword{ htest }
\keyword{ multivariate }


