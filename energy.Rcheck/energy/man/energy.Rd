\name{edist}
\alias{edist}
\title{E-distance}
\description{
 Returns the E-distances (energy statistics) between clusters. 
}
\usage{
 edist(x, sizes, distance=FALSE, ix = 1:sum(sizes))
}
\arguments{
  \item{x}{ data matrix of pooled sample or Euclidean distances}
  \item{sizes}{ vector of sample sizes}
  \item{distance}{ logical: if TRUE, x is a distance matrix}
  \item{ix}{ a permutation of the row indices of x }
}
\details{
  A vector containing the pairwise two-sample multivariate 
  \eqn{\mathcal{E}}{E}-statistics for comparing clusters or samples is returned. 
  The e-distance between clusters is computed from the original pooled data, 
  stacked in matrix \code{x} where each row is a multivariate observation, or 
  from the distance matrix \code{x} of the original data, or distance object 
  returned by \code{dist}. The first \code{sizes[1]} rows of the original data 
  matrix are the first sample, the next \code{sizes[2]} rows are the second 
  sample, etc. The permutation vector \code{ix} may be used to obtain
  e-distances corresponding to a clustering solution at a given level in
  the hierarchy.
 
  The e-distance between two clusters \eqn{C_i, C_j}
  of size \eqn{n_i, n_j} 
  proposed by Szekely and Rizzo (2003ab)
  is the e-distance \eqn{e(C_i,C_j)}, defined by
  \deqn{e(C_i,C_j)=\frac{n_i n_j}{n_i+n_j}[2M_{ij}-M_{ii}-M_{jj}],
  }{e(S_i, S_j) = (n_i n_j)(n_i+n_j)[2M_(ij)-M_(ii)-M_(jj)],}
  where
  \deqn{M_{ij}=\frac{1}{n_i n_j}\sum_{p=1}^{n_i} \sum_{q=1}^{n_j}
     \|X_{ip}-X_{jq}\|,}{
     M_{ij} = 1/(n_i n_j) sum[1:n_i, 1:n_j] ||X_(ip) - X_(jq)||,}
     \eqn{\|\cdot\|}{|| ||} denotes Euclidean norm, and \eqn{X_{ip}}{
     X_(ip)} denotes the p-th observation in the i-th cluster.  
}
\value{
 A object of class \code{dist} containing the lower triangle of the
 e-distance matrix of cluster distances corresponding to the permutation 
 of indices \code{ix} is returned.
}
\references{ 
 Szekely, G. J. and Rizzo, M. L. (2003a) Hierarchical Clustering
 via Joint Between-Within Distances, submitted.
 
 Szekely, G. J. and Rizzo, M. L. (2003b) Testing for Equal
 Distributions in High Dimension, submitted.
 
 Szekely, G. J. (2000) \eqn{\mathcal{E}}{E}-statistics: Energy of 
 Statistical Samples, preprint.
} 
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\seealso{
 \code{\link{energy.hclust}}
 \code{\link{eqdist.etest}} \code{\link{ksample.e}}
 }
\examples{
 ## compute e-distances for 3 samples of iris data
 data(iris)
 edist(iris[,1:4], c(50,50,50))

\dontshow{
     ## compute e-distances from a distance object
     data(iris)
     edist(dist(iris[,1:4]), c(50, 50, 50), distance=TRUE)
    
     ## compute e-distances from a distance matrix
     data(iris)
     d <- as.matrix(dist(iris[,1:4]))
     edist(d, c(50, 50, 50), distance=TRUE) 
     }
 ## compute e-distances from vector of group labels
 d <- dist(matrix(rnorm(100), nrow=50))
 g <- cutree(energy.hclust(d), k=4)
 edist(d, sizes=table(g), ix=rank(g, ties.method="first"))
 }
\keyword{ multivariate }
\keyword{ cluster }
\keyword{ nonparametric }
\concept{ energy statistics }

\eof
\name{energy.hclust}
\alias{energy.hclust}
\title{ Hierarchical Clustering by Minimum (Energy) E-distance }
\description{
  Performs hierarchical clustering on a set of Euclidean distance 
  dissimilarities by minimum (energy) E-distance method.
}
\usage{
    energy.hclust(dst)
}
\arguments{
  \item{dst}{dissimilarity object produced by \code{dist} with
        \code{method=euclidean}, or lower triangle of distance
        matrix as vector in column order. If \code{dst} is a square
        matrix, the lower triangle is interpreted as a vector of
        distances.}
}
\details{
  This function performs agglomerative hierarchical cluster analysis
  based on the pairwise distances between sample elements in \code{dst}.
  Initially, each of the n singletons is a cluster. At each of n-1 steps, the 
  procedure merges the pair of clusters with minimum e-distance. 
  The e-distance
  between two clusters \eqn{C_i, C_j} of sizes \eqn{n_i, n_j} is given by
    \deqn{e(C_i, C_j)=\frac{n_i n_j}{n_i+n_j}[2M_{ij}-M_{ii}-M_{jj}],
    }
    where
    \deqn{M_{ij}=\frac{1}{n_i n_j}\sum_{p=1}^{n_i} \sum_{q=1}^{n_j}
       \|X_{ip}-X_{jq}\|,}{
       M_{ij} = 1/(n_i n_j) sum[1:n_i, 1:n_j] ||X_(ip) - X_(jq)||,}
       \eqn{\|\cdot\|}{|| ||} denotes Euclidean norm, and \eqn{X_{ip}}{
       X_(ip)} denotes the p-th observation in the i-th cluster.  

  The return value is an object of class \code{hclust}, so \code{hclust}
  methods such as print or plot methods, \code{plclust}, and \code{cutree}
  are available. See the documentation for \code{hclust}.
  
  The e-distance measures both the heterogeneity between clusters and the
  homogeneity within clusters. E-clustering is particularly effective in
  high dimension, and is more effective than some standard hierarchical
  methods when clusters have equal means (see example below).
  For other advantages see the references.

}
\value{
     An object of class \code{hclust} which describes the tree produced by
     the clustering process. The object is a list with components: 
\item{merge:}{ an n-1 by 2 matrix, where row i of \code{merge} describes the
     merging of clusters at step i of the clustering. If an element j in the
     row is negative, then observation -j was merged at this
     stage. If j is positive then the merge was with the cluster
     formed at the (earlier) stage j of the algorithm.} 
\item{height:}{the clustering height: a vector of n-1 non-decreasing
     real numbers (the e-distance between merging clusters)}
\item{order:}{ a vector giving a permutation of the indices of 
     original observations suitable for plotting, in the sense that a 
     cluster plot using this ordering and matrix \code{merge} will not have 
     crossings of the branches.}
\item{labels:}{ labels for each of the objects being clustered.}
\item{call:}{ the call which produced the result.}
\item{method:}{ the cluster method that has been used (e-distance).}
\item{dist.method:}{ the distance that has been used to create \code{dst}.}
}
\references{ 
     Szekely, G. J. and Rizzo, M. L. (2003) Hierarchical Clustering
     via Joint Between-Within Distances, submitted.

     Szekely, G. J. and Rizzo, M. L. (2003) Testing for Equal
     Distributions in High Dimension, submitted.

     Szekely, G. J. (2000) \eqn{\mathcal E}{E}-statistics: Energy of
     Statistical Samples, preprint.
}
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\seealso{ \code{\link{edist}} \code{\link{ksample.e}} \code{hclust}} }
\examples{
   \dontrun{   
   library(cluster)
   data(animals)
   plot(energy.hclust(dist(animals)))
   }
   
   library(mva)
   data(USArrests)
   ecl <- energy.hclust(dist(USArrests))
   print(ecl)    
   plot(ecl)
   cutree(ecl, k=3)
   cutree(ecl, h=150)
   
   ## compare performance of e-clustering, Ward's method, group average method
   ## when sampled populations have equal means: n=200, d=5, two groups
   z <- rbind(matrix(rnorm(1000), nrow=200), matrix(rnorm(1000, 0, 5), nrow=200))
   g <- c(rep(1, 200), rep(2, 200))
   d <- dist(z)
   e <- energy.hclust(d)
   a <- hclust(d, method="average")
   w <- hclust(d^2, method="ward")
   list("E" = table(cutree(e, k=2) == g), "Ward" = table(cutree(w, k=2) == g),
 	"Avg" = table(cutree(a, k=2) == g))
 }
\keyword{ multivariate }
\keyword{ cluster }
\concept{ energy statistics }

\eof
\name{eqdist.etest}
\alias{eqdist.etest}
\title{Multisample E-statistic (Energy) Test of Equal Distributions}
\description{
 Performs the nonparametric multisample E-statistic (energy) test
 for equality of multivariate distributions. 
}
\usage{
 eqdist.etest(x, sizes, distance = FALSE, 
              incomplete = FALSE, N = 100, R = 999)
}
\arguments{
  \item{x}{ data matrix of pooled sample}
  \item{sizes}{ vector of sample sizes}
  \item{distance}{logical: if TRUE, first argument is a distance matrix}
  \item{incomplete}{logical: if TRUE, compute incomplete E-statistics}
  \item{N}{sample size for incomplete statistics}
  \item{R}{ number of bootstrap replicates }
}
\details{
  The k-sample multivariate \eqn{\mathcal{E}}{E}-test of equal distributions
  is performed. The statistic is computed from the original
  pooled samples, stacked in matrix \code{x} where each row
  is a multivariate observation, or the corresponding distance matrix. The
  first \code{sizes[1]} rows of \code{x} are the first sample, the next
  \code{sizes[2]} rows of \code{x} are the second sample, etc.
 
  The test is implemented by nonparametric bootstrap, an approximate 
  permutation test with \code{R} replicates. For large samples it is
  more efficient if \code{x} contains the data matrix rather than the
  distances. Incomplete statistics are supported for the two-sample test. If
  \code{incomplete==TRUE}, at most \code{N} observations from each sample 
  (by sampling without replacement) are used in the calculation of the statistic.
  If \code{distance==TRUE} complete statistics are always computed.
  
  The definition of the multisample \eqn{\mathcal{E}}{E}-statistic is given in the 
  \code{\link{ksample.e}} documentation.
}
\value{
 A list with class \code{etest.eqdist} containing
 \item{method}{Description of test}
 \item{statistic}{Observed value of the test statistic}
 \item{p.value}{Approximate p-value of the test}
 \item{sizes}{Vector of sample sizes}
 \item{R}{Number of replicates}
 \item{replicates}{Vector of replicates of the statistic}
}

\references{ 
 Szekely, G. J. and Rizzo, M. L. (2003) Testing for Equal
 Distributions in High Dimension, submitted.
 
 Szekely, G. J. (2000) \eqn{\mathcal{E}}{E}-statistics: Energy of 
 Statistical Samples, preprint.

} 
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}

\seealso{
 \code{\link{ksample.e}},
 \code{\link{print.etest.eqdist}}
 \code{\link{edist}}
 \code{\link{energy.hclust}}
 }
\examples{
 data(iris)
 
 ## test if the 3 varieties of iris data (d=4) have equal distributions
 eqdist.etest(iris[,1:4], c(50,50,50))

 ## compare incomplete versions of two sample test
 x <- c(rpois(400, 2), rnbinom(600, size=1, mu=2))
 eqdist.etest(x, c(400, 600), incomplete=TRUE, N=100)
 eqdist.etest(x, c(400, 600), incomplete=TRUE, N=200)
  
\dontshow{
  x <- matrix(rnorm(500), nrow=100)
  y <- matrix(rnorm(500, mean=5), nrow=100)
  x <- rbind(x, y)
  eqdist.etest(dist(x), sizes=c(100, 100), distance=TRUE)
  eqdist.etest(x, sizes=c(100, 100), incomplete=TRUE, N=50, R=100)
}
}
\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
\concept{ energy statistics}

\eof
\name{ksample.e}
\alias{ksample.e}
\title{E-statistic (Energy Statistic) for Multivariate k-sample Test of Equal Distributions}
\description{
 Returns the E-statistic (energy statistic)
 for the multivariate k-sample test of equal distributions. 
}
\usage{
 ksample.e(x, sizes, distance = FALSE, ix = 1:sum(sizes), 
           incomplete = FALSE, N = 100)
}
\arguments{
  \item{x}{ data matrix of pooled sample}
  \item{sizes}{ vector of sample sizes}
  \item{distance}{ logical: if TRUE, x is a distance matrix}
  \item{ix}{ a permutation of the row indices of x }
  \item{incomplete}{ logical: if TRUE, compute incomplete \eqn{\mathcal{E}}{E}-statistics}
  \item{N}{ incomplete sample size}
}
\details{
  The k-sample multivariate \eqn{\mathcal{E}}{E}-statistic for testing equal distributions
  is returned. The statistic is computed from the original pooled samples, stacked in 
  matrix \code{x} where each row is a multivariate observation, or from the distance 
  matrix \code{x} of the original data. The
  first \code{sizes[1]} rows of \code{x} are the first sample, the next
  \code{sizes[2]} rows of \code{x} are the second sample, etc. Incomplete statistics are 
  supported for the two-sample case. If
  \code{incomplete==TRUE}, at most \code{N} observations from each sample 
  (by sampling without replacement) are used in the calculation of the statistic.
  If \code{distance==TRUE} complete statistics are always computed.
 
  The two-sample \eqn{\mathcal{E}}{E}-statistic proposed by Szekely and Rizzo (2003)
  is the e-distance \eqn{e(S_i,S_j)}, defined for two samples \eqn{S_i, S_j}
  of size \eqn{n_i, n_j} by
  \deqn{e(S_i,S_j)=\frac{n_i n_j}{n_i+n_j}[2M_{ij}-M_{ii}-M_{jj}],
  }{e(S_i, S_j) = (n_i n_j)(n_i+n_j)[2M_(ij)-M_(ii)-M_(jj)],}
  where
  \deqn{M_{ij}=\frac{1}{n_i n_j}\sum_{p=1}^{n_i} \sum_{q=1}^{n_j}
     \|X_{ip}-X_{jq}\|,}{
     M_{ij} = 1/(n_i n_j) sum[1:n_i, 1:n_j] ||X_(ip) - X_(jq)||,}
     \eqn{\|\cdot\|}{|| ||} denotes Euclidean norm, and \eqn{X_{ip}}{
     X_(ip)} denotes the p-th observation in the i-th sample.  
  The k-sample  
  \eqn{\mathcal{E}}{E}-statistic is defined by summing the pairwise e-distances over 
  all \eqn{k(k-1)/2} pairs 
  of samples:
  \deqn{\mathcal{E}=\sum_{1 \leq i < j \leq k} e(S_i,S_j).
  }{\emph{E} = sum[i<j] e(S_i,S_j).}  
  Large values of \eqn{\mathcal{E}}{\emph{E}} are significant.
}
\value{
 The value of the multisample \eqn{\mathcal{E}}{E}-statistic corresponding to
 the permutation \code{ix} is returned.
}
\references{ 
 Szekely, G. J. and Rizzo, M. L. (2003) Testing for Equal
 Distributions in High Dimension, submitted.
 
 Szekely, G. J. (2000) \eqn{\mathcal{E}}{E}-statistics: Energy of 
 Statistical Samples, preprint.
} 
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\note{  This function computes the \eqn{\mathcal{E}}{E}-statistic only. 
 For the test decision,
 a nonparametric bootstrap test (approximate permutation test)
 is provided by the function \code{\link{eqdist.etest}}.
 }
\seealso{
 \code{\link{eqdist.etest}}
 \code{\link{edist}}
 \code{\link{energy.hclust}} 
 }
\examples{
## compute 3-sample E-statistic for 4-dimensional iris data
 data(iris)
 ksample.e(iris[,1:4], c(50,50,50))

## compute a 3-sample univariate E-statistic
 ksample.e(rnorm(150), c(25,75,50))
}

\keyword{ multivariate }
\keyword{ htest }
\keyword{ nonparametric }
\concept{ energy statistics }

\eof
\name{mvnorm.e}
\alias{mvnorm.e}
\title{ E-statistic (Energy Statistic) for Testing Multivariate Normality}
\description{
 Computes the E-statistic (energy statistic) for testing multivariate 
 or univariate normality when parameters are estimated.
}
\usage{
mvnorm.e(x)
}
\arguments{
  \item{x}{ matrix or vector of sample data}
}
\details{
 If \code{x} is a matrix, each row is a multivariate observation. The
 data will be standardized to zero mean and identity covariance matrix
 using the sample mean vector and sample covariance matrix. If \code{x}
 is a vector, the univariate statistic \code{normal.e(x)} is returned. 
 If the data contains missing values or the sample covariance matrix is 
 singular, NA is returned.
 
 The \eqn{\mathcal{E}}{E}-test of multivariate normality was proposed
 and implemented by Szekely and Rizzo (2004). The test statistic for 
 d-variate normality is given by
 \deqn{\mathcal{E} = n (\frac{2}{n} \sum_{i=1}^n E\|y_i-Z\| - 
 E\|Z-Z'\| - \frac{1}{n^2} \sum_{i=1}^n \sum_{j=1}^n \|y_i-y_j\|),
 }{E = n((2/n) sum[1:n] E||y_i-Z|| - E||Z-Z'|| - (1/n^2) sum[1:n,1:n]
 ||y_i-y_j||),}
 where \eqn{y_1,\ldots,y_n} is the standardized sample, 
 \eqn{Z, Z'} are iid standard d-variate normal, and
 \eqn{\| \cdot \|}{|| ||} denotes Euclidean norm.
}
\value{
 The value of the \eqn{\mathcal{E}}{E}-statistic for multivariate
 (univariate) normality is returned.
}
\references{
 Szekely, G. J. and Rizzo, M. L. (2004) A New Test for 
 Multivariate Normality, \emph{Journal of Multivariate Analysis},
 \url{http://dx.doi.org/10.1016/j.jmva.2003.12.002}.

 Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit Test,
 Ph.D. dissertation, Bowling Green State University.
 
 Szekely, G. J. (1989) Potential and Kinetic Energy in Statistics, 
 Lecture Notes, Budapest Institute of Technology (Technical University). 
 }
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}

\seealso{ \code{\link{mvnorm.etest}}, \code{\link{normal.e}}}

\examples{
 
 ## compute multivariate normality test statistic for iris Setosa data
 data(iris)
 mvnorm.e(iris[1:50, 1:4])
 }
\keyword{ multivariate }
\keyword{ htest }
\concept{ energy statistics }

\eof
\name{mvnorm.etest}
\alias{mvnorm.etest}
\title{E-statistic (Energy) Test of Multivariate Normality}
\description{
 Performs the E-statistic (energy) test of multivariate or univariate normality.
}
\usage{
 mvnorm.etest(x, R = 999)
}
\arguments{
  \item{x}{ data matrix of multivariate sample, or univariate data vector}
  \item{R}{ number of bootstrap replicates }
}
\details{
  The \eqn{\mathcal{E}}{E}-test of multivariate (univariate) normality
  is performed. The test is implemented by parametric bootstrap with 
  \code{R} replicates. 
  
  The definition of the \eqn{\mathcal{E}}{E}-statistic is given in the 
  \code{\link{mvnorm.e}} documentation.
}
\value{
 A list with class \code{etest.mvnorm} containing
 \item{method}{Description of test}
 \item{statistic}{Observed value of the test statistic}
 \item{p.value}{Approximate p-value of the test}
 \item{n}{Sample size}
 \item{R}{Number of replicates}
 \item{replicates}{Vector of replicates of the statistic}
}

\references{ 
 Szekely, G. J. and Rizzo, M. L. (2004) A New Test for 
 Multivariate Normality, \emph{Journal of Multivariate Analysis},
 \url{http://dx.doi.org/10.1016/j.jmva.2003.12.002}.
 
 Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit Test,
 Ph.D. dissertation, Bowling Green State University.
 
 Szekely, G. J. (1989) Potential and Kinetic Energy in Statistics, 
 Lecture Notes, Budapest Institute of Technology (Technical University). 
} 
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}

\seealso{
 \code{\link{mvnorm.e}},
 \code{\link{print.etest.mvnorm}}
 }
\examples{
 ## test if the iris Setosa data has multivariate normal distribution
 data(iris)
 mvnorm.etest(iris[1:50,1:4])
 
 ## test a univariate sample for normality
 x <- runif(50, 0, 10)
 mvnorm.etest(x)
 }
\keyword{ multivariate }
\keyword{ htest }
\concept{ energy statistics }

\eof
\name{normal.e}
\alias{normal.e}
\title{ E-statistic (Energy Statistic) for Testing Univariate Normality}
\description{
 Computes the E-statistic for testing univariate normality 
 when parameters are estimated.
}
\usage{
normal.e(x)
}
\arguments{
  \item{x}{ vector of univariate sample data}
}
\details{
 The
 data will be standardized to zero mean and unit variance
 using the sample mean and sample variance. If the data contains
 missing values or the sample variance is zero, NA is
 returned.
 
 The 
 \eqn{\mathcal{E}}{E}-test of multivariate (and univariate)
  normality was proposed and implemented by Szekely and Rizzo 
  (2004). The univariate test statistic
 is given by
 \deqn{\mathcal{E} = n (\frac{2}{n} \sum_{i=1}^n E|y_i-Z| - E|Z-Z'| -
 \frac{1}{n^2} \sum_{i=1}^n \sum_{j=1}^n |y_i-y_j|),
 }{n((2/n) sum[1:n] E|y_i-Z| - E|Z-Z'| - (1/n^2) sum[1:n,1:n]
 |y_i-y_j|),}
 where \eqn{y_1,\ldots,y_n} is the standardized sample and
 \eqn{Z, Z'} are iid standard normal variables. See 
 \code{\link{mvnorm.e}} for the multivariate statistic.
}
\value{
 The value of the \eqn{\mathcal{E}}{E}-statistic for univariate
 normality is returned.
}
\references{
 Szekely, G. J. and Rizzo, M. L. (2004) A New Test for 
 Multivariate Normality, \emph{Journal of Multivariate Analysis},
 \url{http://dx.doi.org/10.1016/j.jmva.2003.12.002}.

 Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit Test,
 Ph.D. dissertation, Bowling Green State University.
 
 Szekely, G. J. (1989) Potential and Kinetic Energy in Statistics, 
 Lecture Notes, Budapest Institute of Technology (Technical University). 
 }
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}

\seealso{ \code{\link{mvnorm.e}}, \code{\link{mvnorm.etest}}}

\examples{
 x <- rnorm(30)
 normal.e(x)
}
\keyword{ htest }
\concept{ energy statistics }

\eof
\name{poisson.m}
\alias{poisson.m}
\title{ Mean Distance Statistic for Testing Poisson Distribution}
\description{
 Returns the mean distance statistic for a goodness-of-fit test of Poisson distribution with unknown parameter.
}
\usage{
poisson.m(x)
}
\arguments{
  \item{x}{ vector of nonnegative integers, the sample data }
}
\details{
The mean distance test of Poissonity was proposed and implemented by Szekely and Rizzo (2004). The test is based on the result that the sequence of expected values E|X-j|, j=0,1,2,... characterizes the distribution of the random variable X. As an application of this characterization one can get an estimator \eqn{\hat F(j)} of the CDF. The test statistic is a Cramer-von Mises type of distance, with M-estimates replacing the usual EDF estimates of the CDF: 
 \deqn{M_n = n\sum_{j=0}^\infty (\hat F(j) - F(j\;; \hat \lambda))^2
f(j\;; \hat \lambda).}{M_n = n sum [j>=0] (\hat F(j) - F(j; \hat \lambda))^2
f(j; \hat \lambda).} See Szekely and Rizzo (2004) for the computing formula. 

}
\value{
 The value of the \eqn{M}-statistic for testing Poisson distribution is returned.
}
\references{ 
Szekely, G. J. and Rizzo, M. L. (2004) Mean Distance Test of Poisson Distribution, \emph{Statistics and Probability Letters}, 
67/3, 241-247. \url{http://dx.doi.org/10.1016/j.spl.2004.01.005}.
}
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\seealso{ \code{\link{poisson.mtest}}}
\examples{
 x <- rpois(20, 1)
poisson.m(x)
 }
\keyword{ htest }

\eof
\name{poisson.mtest}
\alias{poisson.mtest}
\title{ Mean Distance Test for Poisson Distribution}
\description{
 Performs the mean distance goodness-of-fit test of Poisson distribution
 with unknown parameter.
}
\usage{
poisson.mtest(x, R=999)
}
\arguments{
  \item{x}{ vector of nonnegative integers, the sample data }
  \item{R}{ number of bootstrap replicates }
}
\details{
 The mean distance test of Poissonity was proposed and implemented by 
 Szekely and Rizzo (2004). The test is based on the result that the sequence 
 of expected values E|X-j|, j=0,1,2,... characterizes the distribution of 
 the random  variable X. As an application of this characterization one can 
 get an estimator \eqn{\hat F(j)} of the CDF. The test statistic 
 (see \code{\link{poisson.m}}) is a Cramer-von Mises type of distance, with 
 M-estimates replacing the usual EDF estimates of the CDF:
  \deqn{M_n = n\sum_{j=0}^\infty (\hat F(j) - F(j\;; \hat \lambda))^2
 f(j\;; \hat \lambda).}{M_n = n sum [j>=0] (\hat F(j) - F(j; \hat \lambda))^2
 f(j; \hat \lambda).} The test is implemented by parametric bootstrap with 
 \code{R} replicates. 
}
\value{
 A list with class \code{etest.poisson} containing
 \item{method}{Description of test}
 \item{statistic}{Observed value of the test statistic}
 \item{p.value}{Approximate p-value of the test}
 \item{n}{Sample size}
 \item{lambda}{Sample mean}
 \item{R}{Number of replicates}
 \item{replicates}{Vector of replicates of the statistic}
}
\references{ 
Szekely, G. J. and Rizzo, M. L. (2004) Mean Distance Test of Poisson Distribution, \emph{Statistics and Probability Letters}, 
67/3, 241-247. \url{http://dx.doi.org/10.1016/j.spl.2004.01.005}.
}
\author{ Maria L. Rizzo \email{rizzo@math.ohiou.edu} and
Gabor J. Szekely \email{gabors@bgnet.bgsu.edu}}
\seealso{ \code{\link{poisson.m}}}
\examples{
 x <- rpois(20, 1)
 poisson.mtest(x)
 }
\keyword{ htest }

\eof
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

\eof
\name{print.etest.mvnorm}
\alias{print.etest.mvnorm}
\title{ Print E-test (Energy Test) of Multivariate Normality }
\description{
 Print method for \code{etest.mvnorm} object returned by the 
 \code{mvnorm.etest} function. 
}
\usage{
print.etest.mvnorm(x, ...)
}
\arguments{
  \item{x}{ an object of class \code{etest.mvnorm} }
  \item{...}{ extra arguments passed to or from other methods }  
}
\seealso{ \code{\link{mvnorm.etest}} }
\examples{
 ## print E-test for 5-dimensional data
 x <- matrix(rnorm(100), nrow=20, ncol=5)
 e <- mvnorm.etest(x)
 print.etest.mvnorm(e)
 }
\keyword{ print }

\eof
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

\eof
