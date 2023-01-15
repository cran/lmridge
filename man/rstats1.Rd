\name{rstats1.lmridge}
\alias{rstats1}
\alias{rstats1.lmridge}
\alias{print.rstats1}

\title{Ordinary Ridge Regression Statistics 1}

\description{
The \code{rstats1} function computes the ordinary ridge related statistics such as variance, squared bias, MSE, R-squared and condition number (CN), etc. (see Lee, 1979; Kalivas and Palmer, 2014 <\doi{10.1002/cem.2555}>)}

\usage{
   rstats1(x, \dots)
   \method{rstats1}{lmridge}(x, \dots)
   \method{print}{rstats1}(x, digits = max(5,getOption("digits") - 5), \dots)
}

\arguments{
  \item{x}{An object of class "lmridge" (for the \code{rstats1} or \code{print.rstats1.lmridge)}}
  \item{digits}{Minimum number of significant digits to be used for most numbers.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{rstats1} function computes the ordinary ridge regression related statistics which may help in selecting optimal value of biasing parameter \eqn{K}{K}. If value of \eqn{K}{K} is zero then these statistics are equivalent to the relevant OLS statistics.
}

\value{Following are the ridge related statistics computed for given scalar or vector value of biasing parameter \eqn{K}{K} provided as argument to \code{lmridge} or \code{lmridgeEst} function.

\item{var}{Variance of ridge regression for given biasing parameter \eqn{K}{K}.}
\item{bias2}{Squared bias of ridge regression for given biasing parameter \eqn{K}{K}.}
\item{mse}{Total MSE value for given biasing parameter \eqn{K}{K}.}
\item{Fv}{F-statistics value for testing of the significance of the ordinary ridge regression estimator computed for given biasing parameter \eqn{K}{K}.}
\item{rfact}{Shrinkage factor \eqn{\frac{\lambda_j}{\lambda_j+K}}{[\lambda_j/(\lambda_j+K)]} for given biasing parameter \eqn{K}{K}.}
\item{R2}{R-squared for given biasing parameter \eqn{K}{K}.}
\item{adjR2}{Adjusted R-squared for given biasing parameter \eqn{K}{K}.}
\item{eigval}{Eigenvalue of \eqn{X'X}{X'X} matrix for \eqn{K=0}{K=0}.}
\item{CN}{Condition number after addition of biasing parameter in \eqn{X'X}{X'X} matrix.}
}

\references{
Cule, E. and De lorio, M. (2012). A semi-Automatic method to guide the choice of ridge parameter in ridge regression.  \emph{arXiv:1205.0686v1 [stat.AP]}.

Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975). Ridge Regression: Some Simulation. \emph{Communication in Statistics}, \strong{4}, 105-123. \doi{10.1080/03610927508827232}.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67. \doi{10.1080/00401706.1970.10488634}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Kalivas, J. H., and Palmer, J. (2014). Characterizing multivariate calibration tradeoffs (bias, variance, selectivity, and sensitivity) to select model tuning parameters. \emph{Journal of Chemometrics}, \strong{28}(5), 347--357. \doi{10.1002/cem.2555}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Ridge related statistics \code{\link{rstats2}}, the ridge model fitting \code{\link{lmridge}}, ridge var-cov matrix \code{\link{vcov}}
}

\examples{
data(Hald)
mod <- lmridge(y~., data = as.data.frame(Hald), K = seq(0,0.2, 0.005) )

rstats1(mod)

## Getting only Ridge MSE
rstats1(mod)[3]

rstats1(mod)$mse
}

\keyword{ridge statistics}
\keyword{ridge condition number}
