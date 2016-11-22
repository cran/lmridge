\name{vcov.lmridge}
\alias{vcov.lmridge}
\alias{vcov}

\title{Variance-Covariance Matrix for Fitted Ridge Model}

\description{The \code{vcov} function computes the variance-covariance matrix for the estimates of linear ridge regression model.}

\usage{\method{vcov}{lmridge}(object, \dots)}

\arguments{
  \item{object}{For \code{VCOV} method, an object of class "lmridge", i.e., a fitted model.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{vcov} function computes variance-covariance matrix for scalar or vector value of biasing parameter \eqn{K}{K} provided as argument to \code{lmridge} function.}

\value{
A list of matrix of estimated covariances in the linear ridge regression model for scalar or vector biasing parameter \code{K}{K} is produced. Each list element has row and column names corresponding to the parameter names given by the \code{coef(mod)}. List items are named correspond to values of biasing parameter \eqn{K}{K}.}

\references{
Brown, G.W. and Beattie, B.R. (1975). Improving Estimates of Economic Parameters by use of Ridge Regression with Production Function Applications. \emph{American Journal of Agricultural Economics}, 57(1), 21-32.
}

\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\note{Covariance will be without intercept term, as intercept term is not penalized in ridge regression.}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge VIF values \code{\link{vif}}}

\examples{
data(Hald)
mod<- lmridge(y~., data=as.data.frame(Hald), scaling="sc", K=seq(0,1,.2) )

vcov.lmridge(mod)
vcov(mod)
}

\keyword{ Ridge Variance Covariance Matrix}
