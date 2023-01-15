\name{hatr.lmridge}
\alias{hatr}
\alias{hatr.lmridge}

\title{Ridge Regression: Hat Matrix}

\description{
The \code{hatr} function computes hat matrix (see Hastie and Tibshirani, 1990).}

\usage{
hatr(x, \dots)
\method{hatr}{lmridge}(x, \dots)
}

\arguments{
  \item{x}{An object of class "lmridge".}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
Hat matrix for scalar or vector values of biasing parameter provided as argument to \code{lmridge}. It is used to compute degrees of freedom for given \eqn{K}{K}, and error degree of freedom etc. The hat matrix can be computed using formula \eqn{X(X'X+kI)^{-1}X'}{X(X'X+kI)^{-1}X'} equivalently \eqn{\sum{\frac{\lambda_j}{(\lambda_j+k)}}}{\sum((\lambda_j)/(\lambda_j+k))}. }

\value{
returns a list of matrix for each biasing parameter \eqn{K}{K}:
\item{hatr}{A list of hat matrix for each biasing parameter \eqn{K}{K}}. }

\references{
Cule, E. and De lorio, M. (2012). A semi-Automatic method to guide the choice of ridge parameter in ridge regression.  \emph{arXiv:abs/1205.0686v1 [stat.AP]}.

Hastie, T. and Tibshirani, R. (1990). \emph{Generalized Additive Models}. Chapman and Hall.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge Var-Cov matrix \code{\link{vcov.lmridge}}}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = c(0, 0.1, 0.2, 0.3))
## Hat matrix for each biasing parameter
hatr(mod)

## Hat matrix for first biasing parameter i.e. K = 0.1
hatr(mod)[[2]]
}

\keyword{ ridge regression}
\keyword{ ridge hat matrix }
