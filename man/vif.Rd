\name{vif.lmridge}
\alias{vif}
\alias{vif.lmridge}

\title{Variance Inflation Fator for Linear Ridge Regression}

\description{Computes VIF values for each scalar or vector value of biasing parameter \eqn{K}{K}.}

\usage{vif(x, \dots)
\method{vif}{lmridge}(x, \dots)
}

\arguments{
  \item{x}{For VIF method, an object of class "lmridge", i.e., a fitted model.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{vif.lmridge} function computes VIF value for each regressor in data set after addition of biasing parameter as argument to \code{lmridge} function. The VIF is computed using \eqn{(X'X+kI)^{-1}X'X(X'X+kI)^{-1}}{(X'X+kI)^{-1}X'X(X'X+kI)^{-1}}, given by Marquardt, (1970).
}

\value{
The \code{vif} function returns a matrix of VIF values for each regressor after adding scalar or vector biasing parameter \eqn{K}{K} to \eqn{X'X}{X'X} matrix. The column of returned matrix indicates regressors name and row indicates value of each biasing parameter \eqn{K}{K} provided as argument to \code{lmridge} function.
}

\references{
Fox, J. and Monette, G. (1992). Generalized Collinearity Diagnostics. \emph{JASA}, \strong{87}, 178--183.

Marquardt, D. (1970). Generalized Inverses, Ridge Regression, Biased Linear Estimation, and Nonlinear Estimation. \emph{Technometrics}, \strong{12}(3), 591--612.

}

\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge Var-Cov matrix \code{\link{vcov}} }

\examples{
data(Hald)
mod <- lmridge(y~., data=as.data.frame(Hald), scaling="sc", K=seq(0,1,.2) )
vif(mod)
}

\keyword{ Variance Inflation Factor }
