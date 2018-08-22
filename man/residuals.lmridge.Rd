\name{residuals.lmridge}
\alias{residuals.lmridge}
\alias{residuals}

\title{Ridge Regression Residuals}

\description{The \code{residuals} function computes the ridge residuals for scalar or vector value of biasing parameter \eqn{K}{K}.}

\usage{\method{residuals}{lmridge}(object, \dots)}

\arguments{
  \item{object}{An object of class "lmridge".}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The generic functions \code{residuals} can be used to compute residuals object of linear ridge regression from \code{lmridge} function.
}

\value{
Returns a vector or a matrix of ridge residuals for scalar or vector value biasing parameter \eqn{K}{K} provided as argument to \code{lmridge} function.
}

\references{
Berk, R. (2008). \emph{Statistical Learning from a Regression Perspective.} Springer.

Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975). Ridge Regression: Some Simulation. \emph{Communication in Statistics}, \strong{4}, 105-123. \href{https://doi.org/10.1080/03610927508827232}{Hoer et al., 1975}.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67. \href{http://doi.org/10.2307/1267351}{Hoerl and Kennard, 1970}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Lee, W. F. (1979). Model Estimation Using Ridge Regression with the Variance Normalization Criterion. \emph{Master thesis, Department of Educational Foundation, Memorial University of Newfoundland.}
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{
The ridge mode fitting \code{\link{lmridge}}, ridge prediction \code{\link{predict}}, ridge PRESS values \code{\link{press}}}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 1, 0.2))
residuals(mod)
}

\keyword{ ridge residuals }
