\name{press.lmridge}
\alias{press.lmridge}
\alias{press}

\title{Predicted Residual Sum of Squares}

\description{
The \code{press.lmridge} function computes predicted residual sum of squares (PRESS) (see Allen, 1971).}

\usage{press(object, \dots)
\method{press}{lmridge}(object, \dots)
}

\arguments{
  \item{object}{An object of class "lmridge".}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
All of the \code{n} leave-one-out predicted residual sum of squares is calculated by fitting full regression model by using, \eqn{\sum\frac{\hat{e}_{i,k}}{1-\frac{1}{n}-H_{ii_{R,k}}}}{\sum(\hat{e}_{i,k})(1-(1/n)-H_{ii_{R,k}})}, where \eqn{H_{ii_{R,k}}}{H_{ii_{R,k}}} is hat matrix from ridge model fit, \eqn{\hat{e_{i,k}}}{\hat{e_{i,k}}} is the ith residual at specific value of \eqn{K}{K}.
}

\value{
The \code{press.lmridge} produces a vector of PRESS or a matrix of PRESS for scalar or vector values of biasing parameter.
}

\references{
Allen, D. M. (1971). Mean Square Error of Prediction as a Criterion for Selecting Variables. \emph{Technometrics}, \strong{13}, 469-475. \doi{10.1080/00401706.1971.10488811}.

Allen, D. M. (1974). The Relationship between Variable Selection and Data Augmentation and Method for Prediction. \emph{Technometrics}, \strong{16}, 125-127. \doi{10.1080/00401706.1974.10489157}.

Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975). Ridge Regression: Some Simulation. \emph{Communication in Statistics}, \strong{4}, 105-123. \doi{10.1080/03610927508827232}.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67. \doi{10.1080/00401706.1970.10488634}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{
The ridge model fitting \code{\link{lmridge}}, ridge residual \code{\link{residuals}}, ridge predicted value \code{\link{predict}}
}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.5, 0.04))
press(mod)
}

\keyword{ ridge PRESS }
