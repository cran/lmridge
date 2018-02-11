\name{bias.plot}
\alias{bias.plot}

\title{Bias Variance and MSE Trade-off Plot}

\description{
Trade-off between bias, variance and MSE of the linear ridge regression against vector or scalar value of biasing parameter \eqn{K}{K} (see Kalivas and Palmer, 2014).}

\usage{bias.plot(x, abline = TRUE, \dots)}

\arguments{
  \item{x}{An object of class "lmridge".}
  \item{abline}{Horizontal and vertical lines show the minimum value of the ridge MSE at certain value of biasing parameter \eqn{K}{K}.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The effect of multicollinearity on the coefficient estimates can be identified using different graphical display. One of them is plot of bias, variance and MSE. A little addition of bias lead to a substantial decrease in variance, and MSE. Therefore, a trade-off is made between bias and variance to have acceptable MSE. The \code{bias.plot} can be helpful for selection of optimal value of biasing parameter \eqn{K}{K}.}

\value{Nothing returned}

\references{
Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Kalivas, J. H., and Palmer, J. (2014). Characterizing multivariate calibration tradeoffs (bias, variance, selectivity, and sensitivity) to select model tuning parameters. \emph{Journal of Chemometrics}, \strong{28}(5), 347--357.
}

\author{Dr. Muhammad Imdad Ullah, Dr. Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge CV and GCV plots \code{\link{cv.plot}}, ridge AIC and BIC plots \code{\link{info.plot}}, m-scale and isrm plots \code{\link{isrm.plot}}, ridge and VIF trace \code{\link{plot.lmridge}}, miscellaneous ridge plots \code{\link{rplots.plot}} }

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.3, 0.001))
## for indication vertical line (biasing parameter k) and
## horizontal line (minimum minimum ridge MSE values corresponding to vertical line)
bias.plot(mod)

## without Horizontal and vertical line as set \code{abline = FALSE}
bias.plot(mod, abline=FALSE)
}

\keyword{Ridge MSE}
\keyword{Ridge Bias}
\keyword{Ridge Variance}
