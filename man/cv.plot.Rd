\name{cv.plot}
\alias{cv.plot}

\title{Ridge CV and GCV Plot}

\description{Plot of ridge CV and GCV against scalar or vector values of biasing parameter \eqn{K}{K} (see Golub et al., 1979).}

\usage{cv.plot(x, abline = TRUE, \dots)}
\arguments{
  \item{x}{An object of class "lmridge".}
  \item{abline}{Horizontal and vertical lines to show minimum value of ridge GCV and CV at certain value of biasing parameter \eqn{K}{K}.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
Function \code{cv.plot} can be used to plot the values of ridge CV and GCV against scalar or vector value of biasing parameter \eqn{K}{K}. The \code{cv.plot} can be helpful for selection of optimal value of ridge biasing parameter \eqn{K}{K}. If no argument is used then horizontal line will indicate minimum GCV and Cv at certain value of biasing parameter \eqn{K}{K}.
}

\value{Nothing returned}

\references{
Delaney, N. J. and Chatterjee, S. (1986). Use of the Bootstrap and Cross-Validation in Ridge Regression. \emph{Journal of Business & Economic Statistics}. \strong{4}(2), 255--262.

Golub, G., Wahba, G. and Heat, C. (1979). Generalized Cross Validation as a Method for Choosing a Good Ridge Parameter. \emph{Technometrics.} \strong{21}, 215--223.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Dr. Muhammad Imdad Ullah, Dr. Muhammad Aslam}

\seealso{
The ridge model fitting \code{\link{lmridge}}, bias variance trade-off plot \code{\link{bias.plot}}, ridge AIC and BIC plots \code{\link{info.plot}}, m-scale and isrm plots \code{\link{isrm.plot}}, ridge and VIF trace \code{\link{plot.lmridge}}, miscellaneous ridge plots \code{\link{rplots.plot}} }

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.2, 0.001))
## for indication vertical line (biasing parameter k) and
## horizontal line (minimum respective CV and GCV values corresponding to vertical line)
cv.plot(mod)

## without Horizontal and vertical line set \code{abline = FALSE}
cv.plot(mod, abline = FALSE)
}

\keyword{ Ridge Cross Validation Criteria }
\keyword{ Ridge CV }
\keyword{ Ridge GCV }
