\name{info.plot}
\alias{info.plot}

\title{Model Selection Criteria Plots}

\description{Plot of ridge AIC and BIC model selection criteria against ridge degrees of freedom.}

\usage{info.plot(x, abline = TRUE, \dots)}

\arguments{
  \item{x}{An object of class "lmridge".}
  \item{abline}{Vertical line to show minimum value of ridge MSE at certain value of ridge degrees of freedom.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
Plot of ridge AIC and BIC against ridge degress of freedom \eqn{\sum_{j=1}^p \frac{\lambda_j}{\lambda_j+k}}{\sum_{j=1}^p (\lambda_j)/(\lambda_j+k)}. A vertical line represents the minimum ridge MSE at certain value of ridge df.}

\value{Nothing returned}

\references{

Akaike, H. (1974). A new look at the Statistical Model Identification. \emph{IEEE Transaction on Automatic Control}, \strong{9}(6), 716--723.

Schwarz, G. (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, \strong{6}(2), 461--464.
}

\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge CV and GCV plot\code{\link{cv.plot}}, variance biase trade-off plot \code{\link{bias.plot}}, m-scale and isrm plots \code{\link{isrm.plot}}, ridge and VIF trace \code{\link{plot.lmridge}}, miscellaneous ridge plots \code{\link{rplots.plot}} }

\examples{
mod<- lmridge(y~., as.data.frame(Hald), K=seq(0, 0.2, 0.001))
## for indication vertical line (df ridge)
info.plot(mod)

## without vertical line set \code{abline=FALSE}
info.plot(mod, abline=FALSE)
}

\keyword{ Model Selection Criteria }
\keyword{ Ridge Regression }
\keyword{ Ridge degrees of freedom }
