\name{info.plot}
\alias{info.plot}

\title{Model Selection Criteria Plots}

\description{Plot of ridge AIC and BIC model selection criteria against ridge degrees of freedom (see Akaike, 1974 <\url{https://doi.org/10.1109/TAC.1974.1100705}>; Imdad, 2017 and Schwarz, 1978 <\url{https://doi.org/10.1214/aos/1176344136}>).}

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

Akaike, H. (1974). A new look at the Statistical Model Identification. \emph{IEEE Transaction on Automatic Control}, \strong{9}(6), 716--723. \href{https://doi.org/10.1109/TAC.1974.1100705}{Akaike, 1974}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Schwarz, G. (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, \strong{6}(2), 461--464. \href{https://doi.org/10.1214/aos/1176344136}{Schwarz, 1978}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge CV and GCV plot\code{\link{cv.plot}}, variance biase trade-off plot \code{\link{bias.plot}}, m-scale and isrm plots \code{\link{isrm.plot}}, ridge and VIF trace \code{\link{plot.lmridge}}, miscellaneous ridge plots \code{\link{rplots.plot}} }

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.15, 0.002))
## for indication vertical line (df ridge)
info.plot(mod)

## without vertical line set \code{abline = FALSE}
info.plot(mod, abline = FALSE)
}

\keyword{ model selection criteria }
\keyword{ ridge regression }
\keyword{ ridge degrees of freedom }
