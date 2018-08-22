\name{infocr.lmridge}
\alias{infocr}
\alias{infocr.lmridge}

\title{Model Selection Criteria for Ridge Regression}

\description{
The \code{infocr.lmridge} function computes model information selection criteria (AIC and BIC), see Akaike, 1974 <\url{https://doi.org/10.1109/TAC.1974.1100705}>; Imdad, 2017 and Schwarz, 1978 <\url{https://doi.org/10.1214/aos/1176344136}>).
}

\usage{
infocr(object, \dots)
\method{infocr}{lmridge}(object, \dots)
}

\arguments{
  \item{object}{An object of class "lmridge".}
  \item{\dots}{Not presently used in this implementation.}
}

\details{Model information selection criteria are common way of selecting among model while balancing the competing goals of fit and parsimony. The model selection criteria AIC and BIC are computed by quantifying \code{df} in the ridge regression model, using formula (\eqn{df=trace[X(X'X+kI)^{-1}X']}{trace[X(X'X+kI)^{-1}X']}). It can be helpful for selecting optimal value of biasing parameter \eqn{K}{K}.}

\value{It returns a matrix of information criteria, AIC and BIC for each biasing parameter \eqn{K}{K}. Column of matrix indicates model selection criteria AIC and BIC, respectively, while rows indicate value of biasing parameter \eqn{K}{K} for which model selection criteria are computed. }

\references{
Akaike, H. (1974). A new look at the Statistical Model Identification. \emph{IEEE Transaction on Automatic Control}, \strong{9}(6), 716-723. \href{https://doi.org/10.1109/TAC.1974.1100705}{Akaike, 1974}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Schwarz, G. (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, \strong{6}(2), 461--464. \href{https://doi.org/10.1214/aos/1176344136}{Schwarz, 1978}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{the ridge model fitting \code{\link{lmridge}}, ridge AIC and BIC plot \code{\link{info.plot}}}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, .2, 0.001))
infocr(mod)

## Vector of AIC values
infocr(mod)[,1]

## vector of BIC values
infocr(mod)[,2]
}

\keyword{ model information selection criteria }
\keyword{AIC }
\keyword{BIC}
