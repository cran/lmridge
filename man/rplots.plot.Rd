\name{rplots.plot}
\alias{rplots.plot}

\title{Miscellaneous Ridge Plots}

\description{Panel of three ridge related plots, df trace vs \eqn{K}{K}, RSS vs \eqn{K}{K} and PRESS vs \eqn{K}{K} for graphical judgement of optimal value of \eqn{K}{K}.}

\usage{rplots.plot(x, abline = TRUE, \dots)}

\arguments{
  \item{x}{An object of class "lmridge"}
  \item{abline}{Vertical line to show minimum value of ridge PRESS at cartain value of biasing parameter \eqn{K}{K} on PRESS vs \eqn{K}{K} plot.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
Function \code{rplots.plot} can be used to plot the values of df vs \eqn{K}{K}, RSS vs \eqn{K}{K} and PRESS vs \eqn{K}{K} for scalar or vector values of biasing parameter \eqn{K}{K}. If no argument is used then a vertical line will be drawn on ridge PRESS plot to show the minimum value of PRESS at certain \eqn{K}{K}. The panel of these three plots can be helful in selecting the optimal value of biasing parameter \eqn{K}{K}.
}

\value{nothing}

\references{
Allen, D. M. (1971). Mean Square Error of Prediction as a Criterion for Selecting Variables. \emph{Technometrics}, \strong{13}, 469-475.

Allen, D. M. (1974). The Relationship between Variable Selection and Data Augmentation and Method for Prediction. \emph{Technometrics}, \strong{16}, 125-127.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67.

Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975). A Simulation Study of Ridge and Other Regression Estimators. \emph{Communication in Statistics}, \strong{5}, 308-323.
Berk, R. (2008). \emph{Statistical Learning from a Regression Perspective.} Springer.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Dr. Muhammad Imdad Ullah, Dr. Muhammad Aslam}

\seealso{
The ridge model fitting \code{\link{lmridge}}, ridge CV and GCV plots \code{\link{cv.plot}}, variance bias trade-off plot \code{\link{bias.plot}}, m-scale and isrm plots \code{\link{isrm.plot}}, ridge AIC and BIC plots \code{\link{info.plot}}, ridge and VIF trace \code{\link{plot.lmridge}} }

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.2, 0.005))
rplots.plot(mod)
rplots.plot(mod, abline = FALSE)
}

\keyword{PRESS}
\keyword{df trace}
\keyword{ridge RSS }
