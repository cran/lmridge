\name{plot.lmridge}
\alias{plot.lmridge}
\alias{plot}
\title{VIF and Ridge Trace Plot}

\description{
Plot of VIF values (VIF trace) and ridge coefficients (ridge trace) for scalar or vector values of biasing parameter \eqn{K}{K}.
}

\usage{\method{plot}{lmridge}(x, type = c("ridge", "vif"), abline = TRUE, \dots)}

\arguments{
  \item{x}{An object of class "lmridge". }
  \item{type}{Either VIF trace or ridge trace.}
  \item{abline}{Horizontal and vertical line to show minimum value of MSE and GCV value at certain value of biasing parameter \eqn{K}{K} on ridge and VIF trace respectively.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{Graphical way of selecting optimal value of biasing parameter \eqn{K}{K}. The biasing parameter is selected when coefficients becomes stable in case of ridge trace. In cae of VIF trace \eqn{K}{K} (ridge biasing parameter) can be selected for which VIF of each regressor near to one or value of \eqn{K}{K} at which GCV is minimum. If no argument is used then all traces of ridge coefficients will be displayed. A vertical and horizontal line will also be displayed on \emph{ridge trace} graph to indicate minimum ridge MSE (among the all computed ridge MSE based on provided vector of \eqn{K}{K}) along with the value of respective biasing parameter \eqn{K}{K}. For VIF trace, vetical line shows minmum GCV value at certain value of biasing parameter \eqn{K}{K}.}

\value{Nothing}

\references{
Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67.

Hoerl, A. E., Kennard, R. W., and Baldwin, K.F. (1975). A Simulation Study of Ridge and Other Regression Estimators. \emph{Communication in Statistics}, \strong{5}, 308-323.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Dr. Muhammad Imdad Ullah, Dr. Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge CV and GCV plots \code{\link{cv.plot}}, variance bias trade-off plot \code{\link{bias.plot}}, m-scale and isrm plots \code{\link{isrm.plot}}, ridge AIC and BIC plots \code{\link{info.plot}}, miscellaneous ridge plots \code{\link{rplots.plot}} }

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.15, 0.002))
## Ridge trace
plot(mod)
plot(mod, type = "ridge")

## VIF trace
plot(mod, type = "vif")
## Ridge trace without abline
plot(mod, type = "ridge", abline = FALSE)
}

\keyword{ VIF trace }
\keyword{ Ridge trace }
