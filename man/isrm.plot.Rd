\name{isrm.plot}
\alias{isrm.plot}

\title{ISRM and m-scale Plot}
\description{Plot of m-scale and ISRM against scalar or vector values of biasing parameter \eqn{K}{K} (Vinod, 1976 <\doi{10.1080/01621459.1976.10480955}>).
}

\usage{isrm.plot(x, \dots)}

\arguments{
  \item{x}{An object of class "lmridge".}
  \item{\dots}{Not presently used in this implementation.}}

\details{
The \code{isrm.plot} function can be used to plot the values of m-scale and ISRM against given list (scalar or vector values) of biasing parameter  \eqn{K}{K} as argument to \code{lmridge}. It can be helpful for the optimal selection of the biasing parameter  \eqn{K}{K}.
}

\value{Nothing returned}

\references{
Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Vinod, H. (1976). Application of New Ridge Regression Methods to a Study of Bell System Scale Economics. \emph{Journal of the American Statistical Association}, \strong{71}, 835--841. \doi{10.2307/2286847}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, ridge CV and GCV plots \code{\link{cv.plot}}, ridge AIC and BIC plots \code{\link{info.plot}}, variance bias trade-off plot \code{\link{bias.plot}}, ridge and VIF trace \code{\link{plot.lmridge}}, miscellaneous ridge plots\code{\link{rplots.plot}} }

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.2, 0.002))

isrm.plot(mod)
isrm.plot(mod, abline=FALSE)
}
\keyword{ ISRM }
\keyword{ m-scale }
