\name{predict.lmridge}
\alias{predict}
\alias{predict.lmridge}

\title{Predict method for Linear Ridge Model Fits}

\description{
Predicted values based on linear ridge regression model for scalar or vector values of biasing parameter \eqn{K}{K}.}

\usage{\method{predict}{lmridge}(object, newdata, na.action=na.pass, \dots)}

\arguments{
  \item{object}{An object of class "lmridge".}
  \item{newdata}{An optional data frame in which to look for variables with which to predict.}
  \item{na.action}{Function determine what should be done with missing values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{predict.lmridge} function produces predicted values, obtained by evaluating the regression function in the frame \code{newdata} which defaults to model.frame (\code{object}). If \code{newdata} is omitted the predictions are based on the data used for the fit. In that case how cases with missing values in the original fit are handled is determined by the \code{na.action} argument of that fit. If \code{na.action = na.omit} omitted cases will not appear in the predictions, whereas if \code{na.action = na.exclude} they will appear (in predictions), with value NA.}

\value{
\code{predict.lmridge} produces a vector of predictions or a matrix of predictions for scalar or vector values of biasing parameter.
}

\references{
Cule, E. and De lorio, M. (2012). A semi-Automatic method to guide the choice of ridge parameter in ridge regression.  \emph{arXiv:1205.0686v1 [stat.AP]}.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67.

Hoerl, A. E., Kennard, R. W., and Baldwin, K.F. (1975). A Simulation Study of Ridge and Other Regression Estimators. \emph{Communication in Statistics}, \strong{5}, 308-323.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Dr. Muhammad Imdad Ullah, Dr. Muhammad Aslam}

\note{
Variables are first looked for in \code{newdata} and then are searched for in the usual way (which will include the environment of the formula used in the fit). A warning will be given if the variables found are not of the same length as those in the \code{newdata} if it was supplied.
}


\seealso{The ridge model fitting \code{\link{lmridge}}, ridge residuals \code{\link{residuals}}, ridge PRESS \code{\link{press.lmridge}}}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = seq(0, 0.2, 0.05))
predict(mod)
predict(mod, newdata = as.data.frame(Hald[1:5, -1]))
}

\keyword{ Ridge Model Predictions }
