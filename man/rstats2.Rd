\name{rstats2.lmridge}
\alias{rstats2}
\alias{rstats2.lmridge}
\alias{print.rstats2}

\title{Ordinary Ridge Regression Statistics 2}

\description{
The \code{rstats2} function computes the ordinary ridge related statistics such as \eqn{Ck}{ck}, \eqn{\sigma^2}{\sigma^2}, ridge degrees of freedom, effective degrees of freedom (EDF), and prediction residual error sum of squares PRESS statistics for scalar or vector value of biasing parameter \eqn{K}{K} (See Allen, 1974 <\doi{10.2307/1267500}>; Lee, 1979; Hoerl and Kennard, 1970 <\doi{10.2307/1267351}>).}

\usage{
   rstats2(x, \dots)
   \method{rstats2}{lmridge}(x, \dots)
   \method{print}{rstats2}(x, digits = max(5,getOption("digits") - 5), \dots)
}

\arguments{
  \item{x}{For the \code{rstats2} method, an object of class "lmridge", i.e., a fitted model.}
  \item{digits}{Minimum number of significant digits to be used.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{rstats2} function computes the ridge regression related different statistics which may help in selecting the optimal value of biasing parameter \eqn{K}{K}. If value of \eqn{K}{K} is zero then these statistics are equivalent to the relevant OLS statistics.
}

\value{Following are ridge related statistics computed for given scalar or vector value of biasing parameter \eqn{K}{K} provided as argument to \code{lmridge} or \code{lmridgeEst} function.

\item{CK}{\eqn{Ck}{Ck} similar to Mallows \eqn{Cp}{Cp} statistics for given biasing parameter \eqn{K}{K}.}
\item{dfridge}{DF of ridge for given biasing parameter \eqn{K}{K}, i.e., \eqn{Trace[Hat_{R,k}]}{Trace[Hat_{R,k}]}.}
\item{EP}{Effective number of Parameters for given biasing parameter \eqn{K}{K}, i.e., \eqn{Trace[2Hat_{R,k}-Hat_{R,k}t(Hat_{R,k})]}{Trace[2Hat_{R,k}-Hat_{R,k}t(Hat_{R,k})]}. }
\item{redf}{Residual effective degrees of freedom for given biasing parameter \eqn{K}{K} from Hastie and Tibshirani, (1990), i.e., \eqn{n-Trace[2Hat_{R,k}-Hat_{R,k}t(Hat_{R,k})]}{n-Trace[2Hat_{R,k}-Hat_{R,k}t(Hat_{R,k})]}.}
\item{EF}{Effectiveness index for given biasing parameter \eqn{K}{K}, also called the ratio of reduction in total variance in the total squared bias by the ridge regression, i.e., \eqn{EF=\frac{\sigma^2 trace(X'X)^{-1}-\sigma^2 trace(VIF_R)}{Bias^2(\hat{\beta}_R)}}{EF={\sigma^2 trace(X'X)^{-1}-\sigma^2 trace(VIF)}/{Bias^2(\hat{\beta}_R)}}.}
\item{ISRM}{Quantification of concept of stable region proposed by Vinod and Ullah, 1981, i.e., \eqn{ISRM_k=\sum_{j=1}^p (\frac{p(\frac{\lambda_j}{\lambda_j+k})^2}{\sum_{j=1}^p \frac{\lambda_j}{(\lambda_j+k)^2} \lambda_j}-1)^2}{ISRM_k=\sum_{j=1}^p ([p (\lambda_j/(\lambda_j+k))^2]/[\sum_{j=1}^p (\lambda_j /(\lambda_j+k)^2 \lambda_j)]-1)^2}.}
\item{m}{m-scale for given value of biasing parameter proposed by Vinod (1976) alternative to plotting of the ridge coefficients, i.e., \eqn{p-\sum_{j-1}^p \frac{\lambda_j}{\lambda_j+k}}{p-\sum_{j-1}^p (\lambda_j/(\lambda_j+k))}.}
\item{PRESS}{PRESS statistics for ridge regression introduced by Allen, 1971, 1974, i.e., \eqn{PRESS_k=\sum_{i=1}^n e^2_{i,-i} }{PRESS_k=\sum_{i=1}^n e^2_{i,-i}} for scalar or vector value of biasing parameter \eqn{K}{K}.}
}

\references{
Allen, D. M. (1971). Mean Square Error of Prediction as a Criterion for Selecting Variables. \emph{Technometrics}, \strong{13}, 469-475. \doi{10.1080/00401706.1971.10488811}.

Allen, D. M. (1974). The Relationship between Variable Selection and Data Augmentation and Method for Prediction. \emph{Technometrics}, \strong{16}, 125-127. \doi{10.1080/00401706.1974.10489157}.

Cule, E. and De lorio, M. (2012). A semi-Automatic method to guide the choice of ridge parameter in ridge regression.  \emph{arXiv:1205.0686v1 [stat.AP]}.

Hastie, T. and Tibshirani, R. (1990). \emph{Generalized Additive Models}. Chapman & Hall.

Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975). Ridge Regression: Some Simulation. \emph{Communication in Statistics}, \strong{4}, 105-123. \doi{10.1080/03610927508827232}.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67. \doi{10.1080/00401706.1970.10488634}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.

Kalivas, J. H., and Palmer, J. (2014). Characterizing Multivariate Calibration Tradeoffs (Bias, Variance, Selectivity, and Sensitivity) to Select Model Tuning Parameters. \emph{Journal of Chemometrics}, \strong{28}(5), 347--357. \doi{10.1002/cem.2555}.

Lee, W. F. (1979). Model Estimation Using Ridge Regression with the Variane Normalization Criterion. \emph{Master thesis, Department of Educational Foundation Memorial University of Newfoundland.}
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Ridge related statistics \code{\link{rstats1}}, ridge model fitting \code{\link{lmridge}}}

\examples{
data(Hald)
mod <- lmridge(y~., data=as.data.frame(Hald), K = seq(0,0.2, 0.001) )

rstats2(mod)

}

\keyword{ridge statistics}
\keyword{ridge PRESS}
\keyword{ridge effective degrees of freedom}

