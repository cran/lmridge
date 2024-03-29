\name{summary.lmridge}
\alias{summary.lmridge}
\alias{print.summary.lmridge}

\title{Summarizing Linear Ridge Regression Fits}

\description{The \code{summary} method for class "lmridge" for scalar or vector biasing parameter \eqn{K}{K} (Cule and De lorio, 2012).}

\usage{
\method{summary}{lmridge}(object, \dots)
\method{print}{summary.lmridge}(x, digits = max(3, getOption("digits") - 3),
           signif.stars = getOption("show.signif.stars"), \dots)
}

\arguments{
  \item{object}{An "lmridge" object, typically generated by a call to \code{lmridge}.}
  \item{x}{An object of class \code{summary.lmridge} for the \code{print.summary.lmridge}.}
  \item{signif.stars}{logical: if \code{TRUE}, \emph{p}-values are additionally encoded visually as  \code{significance starts} in order to help scanning of long coefficient tables. It default to the \code{show.signif.stars} slot of \code{options}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{\code{print.summary.lmridge} tries to be smart about formatting the coefficients, standard errors etc. and additionally gives 'significance stars' if \code{signif.stars} is \code{TRUE}.}

\value{
The function \code{summary} computes and returns a list of summary statistics of the fitted linear ridge regression model for scalar or vector value biasing parameter \eqn{K}{K} given as argument in \code{lmridge} function. All summary information can be called using list object \code{summaries}.
\item{coefficients}{A \eqn{p \times 5}{p * 5} matrix with columns for the scaled estimated, descaled estimated coefficients, scaled standard error, scaled \emph{t}-statistics, and corresponding \emph{p}-value (two-tailed). The Intercept term is computed by the relation \eqn{\hat{\beta}_{R_{0K}}=\overline{y}-\sum_{j=1}^{p}\overline{X}_j \hat{\beta}_{R_{0K}}}{\hat{\beta}_{R0K}=ybar-\sum_{j=1}^p(Xbar_j \hat{\beta}_{R0K})}. The standard error of intercept term is computed as, \eqn{SE(\hat{\beta}_{R_{0K}})=\sqrt{Var(\overline{y}) +\overline{X}_j^2 diag[Cov(\hat{\beta}_{R_{0K}})]}}{SE(\hat{\beta}_{R_{0K}})=\sqrt{Var(ybar)+Xbar_j^2 diag[Cov(\hat{\beta}_{R})]}}.}
\item{stats}{Ridge related statistics of \emph{R}-squared, adjusted \emph{R}-squared, \emph{F}-statistics for testing of coefficients, AIC and BIC values for given biasing parameter \eqn{K}{K}.}
\item{rmse1}{Minimum MSE value for given biasing parameter \eqn{K}{K}.}
\item{rmse2}{Value of \eqn{K}{K} at which MSE is minimum.}
\item{K}{Value of given biasing parameter.}
\item{df1}{Numerator degrees of freedom for p-value of F-statistics.}
\item{df2}{Denominator degrees of freedom for p-value of F-statistics.}
\item{fpvalue}{p-value for each F-statistics.}
}

\references{

Cule, E. and De lorio, M. (2012). A semi-Automatic method to guide the choice of ridge parameter in ridge regression.  \emph{arXiv:1205.0686v1 [stat.AP]}.

Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975). Ridge Regression: Some Simulation. \emph{Communication in Statistics}, \strong{4}, 105-123. \doi{10.1080/03610927508827232}.

Hoerl, A. E. and Kennard, R. W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67. \doi{10.1080/00401706.1970.10488634}.

Imdad, M. U. \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan), 2017.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K = c(0, 0.0132, 0.1))
summary(mod)

## coefficients for first biasing parameter
summary(mod)$summaries[[1]]$coefficients
summary(mod)$summaries[[1]][[1]]

## ridge related statistics from summary function
summary(mod)$summaries[[1]]$stats

## Ridge F-test's p-value
summary(mod)$summaries[[1]]$fpvalue
}

\keyword{testing of ridge coefficient}
