\name{kest.lmridge}
\alias{kest}
\alias{kest.lmridge}
\alias{print.klmridge}

\title{Computation of Ridge Biasing Parameter \eqn{K}{K}}

\description{
The \code{kest} function computes different biasing parameters available in the literature proposed by different researchers.}

\usage{kest(object, \dots)
\method{kest}{lmridge}(object, \dots)
\method{print}{klmridge}(x, \dots)
}

\arguments{
  \item{object}{An object of class "lmridge" for the \code{kest}.}
  \item{x}{An object of class "klmridge" for the \code{print.kest.klmridge}.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{kest} function computes different biasing parameter for the ordinary linear ridge regression. All these methods are already available in the literature and proposed by various authors. See reference section.}

\value{
The function returns the list of following biasing parameter methods proposed by various researchers:

  \item{mHKB}{By Thisted (1976), \eqn{\frac{((p-2)*\hat{\sigma}^2)}{\sum(\beta^2)}}{(p-2)\hat{\sigma}^2/{(\hat{\beta}'\hat{\beta})}} }
  \item{LW}{As in \code{lm.ridge} of \code{MASS} \eqn{\frac{((p-2)*\hat{\sigma}^2*n)}{\sum(\hat{y}^2)}}{(p-2)\hat{\sigma}^2*n/\sum(\hat{y}^2) }}
  \item{LW76}{By Lawless and Wang (1976), \eqn{\frac{p*\hat{\sigma}^22}{\sum(\lambda_j*\hat{\alpha}_j^2)}}{(p\hat{\sigma}^2)/(\sum{\lambda_j*\hat{\alpha}_j^2})}}
  \item{CV}{Value of Cross Validation (CV) for each biasing parameter \eqn{K}{K}, \eqn{CV_k=\frac{1}{n}\sum_{j=1}^n (y_i-X_j \hat{\beta}_{j_K})^2 }{CV_k=1/n \sum_{j=1}^n (y_i-X_j \hat{\beta}_{j_K})^2}.}
  \item{kCV}{Value of biasing parameter at which CV is small.}
  \item{HKB}{By Hoerl and Kennard (1970), \eqn{\frac{p*\hat{\sigma}^2}{\hat{\beta}'\hat{\beta}}}{(p\hat{\sigma}^2)/(\hat{\beta}'\hat{\beta})}}
  \item{kibAM}{By Kibria (2003), \eqn{\frac{1}{p}*\sum(\frac{\hat{\sigma}^2}{\hat{\beta}_j^2)}}{1/p*(\sum{\hat{\sigma^2}/\hat{\beta}_j^2)}} }
  \item{GCV}{Value of Generalized Cross Validation (GCV) for each biasing parameter \eqn{K}{K}, \eqn{\frac{(y_i-X_j\hat{\beta}_{J_K})^2}{[n-(1+Trace(H_{R,k}))]^2}}{(\sum_{j=1}^n (y_i-\hat{\beta}_{j_K}))^2/[n-(1+Trace(H_{R,k}))]^2}.}
  \item{kcGCV}{Value of biasing parameter at which GCV is small.}
  \item{DSK}{By Dwividi and Shrivastava, (1978), \eqn{\frac{\hat{\sigma}^2}{\hat{\beta}'\hat{\beta}}}{\hat{\sigma}^2/(\hat{\beta}'\hat{\beta})}}
  \item{kibGM}{By Kibria (2003), \eqn{\frac{\hat{\sigma}^2}{(\prod(\hat{\alpha}_j^2))^(1/p)}}{(\hat{\sigma}^2)/(\prod\hat{\alpha}_j^2)^(1/p)} }
  \item{kibMEd}{By Kibria (2003), \eqn{median(\frac{\hat{\sigma}^2}{\hat{\alpha}_j^2})}{median[(\hat{\sigma}^2)/(\hat{\alpha}_j^2)]} }
  \item{KM2}{By Muniz and Kibria (2009), \eqn{max[\frac{1}{\sqrt{\frac{\hat{\sigma}^2}{\hat{\alpha}^2_j}}}]}{max[(1)/(\sqrt{(\hat{\sigma}^2)/(\hat{\alpha}_j^2)})]} }
  \item{KM3}{By Muniz and Kibria (2009), \eqn{max[\sqrt{\frac{\hat{\sigma}^2}{\hat{\alpha}_j^2}}]}{max[\sqrt{(\hat{\sigma}^2)/(\hat{\alpha}_j^2)}]}}
  \item{KM4}{By Muniz and Kibria (2009), \eqn{[\prod\frac{1}{\sqrt{\frac{\hat{\sigma}^2}{\hat{\alpha}_j^2}}}]^\frac{1}{p}}{[\prod (1/(\sqrt{(\hat{\sigma}^2)/(\hat{\alpha}_j^2)}))]^(1/p)}}
  \item{KM5}{By Muniz and Kibria (2009), \eqn{[\prod \sqrt{\frac{\hat{\sigma}^2}{\hat{\alpha}_j^2}}]^{\frac{1}{p}}}{[\prod\sqrt{(\hat{\sigma}^2)/(\hat{\alpha}_j^2)}]^{1/p}}}
  \item{KM6}{By Muniz and Kibria (2009), \eqn{Median[\frac{1}{\sqrt{\frac{\hat{\sigma}^2}{\hat{\alpha}^2_j}}}]}{Median[1/\sqrt{\hat{\sigma}^2/\hat{\alpha}^2_j}]}}
  \item{KM8}{By Muniz \emph{et al.} (2012), \eqn{max(\frac{1}{\sqrt{\frac{\lambda_{max} \hat{\sigma}^2} {(n-p)\hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j}}})}{max[1/((\lambda_{max}\hat{\sigma}^2)/((n-p)\hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j))]}}
  \item{KM9}{By Muniz \emph{et al.} (2012), \eqn{max[\sqrt{\frac{\lambda_{max}\hat{\sigma}^2}{(n-p)\hat{\sigma}^2}+\lambda_{max}\hat{\alpha}^2_j}]}{max[\sqrt{(\lambda_{max}\hat{\sigma}^2)/((n-p)\hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j)}]}}
  \item{KM10}{By Muniz \emph{et al.} (2012), \eqn{[\prod(\frac{1}{\sqrt{\frac{\lambda_{max}\hat{\sigma}^2}{(n-p)\hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j}}})]^{\frac{1}{p}}}{[\prod(1/\sqrt{(\lambda_{max}\hat{\sigma}^2)/((n-p)\hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j)}) ]^{1/p}}}
  \item{KM11}{By Muniz \emph{et al.} (2012), \eqn{[\prod(\sqrt{\frac{\lambda_{max}\hat{\sigma}^2}{(n-p) \hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j}})^{\frac{1}{p}}}{[\prod (\sqrt{(\lambda_{max}\hat{\sigma}^2)/((n-p)\hat{\sigma}^2)+\lambda_{max}\hat{\alpha}^2_j})]^(1/p)}}
  \item{KM12}{By Muniz \emph{et al.}, \eqn{Median[\frac{1}{\sqrt{\frac{\lambda_{max}\hat{\sigma}^2}{(n-p)\hat{\sigma}^2+\lambda_{max}\hat{\alpha}^2_j}}}]}{Median[(1)/(\sqrt{(\lambda_{max}\hat{\sigma}^2)/((n-p)\hat{\sigma}^2)+\lambda_{max}\hat{\alpha}^2_j})]}}
  \item{KD}{By Dorugade and Kashid (2012), \eqn{0, \frac{p\hat{\sigma}^2}{\hat{\alpha}'\hat{\alpha}}-\frac{1}{n(VIF_j)_{max}}}{max[0, (p\hat{\sigma}^2)/(\hat{\alpha}'\hat{\alpha})-(1)/(n(VIF_j)_{max})]}}

\item{KAD4}{By Dorugade and Kashid (2012), \eqn{HM[\frac{2p}{\lambda_{max}} \sum(\frac{\hat{\sigma}^2}{\hat{\alpha}^2_j})]}{HM[2p/\lambda_{max}\sum(\hat{\sigma}^2 / \hat{\alpha}^2_j)]}}


  \item{alphahat}{The OLS estimator in canonical form, i.e., \eqn{\hat{\alpha}=(P'X'XP)^{-1}X'^*y}{\hat{\alpha}=(P'X'XP)^{-1}X'^*y}, where \eqn{X^*=XP}{X^*=XP} \eqn{P}{P} is eigenvector of \eqn{X'X}{X'X}.}
  %\item{P}{ \eqn{P} is eigenvector of \eqn{X'X}}
  %\item{xstar}{}
%  \item{EV}{Eigenvalue for X'X matrix}
}
\references{
Dorugade, A. and Kashid, D. (2010). Alternative Method for Choosing Ridge Parameter for Regression. \emph{Applied Mathematical Sciences}, \strong{4}(9), 447-456.

Dorugade, A. (2014). New Ridge Parameters for Ridge Regression. \emph{Journal of the Association of Arab Universities for Basic and Applied Sciences}, \strong{15}, 94-99.

Hoerl, A.E., Kennard, R.W., and Baldwin, K.F. (1975). A Simulation Study of Ridge and Other Regression Estimators. \emph{Communication in Statistics}, \strong{5}, 308-323.

Hoerl, A.E. and Kennard, R.W., (1970). Ridge Regression: Biased Estimation of Nonorthogonal Problems. \emph{Technometrics}, \strong{12}, 55-67.

Kibria, B. (2003). Performance of Some New Ridge Regression Estimators. \emph{Communications in Statistics-Simulation and Computation}, \strong{32}(2), 491-435.

Lawless, J., and Wang, P. (1976). A Simulation Study of Ridge and Other Regression Estimators. \emph{Communications in Statistics-Theory and Methods}, \strong{5}(4), 307-323.

Muniz, G., and Kibria, B. (2009). On Some Ridge Regression Estimators: An Empirical Comparisons. \emph{Communications in Statistics-Simulation and Computation}, \strong{38}(3), 621-630.

Muniz, G., Kibria, B., Mansson, K., and Shukur, G. (2012). On developing Ridge Regression Parameters: A Graphical Investigation. \emph{SORT-Statistics and Operations Research Transactions}, \strong{36}(2), 115--138.

Thisted, R. A. (1976). Ridge Regression, Minimax Estimation and Empirical Bayes Methods. \emph{Technical Report 28, Division of Biostatistics}, Stanford University, California.

Venables, W.N. and Ripley, B.D. (2002). \emph{Modern Applied Statistics with S}. Springer New York, 4th edition, ISBN 0-387-95457-0.

}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{The ridge model fitting \code{\link{lmridge}}, Ridge Var-Cov matrix \code{\link{vcov}}}

\examples{
mod <- lmridge(y~., as.data.frame(Hald), K=seq(0, 0.2, 0.001))
kest(mod)

## GCV values
kest(mod)$GCV

## minimum GCV value
kest(mod)$kGCV

## Hoerl and Kennard (1970)
kest(mod)$HKB

}
\keyword{ Ridge Biasing Parameter }
