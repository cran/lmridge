\name{Hald}
\alias{Hald}

\docType{data}

\title{Portland Cement benchmark of Hald(1952)}

\description{Heat evolved during setting of 13 cement mixtures of four basic ingredients.  Each
 ingredient percentage appears to be rounded down to a full integer.  The sum of the four mixture percentages varies from a maximum of 99\% to a minimum of 95\%.  If all four regressor
 X-variables always summed to 100\%, the centered X-matrix would then be of rank only 3.  Thus, the regression of heat on four X-percentages is ill-conditioned, with an approximate rank deficiency of MCAL = 1. }

\usage{data(Hald)}

\details{The (lmridge) Hald data are identical to the (MASS) cement data except for
 variable names.}

\format{
  A data frame with 13 observations on the following 5 variables.
  \describe{
    \item{\code{X1}}{p3ca: Integer percentage of 3CaO.Al2O3 in the mixture.}
    \item{\code{X2}}{p3cs: Integer percentage of 3CaO.SiO2 in the mixture.}
    \item{\code{X3}}{p4caf: Integer percentage of 4CaO.Al2O3.Fe2O3 in the mixture.}
    \item{\code{X4}}{p2cs: Integer percentage of 2CaO.SiO2 in the mixture.}
    \item{\code{y}}{hear: Heat (cals/gm) evolved in setting, recorded to nearest tenth.}
  }
}

\source{
Woods, H., Steinour, H.H. and Starke, H.R. (1932). Effect of Composition of Portland Cement on Heat Evolved During Hardening. \emph{Industrial Engineering and Chemistry} \strong{24}: 1207--1214.
}

\references{
Hald, A. (1952). \emph{Statistical Theory with Engineering Applications}.(page 647.) New York; Wiley.
}

\keyword{datasets}
