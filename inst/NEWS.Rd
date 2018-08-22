\name{NEWS}
\title{News for Package 'lmrdige'}

\section{Changes in lmrdige version 1.2 (2018-08-20)}{
\subsection{Improvements/ Updates}{
  \itemize{
    \item In \code{rstat2(0)} EDF is chanages to EP.
    \item P-value for ridge F-statistics is computed and displayed in summary statistics.
    \item Information about EP, df1, df2 are added in help file of \code{summary()}.
    \item Appropriate rounding of numbers is done in \code{summary(), lmridge(), vif()} functions.
  }
}
}

\section{Changes in lmridge version 1.1 (2018-02-10)}{
  \subsection{Improvements}{
    \itemize{
      \item Arugements of \code{legend()} in all plots are modified for further improvement.
      \item Help file for \code{summary()} and \code{kest()}  is improved.
      \item The \code{y}-axis label removed from \code{bias.plot()} function.
    }
  }
  \subsection{Corrections}{
  \itemize{
    \item Some correction is made in standard error of the intercept term. Therefore, \code{t}-value for intercept (first value of fourth column) and \code{p}-value (first value of fifth column) is updated accordingly.
  \item Correction is made in calculation of CV and GCV methods in \code{kest()} fucntion.

  }
}
}
