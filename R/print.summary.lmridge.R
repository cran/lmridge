print.summary.lmridge <-
  function(x, digits = max(3, getOption("digits") - 3),
           signif.stars = getOption("show.signif.stars"), ...) {
    summaries <- x$summaries
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
          "")
    # cat("\nCoefficients:\n")
    for (i in seq(length(summaries))) {
      CSummary <- summaries[[i]]

      cat("\nCoefficients: for Ridge parameter K=", CSummary$K, "\n")
      coefs <- CSummary$coefficients
      printCoefmat(
        coefs,  digits = digits, signif.stars = signif.stars, P.values = TRUE,
        has.Pvalue = TRUE, na.print = "NA", ...
      )
      #printCoefmat(coefs, P.values=TRUE, has.Pvalue=TRUE, cs.ind=coefs[[5]])
      #cat("\nR-Square: ", format(signif(x$R2r[[i]],digits)),"\n")
      cat("\nRidge Summary\n")
      print(drop(CSummary$stats))
      cat("Ridge minimum MSE=", CSummary$rmse1, "at K=", CSummary$rmse2, "\n")
      cat("-------------------------------------------------------------------\n\n")
      invisible(x)
    }
  }
