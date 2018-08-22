print.lmridge <- function(x, digits = max(5, getOption("digits") - 5),...) {
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
        "")
  print(round(coef(x),  digits), ...)
  cat("\n")
  invisible(x)

}
