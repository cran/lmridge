print.lmridge <- function(x, digits = max(5,getOption("digits") - 5),...) {
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
        "")
  print(coef(x),...)
  cat("\n")
  invisible(x)

}
