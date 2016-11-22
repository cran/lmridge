#residuals<-function(object,...) UseMethod("residuals")
residuals.lmridge <- function(object, ...) {
  #cat("Reaiduals: ridge regression\n")
  y <- object$y
  rfitted <- object$rfit
  resid <- apply(rfitted,2, function(x) {
    y - x
  })
  resid
}

 # print.residuals.lmridge <- function(object,...) {
 #   cat("Call:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), "\n\n", sep ="")
 #   cat("Reaiduals: ridge regression\n")
 #   print(resid(object),...)
 #   cat("\n")
 #   invisible(object)
 # }
