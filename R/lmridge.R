lmridge <- function(formula, data, K = 0,
                    scaling=c("sc", "scaled", "centered"), ...)
  UseMethod("lmridge")
lmridge.default <- function(formula, data,K = 0,
                            scaling=c("sc", "scaled", "centered"), ...) {
  #    x<-as.matrix(x)
  #    y<-as.matrix(y)

  est <- lmridgeEst(formula, data, K, scaling=scaling, ...)

  #est$fitted.values <- as.vector(est$xs %*% est$coef)
  #est$residuals <- as.vector(est$y - est$fitted.values)

  est$call <- match.call()

  class(est) <- "lmridge"
  est
}
