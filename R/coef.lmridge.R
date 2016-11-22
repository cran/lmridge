coef.lmridge <- function(object, ...) {
  ym<-mean(object$mf[,1])
  scaledcoef <- t(as.matrix(object$coef / object$xscale))
  if (object$Inter) {
    inter <- ym - scaledcoef %*% object$xm
    scaledcoef <- cbind(Intercept = inter, scaledcoef)
    colnames(scaledcoef)[1] <- "Intercept"
  }else{
    scaledcoef <- t(as.matrix(object$coef / object$xscale))
  }

  drop(scaledcoef)
}
