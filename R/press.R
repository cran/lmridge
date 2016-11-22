press <- function(object,...)
  UseMethod("press")
press.lmridge <- function(object,...) {
  res <- resid(object)
  n <- nrow(object$xs)
  K <- object$K

  diaghat <- lapply(hatr(object), function(x) {
    diag(x)
  })
  diaghat <- do.call(cbind, diaghat)

  #pres<-lapply(res,function(x){x/(1-diaghat)})
  pres <-
    lapply(1:length(K), function(i, res, diaghat) {
      res[,i] / (1 - (1 / n) - diaghat[,i])
    }
    , res = res, diaghat = diaghat)
  pres <- do.call(cbind,pres)
  colnames(pres) <- paste("K=", K, sep = "")
  pres
}
