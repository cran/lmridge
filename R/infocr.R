infocr <- function(object, ...)
  UseMethod("infocr")

infocr.lmridge <- function(object,...) {
  #AIC for Ridge
  SSER <- apply(resid(object), 2, function(x) {
    sum(x ^ 2)
  })
  df <- rstats2(object)$dfridge
  n <- nrow(object$xs)
  #AIC<-lapply(df, function(x){n*log(SSER)+2*x})
  #rsigma2<-mapply(function(x,y){x/y}, colSums(resid^2), df, SIMPLIFY = FALSE )
  AIC <-
    mapply(function(x,y) {
      n * log(x / n) + 2 * (y)
    }, SSER,df, SIMPLIFY = FALSE)
  AIC <- do.call(cbind, AIC)

  #BIC for Ridge
  #BIC<-lapply(df, function(x){n*log(SSER)+x*log(n)})
  BIC <-
    mapply(function(x,y) {
      n * log(x) + y * log(n)
    }, SSER, df, SIMPLIFY = FALSE)
  BIC <- do.call(cbind, BIC)
  resinfo <- rbind(AIC, BIC)
  rownames(resinfo) <- c("AIC", "BIC")
  t(resinfo)

}
