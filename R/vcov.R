#vcov <- function(x,...)  UseMethod("vcov")
vcov.lmridge <- function(object,...) {
  #Z<-object$vcov
  resid <- residuals(object)
  n <- nrow(object$xs)

  redf <- lapply(hatr(object), function(x) {
    n - sum(diag(2 * x - x %*% t(x)))
  })

  rsigma2 <-
    mapply(function(x,y) {
      x / y
    }, colSums(resid ^ 2), redf, SIMPLIFY = FALSE)

  ZZt <- lapply(object$Z, function(x) {
    x %*% t(x)
  })

  vcovbr <- mapply(function(x, y) {
    x * y
  },rsigma2,ZZt, SIMPLIFY = FALSE)
  #vcovbr<-mapply(function(x,y){x*y},object$sigma2,ZZt, SIMPLIFY = FALSE)

  vcovbr
}
