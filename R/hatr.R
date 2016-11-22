hatr <- function(x,...)
  UseMethod("hatr")
hatr.lmridge<- function(x, ...) {
  Z <- x$Z
  X <- x$xs
  hatr <- lapply(Z, function(x) {
    round(X %*% x, 5)
  })


  hatr
}


