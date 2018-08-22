rstats2 <- function(x,...)
  UseMethod("rstats2")
rstats2.lmridge <- function(x,...) {
  K <- x$K
# x <- obj$xs
  y <- x$y
  n <- nrow(x$xs)
  p <- ncol(x$xs)

  eigval <- eigen(t(x$xs) %*% x$xs)$values

  #df ridge
  dfridge <- as.vector(lapply(hatr(x), function(x) {
    sum(diag(x))
  }))

  dfridge <- do.call(rbind, dfridge)
  rownames(dfridge) <- paste("K=", K,sep = "")
  colnames(dfridge) <- c("df ridge")

# Effective df
#  EDF<-lapply(hatr(x), function(x){
#    sum(diag(2*x-x%*%t(x)))
#  })
#  EDF <- do.call(rbind,EDF)
#  rownames(EDF) <- paste("K=", K, sep = "")
#  colnames(EDF) <- c("EDF")

  #residual effective degree of freedom (Hastie and Tibshirani, 1990)
  redf <- lapply(hatr(x), function(x) {
    n - sum(diag(2 * x - x %*% t(x)))
  })
  redf <- do.call(rbind,redf)
  rownames(redf) <- paste("K=", K, sep = "")
  colnames(redf) <- c("REDF")

  sigma2 <- sum(lm.fit(x$xs,y)$residuals ^ 2) / (n - p)


  #calculations for Ridge variance
  rvarcal <-
    lapply(K, function(lam) {
      sum(eigval / (eigval + rep(lam,each = p))^2 )
    })
  rvarcal <- do.call(rbind,rvarcal)

  #Effectivness Index (EF)
  #EF <- apply(rfact,  2, function(x){(sigma2 *(sum(1/eigval) - x))/bias2})
  EF <- sigma2 * (sum(1 / eigval) - rvarcal) / rstats1(x)$bias2
  EF <- replace(EF, is.na(EF), 0)
  rownames(EF) <- paste("K=", K, sep = "")
  colnames(EF) <- c("EF")

  SSER <- apply(resid(x), 2, function(x) {
    sum(x ^ 2)
  })

# Computation of ISRM

  rfact_isrm <-
    lapply(K, function(lam) {
      eigval / (eigval + rep(lam,each = p))
    })

    ISRM <- lapply(1:length(K), function(i,num, denum) {
    sum(((p * num[[i]] ^ 2) / sum(denum[[i]] %*% eigval) - 1) ^ 2)
  }, num = rfact_isrm, denum = rvarcal)
  ISRM <- do.call(rbind, ISRM)
  rownames(ISRM) <- paste("K=", K, sep = "")
  colnames(ISRM) <- c("ISRM")

#m-scale vinod(1976)
#  m <- lapply(K, function(x) {
#    p - sum(eigval / (eigval + rep(x, each=p) ))
#  })
#  m <- do.call(rbind, m)
#  rownames(m) <- paste("K=", K, sep = "")
#  colnames(m) <- c("m scale")
  m <- p- dfridge

  diaghat <- lapply(hatr(x), function(x) {
    diag(x)
  })
  diaghat <- do.call(cbind, diaghat)
  #   rownames(diaghat)<-paste("K=", K, sep="")
  #   colnames(diaghat)<-c("diag hat")

  rsigma2<-rstats1(x)$rsigma2

  CK <- lapply(1:length(K), function(i, SSRK, rsigma2, hatR) {
    SSRK[i] / rsigma2[i] - n + 2 + 2 * sum(diaghat[,i])
  }, SSRK = SSER, hatR = hatr(x), rsigma2 = rsigma2)
  CK <- do.call(rbind, CK)
  rownames(CK) <- paste("K=", K, sep = " ")
  colnames(CK) <- c("CK")

  #PRESS<-lapply(1:length(K), function(i, res, hatR){sum( (res[,i]/(1-1/n-hatR[,i]) )^2)},
  #              res=resid(obj), hatR=diaghat)
  PRESS <- colSums(press(x) ^ 2)
  EP = n - redf
  colnames(EP) = c("EP")
  #PRESS<-apply(press(obj), 2, function(x){sum(x^2)})
  mses <- list(CK = CK,
               dfridge = dfridge,
               EP = EP,
               redf = redf,
               EF = EF,
               ISRM = ISRM,
               m = m,
               PRESS = PRESS
               #,K = K,
              )

  class(mses) <- "rstats2"
  mses

}

print.rstats2 <- function(x, digits = max(5,getOption("digits") - 5),...) {
#  cat("Call:\n", paste(deparse(x$call), sep="\n", collapse="\n"), "\n\n", sep="")
  cat("\nRidge Regression Statistics 2:\n\n")
  res <- cbind(CK   = x$CK,
               #rsigma2 = x$rsigma2,
               dfridge = x$dfridge,
               EP = x$EP,
               REDF  = x$redf,
               EF   = x$EF,
               ISRM = x$ISRM,
               m    = x$m,
               PRESS = x$PRESS
              )
  colnames(res) <- c("CK",
 #                    "RSigma^2",
                     "DF ridge",
                     "EP",
                     "REDF",
                     "EF"  ,
                     "ISRM",
                     "m scale",
                     "PRESS")
  #  rownames(res)<-paste("K=", x$K)
  #dimnames(res)<-list(rownames(res, do.NULL = FALSE,K), colnames(res))
  #res<-lapply(res, function(x){dimnames(x)<-list(rownames(x, do.NULL=FALSE, as.list(K)), colnames(x))})
  print(round(res, 4),...)
  cat("\n")
  invisible(x)
}
