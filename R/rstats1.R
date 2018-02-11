rstats1 <- function(x,...)
  UseMethod("rstats1")
rstats1.lmridge <- function(x,...) {
  #x <- obj$xs
  y <- x$y
  z <- x$Z
  K <- x$K
  Y <- x$mf[,1]
  K <- x$K
  coef <- x$coef
  n <- nrow(x$xs)
  p <- ncol(x$xs)
  ls <- lm.fit(x$xs,y)$coefficients
  eigval <- eigen(t(x$xs) %*% x$xs)$values
  P <- eigen(t(x$xs) %*% x$xs)$vectors
  #ols model variance
  sigma2 <- sum(lm.fit(x$xs,y)$residuals ^ 2) / (n - p)

  resid <- resid(x)

#calculations for Ridge variance
  rvarcal <-
    lapply(K, function(lam) {
      sum(eigval / (eigval + rep(lam,each = p))^2 )
    })
  rvarcal <- do.call(rbind,rvarcal)
  #rownames(rvarcal) <- paste("K=", K, sep = "")
#  colnames(rvarcal) <- c("R Factor")

# Error dg (REDF)
  redf <- lapply(hatr(x), function(x) {
    n - sum(diag(2 * x - x %*% t(x)))
  })

  rsigma2 <-
    mapply(function(x,y) {
      x / y
    }, colSums(resid(x) ^ 2), redf, SIMPLIFY = FALSE)
  rsigma2 <- do.call(rbind,rsigma2)
  rownames(rsigma2) <- paste("K=", K, sep = "")
  colnames(rsigma2) <- c("Rsigma2")

#var<-obj$sigma2*sum(eigval/(eigval+lambda)^2)
  var <- mapply(function(x,y) {
    x * y
  }, rsigma2, rvarcal, SIMPLIFY = FALSE)
  var <- do.call(rbind,var)
  rownames(var) <- paste("K=", K, sep = "")
  colnames(var) <- c("VAR")

  bias2 <-
    lapply(K, function(lam) {
      sum((-lam * solve(t(x$xs) %*% x$xs + lam * diag(p)) %*% ls) ^ 2)
    })
  bias2 <- do.call(rbind, bias2)
  rownames(bias2) <- paste("K=", K, sep = "")
  colnames(bias2) <- c("Bias^2")


  mser <- mapply(function(x,y) {
    x + y
  }, var, bias2, SIMPLIFY = FALSE)
  mser <- do.call(rbind, mser)
  rownames(mser) <- paste("K=", K, sep = "")
  colnames(mser) <- c("MSE")

  Fv <- lapply(1:length(K), function(i, b, v) {
    1 / p * (t(b[,i]) %*% solve(v[[i]]) %*% b[,i])
  }, b = coef, v = vcov(x))
  Fv <- do.call(rbind, Fv)
  rownames(Fv) <- paste("K=", K, sep = "")
  colnames(Fv) <- c("Fv")

  SSER <- apply(resid,2,function(x) {
    sum(x ^ 2)
  })
  SSRR <- apply(x$rfit,2, function(x) {
    sum(x ^ 2)
  })
  SSTR <- t(y) %*% y

  R2r <- lapply(SSRR,function(x) {
    x / SSTR
  })
  R2r <- do.call(rbind, R2r)
  rownames(R2r) <- paste("K=", K, sep = "")
  colnames(R2r) <- c("R2")

  adjR2r <- 1 - ((n - 1) / (n - p)) * (1 - R2r)
  #adjR2r<-do.call(rbind,adjR2r)
  rownames(adjR2r) <- paste("K=", K, sep = "")
  colnames(adjR2r) <- c("adj-R2")

  CN <- lapply(K, function(K) {
    max(eigval + K) / min(eigval + K)
  })
  CN <- do.call(rbind, CN)
  rownames(CN) <- paste("K=", K, sep = "")
  colnames(CN) <- c("CN")

  mses <-
    list(
      var = var,
      bias2 = bias2,
      mse = mser,
      Fv = Fv,
      R2 = R2r,
      rsigma2=rsigma2,
      adjR2 = adjR2r,
      eigval = eigval,
      CN = CN
    )

  class(mses) <- "rstats1"
  mses
}

print.rstats1 <-
  function(x, digits = max(5,getOption("digits") - 5),...) {
    #  cat("Call:\n", paste(deparse(x$call), sep="\n", collapse="\n"), "\n\n", sep="")
    cat("\nRidge Regression Statistics 1:\n\n")
    res <- cbind(
      var = x$var,
      bias2 = x$bias2,
      mse = x$mse,
      rsigma2 = x$rsigma2,
      Fv   = x$Fv,
      R2    = x$R2,
      adjR2 = x$adjR2,
      CN   = x$CN
    )
    colnames(res) <- c("Variance",
                       "Bias^2",
                       "MSE",
                       "rsigma2",
                       "F",
                       "R2",
                       "adj-R2",
                       "CN")
    #  rownames(res)<-paste("K=", x$K)
    #dimnames(res)<-list(rownames(res, do.NULL = FALSE,K), colnames(res))
    #res<-lapply(res, function(x){dimnames(x)<-list(rownames(x, do.NULL=FALSE, as.list(K)), colnames(x))})
    print(round(res, 4),...)
    cat("\n")
    invisible(x)

  }
