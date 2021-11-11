kest <- function(object,...)
  UseMethod("kest")

kest.lmridge <- function(object,...) {
  # lsfit<-obj$lsfit
  # ls<-obj$ls
  K = object$K
  x <- object$xs # scaled designed matrix
  y <- object$y  # scaled y vector
  #sigma2<-obj$sigma2
  n <- nrow(x)
  p <- ncol(x)

  P <- eigen(t(x) %*% x)$vectors
  EV <- eigen(t(x) %*% x)$values
  xstar <- x %*% P
  alphahat <- solve(diag(EV,p)) %*% t(xstar) %*% y

 # sigma2alpha<-sum((y-xstar%*%alphahat)^2)/(n-p)

  ls <- lm.fit(x,y)$coefficients  # ls coefficients for scaled data
  lsfit <-
  lm.fit(x,y)$fitted.values # ls fitted values for scaled data

  sigma2 <- sum(lm.fit(x,y)$residuals ^ 2) / (n - p)

  coef <- object$coef
  d <- object$d

  div <- object$div
  #SSER <- apply(resid(obj),2,function(x) {
  #  sum(x ^ 2)
  #})

  GCV <- matrix(0, 1, length(K))
  CV <- matrix(0, 1, length(K))

  for (i in seq(length(K))) {
    GCV[,i]<-colSums((y-x%*%(coef[,i]))^2)/(n-colSums(matrix(d^2/div[[i]],p)))^2
  }

  if (length(GCV) > 0) {
    k <- seq_along(GCV)[GCV == min(GCV)]
    kGCV <- object$K[k]
  }

  CV <- 1 / n * colSums(press(object) ^ 2)
  if (length(CV) > 0) {
    k <- seq_along(CV)[CV == min(CV)]
    kCV <- object$K[k]
  }

  mHKB <- ((p - 2) * (sigma2)) / sum(ls ^ 2)     #Thisted , 1976
  HKB <- p * sigma2 / sum(ls ^ 2)                #Hoerl and Kennard 1975
  DSk <- sigma2 / sum(ls ^ 2)                    #Singh, Chaubey and Dwivedi (1986)
  LW <- ((p - 2) * sigma2 * n) / sum(lsfit ^ 2)  #as in lm.ridge of MASS
  Lw1976 <- p * sigma2 / sum(EV * alphahat ^ 2)

  KibAM <- (1 / p) * sum(sigma2 / ls ^ 2)           #Kibria (2003)
  KibGM <- sigma2 / (prod(alphahat ^ 2)) ^ (1 / p)  #Kibria 2003
  KibMED <- median(sigma2 / alphahat ^ 2)       #Kibria (2003)

  mj <- sqrt(sigma2 / alphahat ^ 2)
  KM2 <- max(1 / mj)            # Muniz and Kibria 2009
  KM3 <- max(mj)              # Muniz and Kibria 2009
  KM4 <- prod(1 / mj) ^ (1 / p)     # Muniz and Kibria 2009
  KM5 <- prod(mj) ^ (1 / p)       # Muniz and Kibria 2009
  KM6 <- median(1 / mj)         # Muniz and Kibria 2009

  KMNdenom2 <- (n - p) * sigma2 + max(EV) * alphahat ^ 2
  KMNdenom1 <- max(EV) * sigma2

  KMN8 <-   max(1 / (sqrt(KMNdenom1 / KMNdenom2)))          #Muniz et al 2012
  KMN9 <-   max(sqrt(KMNdenom1 / KMNdenom2))            #Muniz et al 2012
  KMN10 <- (prod((1 / sqrt(KMNdenom1 / KMNdenom2 )))) ^ (1 / p)   #Muniz et al 2012
  KMN11 <- (prod((sqrt(KMNdenom1 / KMNdenom2)))) ^ (1 / p)  #Muniz et al 2012
  KMN12 <-  median(1 / sqrt(KMNdenom1 / KMNdenom2))           #Muniz et al 2012

  KD <- max(cbind(0,HKB - 1 / n * max(vif(object))))               #Dorugae et al 2010
  KAD4 <-  (2 * p / max(EV)) * sum(sigma2 / alphahat ^ 2)

  kesti <- list(mHKB = mHKB, LW = LW, LW76 = Lw1976, CV = CV, kCV = kCV,
                HKB = HKB, KibAM =KibAM, kGCV = kGCV, DSk = DSk,GCV = GCV,#ls=ls,
                KibGM = KibGM, KibMED = KibMED, KM2 = KM2, KM3 = KM3, KM4 = KM4,
                KM5 = KM5, KM6 = KM6, KMN8 = KMN8, KMN9 = KMN9,KMN10 = KMN10,
                KMN11 = KMN11, KMN12 = KMN12, KD = KD, KAD4 = KAD4,
                alphahat=alphahat
      )#,P=P,xstar=xstar, EV=EV)

  class(kesti) <- "klmridge"
  kesti
}
