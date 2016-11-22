summary.lmridge <- function(object, ...) {
  res <- vector("list")
  res$call <- object$call
  res$K <- object$K
  y <- object$y
#  n <- nrow(object$xs)
  #p<-ncol(object$xs)

  #   svdx<-svd(object$xs)
  #   U<-svdx$u
  #   D<-svdx$d
  #   V<-svdx$v
  #   div<-object$div

  # rsigma2<-object$rsigma2
  rcoefs <- object$coef
  #vcov<-object$vcov
  vcov <- vcov(object)
  #calculation of Statndard Error
  SE <- lapply(vcov, function(x) {
    sqrt(diag(x))
  })
  SE <- do.call(cbind, SE)
  rownames(SE) <- rownames(rcoefs)
  colnames(SE) <- colnames(rcoefs)
  tstats <- (rcoefs / SE)
  pvalue <- 2 * (1 - pnorm(abs(tstats)))

  summaries <- vector("list", length(res$K))

  coefs <- rbind(coef(object))
  resid <- resid(object)

  R2r <- round(rstats1(object)$R2, 4)
  adjR2r <- round(rstats1(object)$adjR2, 4)

  #  seb0<-1/n * var(y) +  colSums(rcoefs^2) # standard Error of beta0
  #seb0 <- sqrt(1/n*var(y)+ sum(diag(vcov(object)[[1]]))%*%colMeans(object$mf[,-1]) )
  #seb0<-sqrt(1/n * var(y) + diag(vcov(object)[[1]])%*%object$xm)


  b0 <- mean(object$mf[,1]) - colSums(rcoefs * object$xm)
  seb0<-numeric(length(res$K))
  for (i in seq(length(res$K))) {
   # seb0<-(1/(object$n)*var(object$y)+sum(1/(object$n)*object$coef[,i]^2)) # standard Error of beta0
    #seb0[i]<-sqrt(1/n*var(y) + 1/n*sum(diag(vcov(object)[[i]])%*%object$xscale))
    seb0[i]<-sqrt(var(y)+sum(cbind(1,object$mf[,-1]))+sum(diag(vcov(object)[[i]])))
    #seb0[i]<-sqrt(t(object$coef[,i]) %*%vcov(object)[[i]]%*%object$coef[,i])
    summary <- vector("list")
    #    if(object$lambda[[i]]!=0){

    if (object$Inter) {
      summary$coefficients <-
        cbind(coefs[i,], c(b0[i], rcoefs[,i]), c(seb0[i], SE[,i]),
              c(b0[i] / seb0[i], tstats[,i]), c(2*(1-pnorm(abs(seb0[i]))), pvalue[,i]))

      colnames(summary$coefficients) <-
        c("Estimate", "Estimate (Sc)", "StdErr (Sc)", "t-value (Sc)", "Pr(>|t|)")
      summary$stats <-
        cbind(
          R2r[i], adjR2r[i], rstats2(object)$dfridge[i], rstats1(object)$Fv[i],
          infocr(object)[i,1], infocr(object)[i,2]
        )
      #summary$stats <- cbind(object$lambda[i], R2r[i], object$df[i],
      #                       object$AIC[i], object$BIC[i] )
      colnames(summary$stats) <-
        c("R2", "adj-R2","DF ridge", "F", "AIC", "BIC")
      summary$rmse1 <- min(rstats1(object)$mse)
      summary$rmse2 <- object$K[which.min(rstats1(object)$mse)]
    } else{
      summary$coefficients <-
        cbind(coefs[i,-1], rcoefs[,i], SE[,i],  tstats[,i],  pvalue[,i])
      #
      colnames(summary$coefficients) <-
        c("Estimate", "Estimate (Sc)", "StdErr", "t-value", "Pr(>|t|)")

      summary$stats <-
        cbind(
          R2r[i], adjR2r[i], rstats2(object)$dfridge[i], rstats1(object)$Fv[i],
          infocr(object)[i,1], infocr(object)[i,2]
        )
      #summary$stats <- cbind(object$lambda[i], R2r[i], object$df[i],
      #                       object$AIC[i], object$BIC[i] )
      colnames(summary$stats) <-
        c("R2","adj-R2", "DF ridge", "F", "AIC", "BIC")

      summary$rmse1 <- min(rstats1(object)$mse)
      summary$rmse2 <- object$K[which.min(rstats1(object)$mse)]
    }
    summary$K <- object$K[i]

    summaries[[i]] <- summary
    names(summaries)[[i]] <- paste("summary ", i, sep = " ")
    rm(summary)
  }
  res$summaries <- summaries
  class(res) <- "summary.lmridge"
  res
}
