summary.lmridge <- function(object, ...) {
  res <- vector("list")
  res$call <- object$call
  res$K <- object$K
  y <- object$y
  ym <- mean(object$mf[,1])
  n <- nrow(object$xs)
  xm<-object$xm
  #p<-ncol(object$xs)

  #   svdx<-svd(object$xs)
  #   U<-svdx$u
  #   D<-svdx$d
  #   V<-svdx$v
  #   div<-object$div

  # rsigma2<-object$rsigma2
  rcoefs <- object$coef
  vcov <- vcov(object)

  #calculation of Statndard Error
  SE <- lapply(vcov, function(x) {
    sqrt(diag(x))
  })

  SE <- do.call(cbind, SE)
  rownames(SE) <- rownames(rcoefs)
  colnames(SE) <- colnames(rcoefs)

  # t-values

  tstats <- (rcoefs / SE)

  H <- hatr(object)
  df <- lapply(H, function(x){
  n - sum(diag(x))
  })
  df <- do.call(cbind, df)

  #pvalue <- 2 * (1 - pnorm(abs(tstats)))
  if(object$Inter){
    pvalue <- 2*pt(-abs(tstats), rep(df-1, each=nrow(tstats)))
  }else {
    pvalue <- 2*pt(-abs(tstats), rep(df, each=nrow(tstats)))
  }

  summaries <- vector("list", length(res$K))

  coefs <- rbind(coef(object))
  resid <- resid(object)

  R2r <- round(rstats1(object)$R2, 4)
  adjR2r <- round(rstats1(object)$adjR2, 4)

  b0 <- ym - colSums(rcoefs * xm)
  seb0<-numeric(length(res$K))

  for (i in seq(length(res$K))) {
    seb0[i]<-sqrt(var(y)/n+ (object$xm^2)%*%(diag(vcov(object)[[i]])))
    summary <- vector("list")

    if (object$Inter) {
      summary$coefficients <- cbind(coefs[i,],
                                    c(b0[i], rcoefs[,i]),
                                    c(seb0[i], SE[,i]),
                                    c(b0[i] / seb0[i], tstats[,i]),
                                    c(2*pt(-abs(b0[i]/seb0[i]),df[i]-1 ), pvalue[,i])
                                    )

      colnames(summary$coefficients) <- c("Estimate",
                                          "Estimate (Sc)",
                                          "StdErr (Sc)",
                                          "t-value (Sc)",
                                          "Pr(>|t|)"
                                          )

      summary$stats <- cbind(R2r[i],
                             adjR2r[i],
                             rstats2(object)$dfridge[i],
                             rstats1(object)$Fv[i],
                             infocr(object)[i,1],
                             infocr(object)[i,2]
                             )

      colnames(summary$stats) <- c("R2",
                                   "adj-R2",
                                   "DF ridge",
                                   "F",
                                   "AIC",
                                   "BIC")

      summary$rmse1 <- min(rstats1(object)$mse)
      summary$rmse2 <- object$K[which.min(rstats1(object)$mse)]
      summary$df1<-rstats2(object)$dfridge
      summary$df2<-rstats2(object)$redf
      summary$fpvalue<-pf(rstats1(object)$Fv[i],
                          summary$df1,
                          summary$df2,
                          lower.tail=F )
    } else{
      summary$coefficients <- cbind(coefs[i,],
                                    rcoefs[,i],
                                    SE[,i],
                                    tstats[,i],
                                    pvalue[,i])

      colnames(summary$coefficients) <- c("Estimate",
                                          "Estimate (Sc)",
                                          "StdErr",
                                          "t-value",
                                          "Pr(>|t|)")

      summary$stats <- cbind(R2r[i],
                             adjR2r[i],
                             rstats2(object)$dfridge[i],
                             rstats1(object)$Fv[i],
                             infocr(object)[i,1],
                             infocr(object)[i,2]
        )

      colnames(summary$stats) <- c("R2",
                                   "adj-R2",
                                   "DF ridge",
                                   "F",
                                   "AIC",
                                   "BIC")

      summary$rmse1 <- min(rstats1(object)$mse)
      summary$rmse2 <- object$K[which.min(rstats1(object)$mse)]
      summary$df1<-rstats2(object)$dfridge
      summary$df2<-rstats2(object)$redf
      summary$fpvalue<-pf(rstats1(object)$Fv[i],
                          summary$df1,
                          summary$df2,
                          lower.tail=F )
    }  # else end
    summary$K <- object$K[i]

    summaries[[i]] <- summary
    names(summaries)[[i]] <- paste("summary ", i, sep = " ")
    rm(summary)
  } # for loop end

  res$summaries <- summaries
  class(res) <- "summary.lmridge"
  res

}
