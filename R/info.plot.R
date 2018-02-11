info.plot <- function(x, abline = TRUE, ...) {
  df <- rstats2(x)$dfridge
  aic <- infocr(x)[,1]
  bic <- infocr(x)[,2]

  col = cbind("black", "red")
  mse <- rstats1(x)$mse
  K <- x$K
  dfminmse <- df[which.min(mse)]
  mselect <- cbind(aic, bic)

  if (length(K) == 1) {
    plot(
      x = rep(df,length(mselect)),
      y = mselect,
      main = "Model Selection Criteria",
      xlab = "DF",
      ylab = "Model Criteria",
      col = col,
      lwd = 2,
      lty = c(1,4)
    )
  }  else{
    matplot(
      df,
      mselect,
      main = 'Model Selection Criteria',
      xlab = 'DF',
      ylab = "Model Criteria",
      col = col,
      lwd = 2,
      type = 'l',
      lty = c(1,4)
    )
  }
  legend(
    "topright",
    legend = c("AIC", "BIC"),
    col = col,
    lwd = 2,
    fill = 1:2,
    lty = c(1,4),
    cex = 0.7,
    pt.cex = 0.5,
    bty = "0",
    bg = "transparent",
    y.intersp = 0.4,
    x.intersp = 0.3,
    merge = TRUE
  )
  if (abline) {
    abline(v = dfminmse, lty = 2)
    text(dfminmse,
         min(mselect),
         paste(c("min MSE =", "at df"), c(round(min(mse),3), round(dfminmse,2)), collapse = " "),
         col ="red",
         pos=3
         )
  }
}
