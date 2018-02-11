rplots.plot <- function(x, abline = TRUE,...) {
  K <- x$K[which.min(rstats2(x)$PRESS)]
  df <- rstats2(x)$dfridge
  press <- rstats2(x)$PRESS

  par(mfrow = c(3,1),
      pty = "m",
      mai = c(.7,.8,.3,.3),
      mgp = c(2,1,0),
      mar = c(3,3,3,2),
      oma = c(0,0,3,0)
     )

  plot(x$K,
       df,
       main = "DF-Trace",
       xlab = "K",
       ylab = "DF",
       lwd = 2,
       type = 'l'
      )
  plot(df,
       colSums(resid(x) ^ 2),
       main = 'RSS vs K',
       xlab = 'DF',
       ylab = 'RSS',
       lwd = 2,
       type = 'l'
      )
  plot(x$K,
       press,
       main = "PRESS vs K",
       xlab = "K",
       ylab = "PRESS",
       lwd = 2,
       type ='l'
      )

  if (abline) {
    abline(v = K, lty = 2)
    text(K,
         max(press),
         paste(c("min PRESS=", " at K="),
               c(round(min(press),4), K),
               collapse  =" " ),
         col = "red",
         pos = 4)
    }
    mtext("Misc Ridge Plots", outer = TRUE, side = 3, cex = 1, line = 1  )
}
