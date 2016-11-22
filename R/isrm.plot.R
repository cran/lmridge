#ISRM and m plot
isrm.plot <- function(x, ...) {
  #K<-x$K[which.min(rstats2(x)$PRESS)]
  isrm <- rstats2(x)$ISRM
  m <- rstats2(x)$m

  par(
    mfrow = c(2,1), pty = "m", mai = c(.7,.8,.3,.3),mgp = c(1.9,1,0), mar =
      c(3,3,3,2), oma = c(0,0,3,0)
  )

  plot(
    x$K, m, main = "m-scale vs K", xlab = "K", ylab = "m scale", lwd = 2, type =
      'l'
  )
  # if (abline) {
  #   abline(v = x$K[which.min(m)], lty = 2)
  #   text(x$K[which.min(m)], min(m), paste("m=", round(min(m), 3)), col =
  #          "blue", pos = 4)
  # }

  plot(
    x$K, isrm, main = "ISRM vs K", xlab = "K", ylab = "ISRM", lwd = 2, type =
      'l'
  )
#   if (abline) {
# #    abline(v = x$K[which.min(isrm)], lty = 2)
#     text(x$K[which.min(isrm)], min(isrm), paste("isrm=", round(min(isrm), 3)), col =
#            "blue", pos = 4)
#   }
  mtext(
    "m scale and ISRM", outer = TRUE, side = 3, cex = 1.8, line = 1
  )
  par(par(no.readonly = TRUE))
}
