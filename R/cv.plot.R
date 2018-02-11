cv.plot <- function(x, abline = TRUE,...) {
  #K <- x$K[which.min(rstats2(x)$PRESS)]
  K<-kest(x)$kGCV

  par(
    mfrow = c(2,1),
    pty = "m",
    mai = c(.7,.8,.3,.3),
    mgp = c(1.9,1,0),
    mar = c(3,3,3,2),
    oma = c(0,0,3,0)
  )

  plot(x$K,
       kest(x)$GCV,
       main = "GCV vs K",
       xlab = "K",
       ylab = "GCV",
       lwd = 2,
       type = 'l'
      )
  if (abline) {
    abline(v = K, lty = 2)
    #text(K, max(kest(x)$GCV), paste("K=", x$K[which.min(kest(x)$GCV)]), col = "blue", pos = 4)

    abline(h=min(kest(x)$GCV), lty=2)
    #text(K, min(kest(x)$GCV), paste("GCV=",round(min(kest(x)$GCV), 3)), col = "blue", pos=4)
    text(K,
        max(kest(x)$GCV),
        paste(c("min GCV=", " at K="), c(round(min(kest(x)$GCV), 3), K), collapse=" "),
        col="red",
        pos=4
        )
  }

  plot(
    x$K,
    kest(x)$CV,
    main = "CV vs K",
    xlab = "K",
    ylab = "CV",
    lwd = 2,
    type ='l'
    )
  if (abline) {
    abline(v = K, lty = 2)
#    text(K, max(kest(x)$CV), paste("K=", x$K[which.min(kest(x)$CV)]), col = "blue", pos = 4)

    abline(h=min(kest(x)$CV), lty=2)
#    text(K, min(kest(x)$CV), paste("CV=", round(min(kest(x)$CV),3)), col="blue", pos=4)
    text(K,
         max(kest(x)$CV),
         paste(c("min CV=", " at K="), c(round(min(kest(x)$CV), 3), K), collapse=" "),
         col="red",
         pos=4
         )

  }
  mtext(
    "Cross Validation Plots",
    outer = TRUE,
    side = 3,
    cex = 1.8,
    line = 1
  )
  par(par(no.readonly = TRUE))
}
