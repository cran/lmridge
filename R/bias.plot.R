bias.plot <- function(x, abline = TRUE,...) {
  vif <- vif(x)
  bias2 <- rstats1(x)$bias2
  var <- rstats1(x)$var
  mse <- rstats1(x)$mse
  minmse <- min(mse)
  K <- x$K[which.min(mse)]

  col = cbind("black", "red", "green")
  ridgetrade <- cbind(var,bias2, mse)

  if (length(x$K) == 1) {
    plot(
      x = rep(x$K, length(ridgetrade)),
      y = ridgetrade,
      main = "Bias, Variance Tradeoof",
      xlab = "Biasing Parameter",
      ylab = " ",
      col = col,
      lwd = 2,
      lty = c(1,4,5)
    )
    legend(
      "topright",
      legend = c("Var", "Bias^2", "MSE"),
      col = col,
      lwd = 2,
      fill = 1:3,
      lty = c(1,4,5),
      cex = 0.7,
      pt.cex = 0.5,
      bty = "o",
      bg = "transparent",
      y.intersp = 0.4,
      x.intersp = 0.3,
      merge = TRUE
    )

  }else
    matplot(
      x$K,
      ridgetrade,
      main = 'Bias, Variance Tradeoff',
      xlab = 'Biasing Parameter',
      ylab= " ",
      col = col,
      lwd = 2,
      type = 'l',
      lty = c(1,4, 5)
    )
  legend(
    "topright",
    legend = c("Var", "Bias^2", "MSE"),
    col = col,
    lwd = 2,
    fill = 1:3,
    lty = c(1,4,5),
    cex = 0.7,
    pt.cex = 0.5,
    bty = "o",
    bg = "transparent",
    y.intersp = 0.4,
    x.intersp = 0.3,
    merge = TRUE
  )

  if (abline) {
    abline(v = K, lty = 2)
    abline(h = minmse, lty = 2)
    #text(K, max(rstats1(x)$mse),  paste("mse=", round(minmse, 3)), col = "blue", pos = 4)

    #text(K,minmse + 10, paste("K=", K), pos = 4, col = "blue")
    text(K,
         max(rstats1(x)$mse),
         paste(c("min MSE", " at K="), c(round(minmse,3), K ), collapse = ''),
         col="red",
         pos=4
        )
  }
}
