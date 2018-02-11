plot.lmridge <-
  function(x, type = c("ridge", "vif"),abline = TRUE,...) {
    K <- x$K
    coefs <- x$coef
    ks <- kest(x)
    col <- 1:ncol(t(coefs))#rainbow(ncol(t(coefs)))
    lends = rownames(coefs)
    df <- rstats2(x)$dfridge

    type <- match.arg(type)
    if (type == "vif") {
      if (length(K) == 1) {
        plot(
          x = rep(K,length(vif(x))),
          y = vif(x), xlab = "Biasing Parameter K",
          ylab = "VIF",
          main = "VIF Trace",
          col = col,
          pch = 19,
          ...
        )
        legend("topright",
               legend = lends,
               lwd = 2,
               cex = 0.7,
               pt.cex = 0.5,
               lty = 1:ncol(x$xs),
               col = col,
               y.intersp = 0.4,
               x.intersp = 0.3,
               bty = "o",
               bg = "transparent",
               merge = TRUE
              )
      } else {
        matplot(
          x = K,
          y = (vif(x)), type = "l",
          xlab = "Biasing Parameter K",
          ylab = "VIF",
          main = "VIF Trace",
          lty = 1:ncol(x$xs),
          lwd = 2,
          col = col
        )

        legend("topright",
               legend = lends,
               lwd = 2,
               cex = 0.7,
               pt.cex = 0.5,
               lty = 1:ncol(x$xs),
               col = col,
               y.intersp = 0.4,
               x.intersp=0.3,
               bty = "o",
               bg = "transparent",
               merge = TRUE
              )

        if (abline) {
          abline(h = 0, lty = 2)
          abline(h = 10, lty = 2, col = "red")
          text(max(K),15, paste("vif=10"), pos = 2, col = "blue")
          abline(v = ks$kGCV, lty = 2, col = "red")
    #      text(ks$kGCV, min(vif(x)), paste("K=", K[which.min(ks$GCV)]), pos=4, col="red")
    #      text(ks$kGCV, max(vif(x)), paste("GCV=", round(min(ks$GCV), 3) ), pos = 4, col = "red")
          text(ks$kGCV,
              max(vif(x)),
              paste(c("min GCV=", " at K="),
                    c(round(min(ks$GCV), 3),K[which.min(ks$GCV)]),
                    collapse = '' ),
              pos = 4,
              col = "red"
              )
        }
      }
    }

    else {
      if (length(K) == 1) {
        plot(
          x = rep(K,length(coefs)),
          y = coefs,
          xlab = "Biasing Parameter K",
          ylab = expression(paste("Ridge Coefficients ", hat(beta[ridge]))) ,
          main = "Ridge Trace Plot",
          col = col,
          pch = 19,
          ...
        )
        legend("topright",
               legend = lends,
               lwd = 2,
               cex=0.7,
               pt.cex=0.5,
               lty = 1:ncol(x$xs),
               col = col,
               y.intersp = 0.4,
               x.intersp = 0.3,
               bty = "o",
               bg = "transparent",
               merge = TRUE
        )
      } else {
        matplot(
          x = K,
          y = t(coefs),
          type = "l",
          xlab = "Biasing Parameter K",
          ylab = expression(paste("Ridge Coefficients, ", hat(beta[ridge]))),
          main = "Ridge Trace Plot",
          lty = 1:ncol(x$xs),
          lwd = 2,
          col = col
        )
        text(
          rep(max(K), length(x$xs)),
          coefs[, length(K)],
          labels = colnames(x$xs),
          pos = 3,
          cex = .7
        )
        legend("topright",
               legend = lends,
               lwd = 2,
               cex=0.7,
               pt.cex=0.5,
               lty = 1:ncol(x$xs),
               col = col,
               y.intersp = .4,
               x.intersp=0.3,
               bty = "o",
               bg = "transparent",
               merge = TRUE
        )
        if (abline) {
          abline(h = 0, lty = 2)
          abline(v = ks$kGCV, lty = 2, col = "red")
        #  text(K[which.min(rstats1(x)$mse)], min(coefs), paste("K=", K[which.min(rstats1(x)$mse)]), pos = 4, col = "red")
        #  text(K[which.min(rstats1(x)$mse)], max(coefs), paste("MSE=", round(min(rstats1(x)$mse), 3) ), pos=4, col="red" )
          text(K[which.min(rstats1(x)$mse)],
               max(coefs),
               paste(c("min MSE=", " at K="),
                     c(round(min(rstats1(x)$mse), 3), K[which.min(rstats1(x)$mse)]),
                     collapse = '' ),
               pos = 4,
               col = "red"
              )
        }
      }
    }
  }
