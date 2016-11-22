#credit predict.lm

predict.lmridge <-
  function(object, newdata, na.action = na.pass, ...) {
    # tt <- terms(object)
    tt <- attr(object$mf, "terms")

    if (!inherits(object, "lmridge"))
      warning("calling predict.lmridge(<fake-lmridge-object>) ...")

    if (missing(newdata) || is.null(newdata)) {
      mm <- X <- model.frame(object)
      mmDone <- TRUE
      offset <- object$offset
    }
    else {
      Terms <- delete.response(tt)
      m <- model.frame(Terms, newdata, na.action = na.action)
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, m)
      mm <- X <- model.matrix(Terms, m)
      offset <- rep(0, nrow(X))

      if (!is.null(off.num <- attr(tt, "offset")))
        for (i in off.num)
          offset <-
        offset + eval(attr(tt,"variables")[[i + 1]], newdata)

      if (!is.null(object$call$offset))
        offset <- offset + eval(object$call$offset, newdata)
      mmDone <- FALSE
    }
    intercept <- attr(tt, "intercept")
    tlabel <- attr(tt, "term.labels")
    if (intercept) {
      mm <- cbind(1, X[,tlabel])
    }else{
      mm <- X[,tlabel]
    }
    beta <- coef(object)
    if (length(object$K) > 1 & nrow(X) > 1)
      res <- apply(beta, 1, function(x) {
        drop(as.matrix(mm) %*% x)
      })
    else if (length(object$K) > 1 & nrow(X) == 1) {
      res <- lapply(1:length(object$K), function(i, mm, beta) {
        c(1,mm[,-1]) %*% beta[i,]
      }, mm = mm, beta = beta)
      res <- do.call(cbind, res)
      colnames(res) <- paste("K=", object$K, sep = "")
    }
    else if (length(object$K) == 1 & nrow(X) > 1) {
      res <- drop(as.matrix(mm) %*% beta)
    }
    else
      res <- drop(t(as.matrix(c(1,mm[,-1]))) %*% beta)
    res
  }
