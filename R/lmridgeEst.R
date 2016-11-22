lmridgeEst <-
  function(formula=formula, data, K = 0, scaling = c("sc", "scaled", "centered"), ...) {
    if (is.null(K)) {
      K <- NULL
    }else{
      K <- K
    }

    call <- match.call()
    mt <- match.call(expand.dots = FALSE)

    mf <- model.frame(formula = formula, data = data)
    x <- model.matrix(attr(mf,"terms"), data = mf)
    y <- model.response(mf)
    mt <- attr(mf, "terms")


    p <- ncol(x) #p<- #number of variables (columns in design matrix (x))
    n <- nrow(x) #number of observations in design matrix (x)

    if (Inter <- attr(mt, "intercept")) {
      Xm <- colMeans(x[,-Inter]) #means of each X Variable
      Ym <- mean(y)     #mean of Y variable
      Y <- y - Ym        #Subtract the mean from y
      p <- p - 1
      X <- x[,-Inter] - rep(Xm,rep(n,p))  #Subtract the mean from x
 #     XX<- x[,-Inter] # actual X
    }else{
      Xm <- colMeans(x)
      Ym <- mean(y)
      Y <- y - Ym
      X <- x - rep(Xm, rep(n,p))
 #     XX<- x # actual X
    }

    scaling <- match.arg(scaling)

    if (scaling == "scaled") {
      #Scaling with Pop SD
      Xscale <- drop(rep(1 / (n - 1),n) %*% X ^ 2) ^ 0.5

    } else if (scaling == "centered") {
      Xscale <- drop(rep(1, p))
      names(Xscale) <- colnames(X)

    # } else if (scaling=="none"){
    #   Xscale<-drop(rep(1,p))
    #   X<-XX
    #   Y<-y
    # }
    } else {
      #Scaling with deviation
      Xscale <-(drop(rep(1 / (n - 1),n) %*% X ^ 2) ^ 0.5) * sqrt(n - 1)
    }


    X <- X / rep(Xscale,rep(n,p))

    Xs <- svd(X)  #Singular value Decomposition of X matrix

    rhs <- t(Xs$u) %*% Y
    d <- Xs$d #singular values of X
    #   Q<-Xs$v
    #   Z<-X%*%Q

    #k <- length(K)

    #lscoef<-Xs$v%*%(rhs/d) #ridge coef
    #lsfit<-X%*%lscoef
    #resid<-Y-lsfit

    #dx <- length(d)

    #div<-d^2+rep(lambda,rep(dx,k))
    div <- lapply(K, function(x) {
      (d ^ 2) + x
    })
    #aridge<-lapply(lambda, function(x){(rhs*lambda)/(lambda+x)})
    a <- lapply(div, function(x) {drop(d * rhs) / x  })

    #dim(a)<-c(dx,k)

    coef <- lapply(a, function(x) {
      Xs$v %*% x
    }) #ridge coef
    coef <- do.call(cbind, coef)
    rownames(coef) <- colnames(X)

    colnames(coef) <- paste("K=", K,sep = "")

    rfit <- apply(coef, 2,function(x) {
      X %*% x
    })

    #resid<-apply(rfit,2, function(x){Y-x})

    Z <- lapply(K, function(x) {
      solve(crossprod(X,X) + diag(x,p)) %*% t(X)
    })

    #rhat<-lapply(Z, function(x){X%*%x})

    #df Hastie and Tibshirani (1990) n=tr(H)
    #df<-lapply(rhat, function(x){n-sum(diag(x))})

    #effective degree of freedom
    #edf<-lapply(rhat, function(x){n-sum(diag(2*x-x%*%t(x)))})

    #rsigma2<-mapply(function(x,y){x/y}, colSums(resid^2), df, SIMPLIFY = FALSE )


    #***********check it ***********
    #sigma2<-sum(lm.fit(x,y)$residuals^2)/(n-p)

    #Scaled Coefficients
    #     br<-diag(1/Xscale)%*%coef #converted slopes
    #     inter<-Ym-Xm%*%br # converted intercept
    #     bridge<-rbind(inter, br) # converted coef
    #bridge<-br

    #XX<-cbind(1,x)

    #   SSER<-apply(resid,2,function(x){sum(x^2)})
    #   SSTR<-sum((Y-mean(Y))^2)
    #
    #   #Ridge R-square
    #   R2r<-lapply(SSER,function(x){1-x/SSTR})
    #   R2r<-do.call(cbind, R2r)


    list(
      coef = coef, xscale = Xscale, xs = X, y = Y, d = d, div = div, terms = mt,
      xm = Xm, K = K, rfit = rfit, Inter = Inter, mf = mf, scaling = scaling,
      Z = Z, call = match.call()#, x=x#,rhs = rhs
    )
  }
