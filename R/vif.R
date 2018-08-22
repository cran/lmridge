vif <- function(x, ...)
  UseMethod("vif")
vif.lmridge <- function(x, ...) {
  #   p<-ncol(object$xs)
  #   K<-object$K
  #   type<-match.arg(type)
  #
  #   if(type=="exal"){
  #       v<-vcov(object)
  #       R<-lapply(v, function(x){cov2cor(x)})
  #       detR <- lapply(R, function(x){det(x)})
  # #   p <- nrow(v)
  #       VIF <- rep(0,p)
  #       for (j in 1:p) {
  #       #  VIF[i] <- R[i,i] * det(as.matrix(R[-i,-i])) / detR
  #       #VIF
  #       VIF<-lapply(1:length(K), function(i,  R, detR){
  #               R[[i]][j,j]*det(as.matrix(R[[i]][-j,-j])) / detR[[i]]},R=R,  detR=detR)
  #             }
  # #      VIF
  #      # VIF<-lapply(1:p, function(i,R, detR){R[i,i] * det(as.matrix(R[-i,-i])) / detR}, R=R, detR=detR)
  #
  #   }else{

  Z <- x$Z

  VIF <- lapply(Z,function(x) {
    x %*% t(x)
  })


  res <- do.call(rbind, lapply(VIF, function(x) {
    diag(x)
  }))
  rownames(res) <- paste("k=", x$K,sep = "")
  round(res, 5)
}
