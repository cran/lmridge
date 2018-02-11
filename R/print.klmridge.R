print.klmridge <- function(x,...) {
  cat("Ridge k from different Authors\n\n")

  krest <- cbind(
    mHKB = x$mHKB,
    DSk = x$DSk,
    LW = x$LW,
    LW76 = x$LW76,
    HKB = x$HKB,
    Kibk = x$KibAM,
    kGCV = x$kGCV,
    kCV = x$kCV,
    KibGM = x$KibGM,
    KibMED = x$KibMED,
    KM2 = x$KM2,
    KM3 = x$KM3,
    KM4 = x$KM4,
    KM5 = x$KM5,
    KM6 = x$KM6,
    KMN8 = x$KMN8,
    KMN9 = x$KMN9,
    KMN10 = x$KMN10,
    KMN11 = x$KMN11,
    KMN12 = x$KMN12,
    KD   = x$KD,
    KAD4 = x$KAD4
    #KL09=x$KL09
  )

  rownames(krest) <- "k values"

  colnames(krest) <- c(
    "Thisted (1976):",
    "Dwividi & Srivastava (1978): ",
    "LW (lm.ridge)",
    "LW (1976)",
    "HKB (1975)",
    "Kibria (2003) (AM)",
    "Minimum GCV at",
    "Minimum CV at",
    "Kibria 2003 (GM): ",
    "Kibria 2003 (MED): ",
    "Muniz et al. 2009 (KM2): ",
    "Muniz et al. 2009 (KM3): ",
    "Muniz et al. 2009 (KM4): ",
    "Muniz et al. 2009 (KM5): ",
    "Muniz et al. 2009 (KM6): ",
    "Mansson et al. 2012 (KMN8): ",
    "Mansson et al. 2012 (KMN9): ",
    "Mansson et al. 2012 (KMN10): ",
    "Mansson et al. 2012 (KMN11): ",
    "Mansson et al. 2012 (KMN12): ",
    "Dorugade et al. 2010: ",
    "Dorugade et al. 2014: "
  )
  #print(t(krest),...)

  print(t(round(krest, 5)),...)
}
