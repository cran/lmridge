print.klmridge <- function(x,...) {
  cat("Ridge k from different Authors\n\n")

  krest <- cbind(
    kCV = x$kCV,   #minimum value of CV
    kGCV = x$kGCV, #minimum value of GCV
    mHKB = x$mHKB, #Thisted , 1976
    LW = x$LW,     #as in lm.ridge of MASS
    LW76 = x$LW76, #LW (1976)
    HKB = x$HKB,   #Hoerl and Kennard 1975
    DSk = x$DSk,   #Singh, Chaubey and Dwivedi (1986)
    Kibk = x$KibAM,#Kibria (2003)
    KibGM = x$KibGM,#Kibria 2003
    KibMED = x$KibMED, #Kibria (2003)
    KM2 = x$KM2,   # Muniz and Kibria 2009
    KM3 = x$KM3,   # Muniz and Kibria 2009
    KM4 = x$KM4,   # Muniz and Kibria 2009
    KM5 = x$KM5,   # Muniz and Kibria 2009
    KM6 = x$KM6,   # Muniz and Kibria 2009
    KMN8 = x$KMN8, # Muniz et al 2012
    KMN9 = x$KMN9, # Muniz et al 2012
    KMN10 = x$KMN10,# Muniz et al 2012
    KMN11 = x$KMN11,# Muniz et al 2012
    KMN12 = x$KMN12,# Muniz et al 2012
    KD   = x$KD,    #Dorugae et al 2010
    KAD4 = x$KAD4   # Dorugade et al. 2014
    #KL09=x$KL09
  )

  rownames(krest) <- "k values"

  colnames(krest) <- c(
    "Minimum CV at K",
    "Minimum GCV at K",
    "Thisted (1976):",
    "LW (lm.ridge)",
    "LW (1976)",
    "HKB (1975)",
    "Dwividi & Srivastava (1978): ",
    "Kibria (2003) (AM)",
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
