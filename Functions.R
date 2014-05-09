
# Graduate School of Managament and Economics (GSME)
# Fama-French in Tehran Bourse
# Econometrics 1 - Dr.Barakchian
# Sepehr Ekbatani & Saber Ahmadi-Renani & Ehsan Azarmsa

# Load Libraries

library("car")
library("tseries")
library("lmtest")
library("strucchange")
library("bstats")


# Functions for lm ---------------------------------------------------------


# Coeficients

FFc <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  c <- coef(lm)
  return(c)
}

# R^2

FFr <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  r <- summary(lm)$r.squared
  return(r)
}

#std

FFstd <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  s <- summary(lm)
  c <- coef(s)
  std <- c[,"Std. Error"]
  return(std)
}

# t

FFt <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  s <- summary(lm)
  c <- coef(s)
  t <- c[,"t value"]
  return(t)
}

# Pr

FFPr <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  s <- summary(lm)
  c <- coef(s)
  Pr <- c[,"Pr(>|t|)"]
  return(Pr)
}

# lm only on Market (Table 4 of FF)


FFt4 <- function (X) {
  lm <- lm( X ~ market )
  s <- summary(lm)
  c <- coef(lm)
  names(c) <- c("Intercept(T4)","market(T4)")
  c["R_sq(T4)"] <- s$r.squared
  cs <- coef(s)
  t <- cs[,"t value"]
  names(t) <- c("t.Intercept(T4)","t.market(T4)")
  Pr <- cs[,"Pr(>|t|)"]
  names(Pr) <- c("Pr.Intercept(T4)","Pr.market(T4)")
  T4 <- c(c,t,Pr)
  return(T4)
}

# lm only on SMB & HML (Table 5 of FF)


FFt5 <- function (X) {
  lm <- lm( X ~ SMB+HML )
  s <- summary(lm)
  c <- coef(lm)
  names(c) <- c("Intercept(In)","SMB(T5)","HML(T5)")
  c["R_sq(T5)"] <- s$r.squared
  cs <- coef(s)
  t <- cs[,"t value"]
  names(t) <- c("t.Intercept(T5)","t.SMB(T5)","t.HML(T5)")
  Pr <- cs[,"Pr(>|t|)"]
  names(Pr) <- c("Pr.Intercept(T5)","Pr.SMB(T5)","Pr.HML(T5)")
  T5 <- c(c,t,Pr)
  return(T5)
}

# Functions for Tests -----------------------------------------------------


# F Test

FFf <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  s <- summary(lm)
  f <- s$fstatistic
  F.V <- f["value"]
  F.Pr <- pf(f[1],f[2],f[3],lower.tail = FALSE )
  F <- rbind(F.V,F.Pr)
  
  return(F)
}

# t test \beta market = 1

FFt1 <- function (X,n) {
  lm <- lm( X ~ market+SMB+HML )
  T1 <- t.test(X,y = rep(1,n))
  t1 <- T1$statistic
  t1.p <- T1$p.value
  T1 <- rbind(t1,t1.p)
  return(T1)
}


# White Test for Heteroskedasticity (bstats)

FFw <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  W <- bstats::white.test(lm)
  w <- W$statistic
  w.p <- W$p.value
  W <- rbind(w,w.p)
  return(W)
}

# BREUSCH-PAGAN Test for Heteroskedasticity

FFbp <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  BP <- bptest(lm)
  bp <- BP$statistic
  bp.p <- BP$p.value
  BP <- rbind(bp,bp.p)
  return(BP)
}

# Durbin-Watson test for Serial corrolation

FFdw <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  DW <- durbinWatsonTest(lm, max.lag = 1)
  dw <- DW$dw
  dw.p <- DW$p
  DW <- rbind(dw,dw.p)
  return(DW)
}

# BREUSCH-Godfrey Test for Serial corrolation

FFbg <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  BG <- bgtest(lm, order = 1)
  bg <- BG$statistic
  bg.p <- BG$p.value
  BG <- rbind(bg,bg.p)
  return(BG)
}

# Shapiro-Wilk Test for Normality of Residuls

FFsw <- function (X) {
  lm <- lm( X ~ market+SMB+HML )
  r <- resid(lm)
  SW <- shapiro.test(r)
  sw <- SW$statistic
  sw.p <- SW$p.value
  SW <- rbind(sw,sw.p)
  return(SW)
}

# Augmented Dickey-Fuller Test ( H0 : Unit Root , H1 : Stationary )


FFdf <- function(X) {
  DF <- adf.test(X)
  df <- DF$statistic
  df.Pr <- DF$p.value
  DF <- rbind(df,df.Pr)
  return(DF)
}

# Chow test for 1 Structional Brack

FFch <- function (X) {
  ch <- Fstats(X ~ market+SMB+HML ,from = 1 , to = NULL, vcov. = NULL  )
  CH <- ch$breakpoint
  return(CH)
}


# Functions for Plots -----------------------------------------------------


# QQ Norm Plot

FFqq <- function(X, ... ) {
  lm <- lm( X ~ market+SMB+HML  )
  r <- resid(lm)
  qqnorm(r, col = "blue")
  qqline(r, col = "red")
  return() }



# Reg Function ------------------------------------------------------------



FF <- function(F,n) {
  
  m <- n + 1
  # n = (ncol(F) - 4)
  # m = (ncol(F) - 3)
  
  attach(F)
  
  # coef
  
  X <- FFc(F[,2])
  FF_C <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFc(F[,i])
    FF_C[i-1] <- data.frame(X) }
  
  FF_C <- t(FF_C[,1:ncol(FF_C)])
  rownames(FF_C) <- rep(1: n )
  colnames(FF_C) <- c("Intercept","market" ,"SMB","HML")
  
  # R^2
  
  X <- FFr(F[,2])
  FF_R <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFr(F[,i])
    FF_R[i-1] <- data.frame(X) }
  
  FF_R <- t(FF_R[,1:ncol(FF_R)])
  rownames(FF_R) <- rep(1: n )
  colnames(FF_R) <- c("R_Sq")
  
  # std
  
  X <- FFstd(F[,2])
  FF_STD <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFstd(F[,i])
    FF_STD[i-1] <- data.frame(X) }
  
  FF_STD <- t(FF_STD[,1:ncol(FF_STD)])
  rownames(FF_STD) <- rep(1: n )
  colnames(FF_STD) <- c("std.intercept","std.market","std.SMB","std.HML")
  
  # t
  
  X <- FFt(F[,2])
  FF_T <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFt(F[,i])
    FF_T[i-1] <- data.frame(X) }
  
  FF_T <- t(FF_T[,1:ncol(FF_T)])
  rownames(FF_T) <- rep(1: n )
  colnames(FF_T) <- c("t.intercept","t.market","t.SMB","t.HML")
  
  # Pr
  
  X <- FFPr(F[,2])
  FF_PR <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFPr(F[,i])
    FF_PR[i-1] <- data.frame(X) }
  
  FF_PR <- t(FF_PR[,1:ncol(FF_PR)])
  rownames(FF_PR) <- rep(1: n )
  colnames(FF_PR) <- c("Pr.intercept","Pr.market","Pr.SMB","Pr.HML")
  
  # T4
  
  X <- FFt4(F[,2])
  FF_T4 <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFt4(F[,i])
    FF_T4[i-1] <- data.frame(X) }
  
  FF_T4 <- t(FF_T4[,1:ncol(FF_T4)])
  rownames(FF_T4) <- rep(1: n )
  
  # T5
  
  X <- FFt5(F[,2])
  FF_T5 <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFt5(F[,i])
    FF_T5[i-1] <- data.frame(X) }
  
  FF_T5 <- t(FF_T5[,1:ncol(FF_T5)])
  rownames(FF_T5) <- rep(1: n )
  
  # F
  
  X <- FFf(F[,2])
  FF_F <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFf(F[,i])
    FF_F[i-1] <- data.frame(X) }
  
  FF_F <- t(FF_F[,1:ncol(FF_F)])
  rownames(FF_F) <- rep(1: n )
  colnames(FF_F) <- c("F","F.Pr")
  
  # t = 1
  
  X <- FFt1(F[,2],n)
  FF_T1 <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFt1(F[,i],n)
    FF_T1[i-1] <- data.frame(X) }
  
  FF_T1 <- t(FF_T1[,1:ncol(FF_T1)])
  rownames(FF_T1) <- rep(1: n )
  colnames(FF_T1) <- c("t.market.1","Pr.t.market.1")
  
  # W test
  
  X <- FFw(F[,2])
  FF_W <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFw(F[,i])
    FF_W[i-1] <- data.frame(X) }
  
  FF_W <- t(FF_W[,1:ncol(FF_W)])
  rownames(FF_W) <- rep(1: n )
  colnames(FF_W) <- c("W Statistic","Pr.W")
  
  # BP test
  
  X <- FFbp(F[,2])
  FF_BP <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFbp(F[,i])
    FF_BP[i-1] <- data.frame(X) }
  
  FF_BP <- t(FF_BP[,1:ncol(FF_BP)])
  rownames(FF_BP) <- rep(1: n )
  colnames(FF_BP) <- c("BP Statistic","Pr.BP")
  
  # DW test
  
  X <- FFdw(F[,2])
  FF_DW <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFdw(F[,i])
    FF_DW[i-1] <- data.frame(X) }
  
  FF_DW <- t(FF_DW[,1:ncol(FF_DW)])
  rownames(FF_DW) <- rep(1: n )
  colnames(FF_DW) <- c("DW Statistic","Pr.DW")
  
  # BG test
  
  X <- FFbg(F[,2])
  FF_BG <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFbg(F[,i])
    FF_BG[i-1] <- data.frame(X) }
  
  FF_BG <- t(FF_BG[,1:ncol(FF_BG)])
  rownames(FF_BG) <- rep(1: n )
  colnames(FF_BG) <- c("BG Statistic","Pr.BG")
  
  # SW test
  
  X <- FFsw(F[,2])
  FF_SW <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFsw(F[,i])
    FF_SW[i-1] <- data.frame(X) }
  
  FF_SW <- t(FF_SW[,1:ncol(FF_SW)])
  rownames(FF_SW) <- rep(1: n )
  colnames(FF_SW) <- c("SW Statistic","Pr.SW")
  
  # DF test
  
  X <- FFdf(F[,2])
  FF_DF <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFdf(F[,i])
    FF_DF[i-1] <- data.frame(X) }
  
  FF_DF <- t(FF_DF[,1:ncol(FF_DF)])
  rownames(FF_DF) <- rep(1: n )
  colnames(FF_DF) <- c("DF Statistic","Pr.DF")
  
  # CH test
  
  X <- FFch(F[,2])
  FF_CH <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFch(F[,i])
    FF_CH[i-1] <- data.frame(X) }
  
  FF_CH <- t(FF_CH[,1:ncol(FF_CH)])
  rownames(FF_CH) <- rep(1: n )
  colnames(FF_CH) <- c("CH Breckpoint")
  
  
  ff <- cbind(FF_C,FF_R,FF_STD,FF_T,FF_PR,FF_T4,FF_T5,FF_F,FF_T1,FF_W,FF_BP,FF_DW
              ,FF_BG,FF_SW,FF_DF,FF_CH)
  detach(F)
  
  return(ff) }


# Partialing Out ----------------------------------------------------------

FFre <- function(X) {
  lm <- lm(X~SMB+HML)
  s <- summary(lm)
  RE <- s$residuals
     return(RE)     }


# Functions for Modification ----------------------------------------------

# Adding Inflation

FFin <- function (X) {
  lm <- lm( X ~ market+SMB+HML+IN )
  s <- summary(lm)
  c <- coef(lm)
  names(c) <- c("Intercept(In)","market(In)","SMB(In)","HML(In)","In(In)")
  c["R_sq"] <- s$r.squared
  c["D.R_Sq"] <- c["R_sq"] - FFr (X)
  cs <- coef(s)
  t <- cs[,"t value"]
  names(t) <- c("t.Intercept","t.market","t.SMB","t.HML","t.In")
  Pr <- cs[,"Pr(>|t|)"]
  names(Pr) <- c("Pr.Intercept","Pr.market","Pr.SMB","Pr.HML","Pr.In")
  IN <- c(c,t,Pr)
  return(IN)
}

# Reg Function for Modification

Mo <- function(F,n) {
  
  m <- n + 1
  
  attach(F)
  
  X <- FFin(F[,2])
  FF_IN <- data.frame(X)
  for ( i in 3: m ) {
    X <- FFin(F[,i])
    FF_IN[i-1] <- data.frame(X) }
  FF_IN <- t(FF_IN[,1:ncol(FF_IN)])
  rownames(FF_IN) <- rep(1: n )
  
  mo <- cbind(FF_IN)
  detach(F)
  
  return(mo) }
