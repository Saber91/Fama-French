
# Graduate School of Managament and Economics (GSME)
# Fama-French in Tehran Bourse
# Econometrics 1 - Dr.Barakchian
# Sepehr Ekbatani & Saber Ahmadi-Renani & Ehsan Azarmsa

# Load Libraries


library("ggplot2")
library("reshape2")
library("lattice")
library("plyr")


# Read Data ---------------------------------------------------------------


F25 <- read.csv("25.csv", sep = "," , header = TRUE )
F16 <- read.csv("16.csv", sep = "," , header = TRUE )
F9 <- read.csv("9.csv", sep = "," , header = TRUE )
F6 <- read.csv("6.csv", sep = "," , header = TRUE )
FFp <- read.csv("FF.csv", sep = "," , header = TRUE )
Ad <- read.csv("Ad.csv", sep = "," , header = TRUE )
Firms <- read.csv("Firms.csv", sep = "," , header = TRUE )

F25 <- cbind(F25,Ad[-1])
F16 <- cbind(F16,Ad[-1])
F9 <- cbind(F9,Ad[-1])
F6 <- cbind(F6,Ad[-1])


# minus risk free

for ( i in 2: 27 ) { (F25[,i] = F25[,i] - F25$Rf ) }
for ( i in 2: 18 ) { (F16[,i] = F16[,i] - F16$Rf ) }
for ( i in 2: 11 ) { (F9[,i] = F9[,i] - F9$Rf ) }

# Run ---------------------------------------------------------------------


### Portfolios

FF25 <- FF(F25,25)
FF16 <- FF(F16,16)
FF9 <- FF(F9,9)

### Run Modifications

FF25 <- cbind(FF25, Mo(F25,25) )
FF16 <- cbind(FF16, Mo(F16,16) )
FF9 <- cbind(FF9, Mo(F25,9) )


# Output ------------------------------------------------------------------


write.csv(FF25, file = "R25.csv", row.names = TRUE, col.names = TRUE)
write.csv(FF16, file = "R16.csv", row.names = TRUE, col.names = TRUE)
write.csv(FF9, file = "R9.csv", row.names = TRUE, col.names = TRUE)


# t Statistics -----------------------------------------------------------


## Legends

Sig1 <- data.frame( y = c(-Inf, Inf), x = 1.785 , Significance = factor("a = 0.05") )
Sig2 <- data.frame( y = c(-Inf, Inf), x = -1.785 , Significance = factor("a = 0.05") )
Sig3 <- data.frame( y = c(-Inf, Inf), x = 2.485 , Significance = factor("a = 0.01") )
Sig4 <- data.frame( y = c(-Inf, Inf), x = -2.485 , Significance = factor("a = 0.01") )

Sigy1 <- data.frame( x = c(-Inf, Inf), y = 1.785 , Significance = factor("a = 0.05") )
Sigy2 <- data.frame( x = c(-Inf, Inf), y = -1.785 , Significance = factor("a = 0.05") )
Sigy3 <- data.frame( x = c(-Inf, Inf), y = 2.485 , Significance = factor("a = 0.01") )
Sigy4 <- data.frame( x = c(-Inf, Inf), y = -2.485 , Significance = factor("a = 0.01") )

# t market

FFDenMt <- data.frame(FF25[,"t.market"])
names(FFDenMt) <- c("t.market")
ggplot(FFDenMt, aes(x=t.market)) + geom_density() + theme_bw() +
  geom_vline(aes(xintercept= 1.708 , color= "a = 0.05" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "a = 0.05" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "a = 0.01" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "a = 0.01" ), linetype="dashed", size=1)+
  ggtitle("t Statistic of Cofficient of Market")
ggsave("t Statistic of Cofficient of Market.png")
dev.off()

# t market = 1

FFDenMt1 <- data.frame(FF25[,"t.market.1"])
names(FFDenMt1) <- c("t.market.1")
ggplot(FFDenMt1, aes(x=t.market.1)) + geom_density() + theme_bw() +
  geom_line(aes( x, y, color = Significance), Sig1,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sig2,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sig3,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sig4,linetype="dashed", size=1) +
  ggtitle("t Statistic of test Cofficient of Market = 1")
ggsave("t Statistic of test Cofficient of Market = 1.png")
dev.off()

# t market for FF

FFpDenMt <- data.frame(FFp[,"t.market"])
names(FFpDenMt) <- c("t.market")
ggplot(FFpDenMt, aes(x=t.market)) + geom_density() +
  geom_vline(aes(xintercept= 1.708 , color= "blue" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "blue" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "green" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "green" ), linetype="dashed", size=1) +
  ggtitle("t Statistic of Cofficient of Market in Fama-French(1993)")
ggsave("t Statistic of Cofficient of Market in Fama-French(1993).png")
dev.off()

# t SMB

FFDenS <- data.frame(FF25[,"t.SMB"])
names(FFDenS) <- c("t.SMB")
ggplot(FFDenS, aes(x=t.SMB)) + geom_density() +
  geom_vline(aes(xintercept= 1.708 , color= "blue" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "blue" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "green" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "green" ), linetype="dashed", size=1) +
  ggtitle("t Statistic of Cofficient of SMB")
ggsave("t Statistic of Cofficient of SMB.png")
dev.off()

# t SMB of FF

FFpDenS <- data.frame(FFp[,"t.SMB"])
names(FFpDenS) <- c("t.SMB")
ggplot(FFpDenS, aes(x=t.SMB)) + geom_density() +
  geom_vline(aes(xintercept= 1.708 , color= "blue" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "blue" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "green" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "green" ), linetype="dashed", size=1) +
  ggtitle("t Statistic of SMB in Fama-French(1993)")
ggsave("t Statistic of SMB in Fama-French(1993).png")
dev.off()

# t HML

FFDenH <- data.frame(FF25[,"t.HML"])
names(FFDenH) <- c("t.HML")
ggplot(FFDenH, aes(x=t.HML)) + geom_density() +
  geom_vline(aes(xintercept= 1.708 , color= "blue" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "blue" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "green" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "green" ), linetype="dashed", size=1) +
  ggtitle("t Statistic of HML")
ggsave("t Statistic of HML.png")
dev.off()

# t HML for FF

FFpDenH <- data.frame(FFp[,"t.HML"])
names(FFpDenH) <- c("t.HML")
ggplot(FFpDenH, aes(x=t.HML)) + geom_density() +
  geom_vline( aes(xintercept=mean(t.HML) , color= "red" )
              , linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 1.708 , color= "blue" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "blue" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "green" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "green" ), linetype="dashed", size=1) +
  ggtitle("t Statistic of HML in Fama-French(1993)")
ggsave("t Statistic of HML in Fama-French(1993).png")
dev.off()

# t intercept

FFDenI <- data.frame(FF25[,"t.intercept"])
names(FFDenI) <- c("t.intercept")
ggplot(FFDenI, aes(x=t.intercept)) + geom_density() + 
  geom_vline( aes(xintercept=mean(t.intercept) , color= "red" )
              , linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 1.708 , color= "blue" ), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept= -1.708 , color= "blue" ), linetype="dashed", size=1) +
  geom_vline(aes(xintercept= 2.485 , color= "green" ), linetype="dashed", size=1)+
  geom_vline(aes(xintercept= -2.485 , color= "green" ), linetype="dashed", size=1) +
  ggtitle("t Statistic of intercept")
ggsave("t Statistic of intercept.png")
dev.off()

# Density for t of Inflation

FFDenIn <- data.frame(FF25[,"t.In"])
names(FFDenIn) <- c("t.In")
ggplot(FFDenIn, aes(x=t.In)) + geom_density() + theme_bw() +
  geom_line(aes( x, y, color = Significance), Sig1,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sig2,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sig3,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sig4,linetype="dashed", size=1) +
  ggtitle("t Statistic of Cofficient of Inflation")
ggsave("t Statistic of Cofficient of Inflation.png")
dev.off()

### t for SMB of each Portfolios

FFportS <- data.frame( portfolios = factor(colnames(F25[,2:26])),
                       t.SMB = as.numeric(FF25[,"t.SMB"]) )
ggplot(FFportS, aes(x =portfolios, y=t.SMB)) + geom_point() + theme_bw() +
  scale_x_discrete(limits = FFportS$portfolios ) +
  geom_line(aes( x, y, color = Significance), Sigy1,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sigy2,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sigy3,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sigy4,linetype="dashed", size=1) + 
  ggtitle("t Statistic of Cofficient of SMB for each portfolios")
ggsave("t Statistic of Cofficient of SMB for each portfolios.png",width =12,height =10)
  dev.off()

### t for HML of each Portfolios

FFportH <- data.frame( portfolios = factor(colnames(F25[,2:26])),
                       t.HML = as.numeric(FF25[,"t.HML"]) )
ggplot(FFportH, aes(x =portfolios, y=t.HML)) + geom_point() + theme_bw() +
  scale_x_discrete(limits = FFportH$portfolios ) +
  geom_line(aes( x, y, color = Significance), Sigy1,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sigy2,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sigy3,linetype="dashed", size=1) +
  geom_line(aes( x, y, color = Significance), Sigy4,linetype="dashed", size=1) + 
  ggtitle("t Statistic of Cofficient of HML for each portfolios")
ggsave("t Statistic of Cofficient of HML for each portfolios.png",width =12,height =10)
dev.off()



# QQ Norm Plots -----------------------------------------------------------------

# 25

png(filename = "QQ Norm Plots - 25(1).png")
par(mfrow = c(3,5))
for (i in 2:16) 
{FFqq(F25[,i],F25)}
dev.off() 

png(filename = "QQ Norm Plots - 25(2).png")
par(mfrow = c(3,5))
for (i in 17:26) 
{FFqq(F25[,i],F25)}
dev.off() 

# 16

png(filename = "QQ Norm Plots - 16(1).png")
par(mfrow = c(3,4))
for (i in 2:13) 
{FFqq(F16[,i],F16)}
dev.off()

png(filename = "QQ Norm Plots - 16(2).png")
par(mfrow = c(3,4))
for (i in 14:17) 
{FFqq(F16[,i],F16)}
dev.off()

# 9

png(filename = "QQ Norm Plots - 9.png")
par(mfrow = c(3,3))
for (i in 2:10) 
{FFqq(F9[,i],F9)}
dev.off()


# Scatterplots ------------------------------------------------------------

### Time Series

# Firms in Time

Firms_m = melt(Firms)
names(Firms_m) <- c("Months","Firms","Return")
flevels <- levels(Firms_m$Months)
flevels[-c(1,13,25,37,49,61)] <- rep("",60)
flevels[c(1,13,25,37,49,61)] <- c(87,88,89,90,91,92) 
ggplot(data = Firms_m, aes(x=Months, y=Return)) +
  geom_point(shape = 1) + theme(legend.position="none") + 
  scale_x_discrete(breaks=levels(Firms_m$Months), labels=flevels) +
  scale_y_continuous(limits=c(-0.25,1)) +
  ggtitle("Return of Firms")
ggsave("Return of Firms.png")
dev.off()

# 25 Portfolios in Time

F25_m = melt(F25[1:26])
names(F25_m) <- c("Months","Portfolio","Return")
flevels <- levels(F25_m$Month)
flevels[-c(1,13,25,37,49,61)] <- rep("",60)
flevels[c(1,13,25,37,49,61)] <- c(87,88,89,90,91,92) 
ggplot(data = F25_m, aes(x=Months, y=Return)) +
  geom_point(aes(colour=Portfolio)) + 
  scale_x_discrete(breaks=levels(F25_m$Months), labels=flevels) +
  ggtitle("Return of Portfolios")
ggsave("Return of Portfolios.png")
dev.off()

### Market \Beta

# Firms VS. Market

Firms$Market <- F25$market
Firms_M <- melt(Firms,id = c("month","Market"))
names(Firms_M) <- c("Months","Market","Firms","Return")
ggplot(data = Firms_M, aes(x=Market, y=Return)) +
geom_point(aes(colour=Months)) + theme(legend.position="none") + 
scale_y_continuous(limits=c(-0.25,1)) +
  ggtitle("Return of Firms and Market") +
  geom_smooth(method=lm , se = FALSE)
ggsave("Return of Firms and Market.png")
dev.off()

# 25 Portfolios VS. Market

F25_P <- melt(F25[,1:27],id = c("month","market"))
names(F25_P) <- c("Months","Market","Portfolio","Return")
ggplot(data = F25_P, aes(x=Market, y=Return)) +
geom_point(aes(colour=Portfolio))  +
  geom_smooth(method="lm",se=FALSE) +
  ggtitle("Return of Portfolios and Market")
ggsave("Return of Portfolios and Market.png",width = 12,height =10)
dev.off()

# 25 Portfolios VS. Market (Partialing out SMB and HML)

attach(F25)
X <- FFre(F25[,2])
FF_RE25 <- data.frame(X)
for ( i in 3: 26 ) {
  X <- FFre(F25[,i])
  FF_RE25[i-1] <- data.frame(X) }
detach(F25)
FF_RE25 <- cbind(F25$month,FF_RE25,F25$market)
colnames(FF_RE25) <- c("month",colnames(F25[2:26]),"market")

FF_RE <- melt(FF_RE25,id = c("month","market"))
names(FF_RE) <- c("Months","Market","Portfolio","Return")
ggplot(data = FF_RE, aes(x=Market, y=Return)) +
  geom_point(aes(colour=Portfolio))  +
  stat_smooth(method="lm" , se = FALSE) +
  ggtitle("Return of Portfolios and Market (Partialing out SMB,HML)")
ggsave("Return of Portfolios and Market (Partialing out SMB,HML).png", width=12, height=10)
dev.off()


# Corrolations ------------------------------------------------------------

# 
# library(ellipse)
# Cor <- cor(F25[c("SMB","HML","market")])
# #plotcorr(Cor)
# colorfun <- colorRamp(c("red","white","green"), space="Lab")
# plotcorr(Cor, col=rgb(colorfun((Cor+1)/2), maxColorValue=255))
# 
# 
# panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
# {
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- abs(cor(x, y))
#   txt <- format(c(r, 0.123456789), digits=digits)[1]
#   txt <- paste(prefix, txt, sep="")  
#   text(0.5, 0.5, txt)
# }
# 
# panel.hist <- function(x, ...)
# {
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(usr[1:2], 0, 1.5) )
#   h <- hist(x, plot = FALSE)
#   breaks <- h$breaks; nB <- length(breaks)
#   y <- h$counts; y <- y/max(y)
#   rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
# }
# 
# pairs(F25[c("SMB","HML","market")] #, col = c('green','red','blue')
#      , diag.panel=panel.hist ,lower.panel=panel.smooth, upper.panel=panel.cor, 
#       pch=20, main="Corolation of Market, SMB and HML")
# 
# pairs(F25[c("SMB","HML","market")] , col = c('green','red','blue')
#       ,lower.panel=panel.smooth, upper.panel=panel.cor, 
#       pch=20, main="Corolation of Market, SMB and HML")
# 
# scatterplotmatrix(F25[c("SMB","HML","market")],
#                    main="Corolation of Market, SMB and HML")
# 
# plotmatrix(F25[c("SMB","HML","market")], colour="red") +
#   geom_smooth(method="lm")
# 
# library("GGally")
# ggpairs(F25[c("SMB","HML","market")], title="Corolation of Market, SMB and HML",
#         upper = list(continuous = "cor"),
#         lower = list(continuous = "points"),
#         diag = list(continuous = "density")
#         )


# Coefficients ------------------------------------------------------------


# Comparing Market Beta of AEA,FF

FFDenM <- data.frame(cbind(FF25[,2],FFp[,2]))
names(FFDenM) <- c("AEA","FF")
FFDenM <- melt(FFDenM)
names(FFDenM) <- c("Market","Coefficient")
cdf <- ddply(FFDenM, "Market", summarise, Coefficient.mean=mean(Coefficient))
ggplot(FFDenM, aes( x= Coefficient , colour = Market)) + geom_density() + 
  geom_vline(data=cdf, aes(xintercept=Coefficient.mean,  colour=Market)
              , linetype="dashed", size=1) +
  ggtitle("Comparing Market Beta of AEA and FF")
ggsave("\beta of FF and AEA.png")
dev.off()

# Comparing Market Beta of AEA,AEA(T4)

FFDenM4 <- data.frame(cbind(FF25[,"market(T4)"],FF25[,"market"]))
names(FFDenM4) <- c("only market","with SMB,HML")
FFDenM4 <- melt(FFDenM4)
names(FFDenM4) <- c("Regression","Coefficient")
cdf <- ddply(FFDenM4, "Regression", summarise, Coefficient.mean=mean(Coefficient))
ggplot(FFDenM4, aes( x= Coefficient , colour = Regression)) + geom_density() + 
  geom_vline(data=cdf, aes(xintercept=Coefficient.mean,  colour=Regression)
             , linetype="dashed", size=1) +
  ggtitle("Comparing Market Beta of with and without SMB,HML")
ggsave("beta of with and without SMB and HML.png",width=12,height=7)
dev.off()

# \Beta Market for each portfolios

FFpormb <- data.frame( portfolios = factor(colnames(F25[,2:26])),
                       market = as.numeric(FF25[,"market"]) , market.T4 = as.numeric(FF25[,"market(T4)"]) )
FFpormb <- melt(FFpormb)
names(FFpormb) <- c("portfolios","Regression","Cofficient")
FFpormb$Cofficient <- as.numeric(FFpormb$Cofficient)
ggplot(FFpormb, aes(x = portfolios, y = Cofficient, group = Regression , colour = Regression)) + 
  geom_point() + geom_line() + theme_bw() +
  scale_x_discrete(labels = FFpormb$portfolios ) +
  geom_hline(yintercept = 1,color='blue',linetype="dashed", size=1) +
  ggtitle("Cofficient of market for each portfolios")
ggsave("Cofficient of market for each portfolios.png",width =12,height =10)
dev.off()



# SMB of FF

FFpSMB <- data.frame(FFp$SMB)
FFpSMB$Size <- c(rep("1",5),rep("2",5),rep("3",5),rep("4",5),rep("5",5))
Group <- c("1","2","3","4","5")
FFpSMB$BE_ME <- c(rep(Group,5))
FFpSMB <- melt(FFpSMB, id = c("Size","BE_ME"))
FFpSMB$variable <- NULL
colnames(FFpSMB)[3] <- "SMB"
ggplot(data=FFpSMB, aes(x=BE_ME, y=SMB, group=Size , colour = Size)) +
  geom_line() + geom_point() +
  scale_x_discrete(breaks= Group , labels= c("Low","2","3","4","High")) +
  scale_colour_discrete(breaks= Group,labels= c("Small","2","3","4","Big"))
ggsave("SMB of FF.png")
dev.off()

# SMB of AEA

FFSMB <- data.frame(FF25[,"SMB"])
FFSMB$Size <- c(rep("1",5),rep("2",5),rep("3",5),rep("4",5),rep("5",5))
Group <- c("1","2","3","4","5")
FFSMB$BE_ME <- c(rep(Group,5))
FFSMB <- melt(FFSMB, id = c("Size","BE_ME"))
FFSMB$variable <- NULL
colnames(FFSMB)[3] <- "SMB"
ggplot(data=FFSMB, aes(x=BE_ME, y=SMB, group=Size , colour = Size)) +
  geom_line() + geom_point() +
  scale_x_discrete(breaks= Group , labels= c("Low","2","3","4","High")) +
  scale_colour_discrete(breaks= Group,labels= c("Small","2","3","4","Big"))
ggsave("SMB of AEA.png")
dev.off()

# HML of FF

FFpHML <- data.frame(FFp$HML)
FFpHML$Size <- c(rep("1",5),rep("2",5),rep("3",5),rep("4",5),rep("5",5))
Group <- c("1","2","3","4","5")
FFpHML$BE_ME <- c(rep(Group,5))
FFpHML <- melt(FFpHML, id = c("Size","BE_ME"))
FFpHML$variable <- NULL
colnames(FFpHML)[3] <- "HML"
print( ggplot(data=FFpHML, aes(x=Size, y=HML, group=BE_ME , colour = BE_ME)) +
  geom_line() + geom_point() +
  scale_x_discrete(breaks= Group , labels= c("Small","2","3","4","Big")) +
  scale_colour_discrete(breaks= Group,labels= c("Low","2","3","4","High"))  )
ggsave("HML of FF.png")
dev.off()

# HML of AEA

FFHML <- data.frame(FF25[,"HML"])
FFHML$Size <- c(rep("1",5),rep("2",5),rep("3",5),rep("4",5),rep("5",5))
Group <- c("1","2","3","4","5")
FFHML$BE_ME <- c(rep(Group,5))
FFHML <- melt(FFHML, id = c("Size","BE_ME"))
FFHML$variable <- NULL
colnames(FFHML)[3] <- "HML"
ggplot(data=FFHML, aes(x=Size, y=HML, group=BE_ME , colour = BE_ME)) +
  geom_line() + geom_point() +
  scale_x_discrete(breaks= Group , labels= c("Small","2","3","4","Big")) +
  scale_colour_discrete(breaks= Group,labels= c("Low","2","3","4","High"))
ggsave("HML of AEA.png")
dev.off()


