setwd("C:/Users/limno/Google Drive/Dugouts/2018")
setwd("/Users/sydneyemmons/Google Drive/Dugouts/2018")

library(mgcv)
library(readxl)
library(ggplot2)
library(dplyr)
library(mgcViz)

library(randomcoloR)
n <- 20
palette <- distinctColorPalette(n)
pie(rep(1, n), col=palette)
col_vector <- unname(distinctColorPalette(n))

#themes ####
SAE_theme  <- theme_bw()
#Data ####

master <- read_xlsx("Dugout_master2018.xlsx", na = c("#VALUE!", "NA", "", "#N/A"))

master$Site_ID <- as.factor(master$Site_ID)
master <- mutate(master, sqrt_bf = sqrt(b.f.max))
master <- mutate(master, log_DIN = log10(DIN.ug.N.L))
master <- mutate(master, log_DOC = log10(DOC.uM))
master <- mutate(master, log_SRP = log10(SRP.mg.P.L))
master <- mutate(master, log_CHL = log10(Chla))
master <- mutate(master, log_NOx = log10(Nitrate_Nitrite.ug.N.L))
master <- mutate(master, log_Cond = log10(Surface_Cond))
master <- mutate(master, SRP.ug.P.L = SRP.mg.P.L*1000)
master <- mutate(master, sqrt_Temp = sqrt(Surface_Temp))

master2 <- filter(master, CH4.uM < 80) #removed very high methane outlier

#grouping high and low sites together to inform a non-normal distribution for N2O
master <- transform(master,
                     group = factor(ifelse(Site_ID %in% c("14A", "68A"), 
                                           "High", "Low")))


#pH only model - Adj R-square 0.764; deviance explained 89.1% ####
CO2_pH <- gam(CO2.uM ~ s(Surface_pH, k = 20) + s(Site_ID, bs = 're', k= 20),
              data = master, family = Gamma(link = 'log'), method = 'REML')

layout(matrix(1:4, ncol = 2))
gam.check(CO2_pH, rep = 100)
layout(1)

summary(CO2_pH)
plot.gam(CO2_pH, pages = 1)

CO2_pH <- getViz(CO2_pH)
check(CO2_pH,
      a.qq = list(method = "tnorm", CI = 'normal', 
                  a.cipoly = list(fill = "grey")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10, color = 'black', fill = 'grey'))


CO2_pH <- getViz(CO2_pH)
pH <- plot(sm(CO2_pH, 1), seWithMean = T)
pH <- pH + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(pH)) + 
  labs(y=expression(CO[2]*" (log("*mu*"M))"))  #+
#scale_y_continuous(lim = c(-2, 2), breaks = c(-2, -1, 0, 1, 2)) 

CCPH <- gridPrint(pH)

ggsave("CO2_pH_PEP.png", CCPH, height = 4, width = 4, units = 'in')

#CO2 seasonal control model
CO2_5 <- gam(CO2.uM ~ s(Site_ID, bs = 're', k = 30) + s(Surface_DO.sat, k = 8) +
               s(log_DIN, k = 8) + s(log_DOC, k =8) +
               s(log_SRP, k = 8) + s(log_CHL, k =8) + 
               s(sqrt_bf, k = 8) + s(d18O, k = 8), 
             data = master, family = Gamma(link = 'log'), select = TRUE,
             method = 'REML')

layout(matrix(1:4, ncol = 2))
gam.check(CO2_5, rep = 100)
layout(1)

summary(CO2_5)
plot(CO2_5, pages = 1, scheme = 1)

CO2_5 <- getViz(CO2_5)
check(CO2_5,
      a.qq = list(method = "tnorm", CI = 'normal', 
                  a.cipoly = list(fill = "grey")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10, color = 'black', fill = 'grey'))

#checking concurvity of the best fit model
concurvityCO2 <- concurvity(CO2_3, full = FALSE)
write.csv(concurvityCO2, "CO2_concurvity.csv")

#Making partial effects plots look better
CO2_5 <- getViz(CO2_5)

site <- plot(sm(CO2_5, 1))
site <- site + l_fitLine(colour = "black") + 
  l_ciLine(mul = 5, colour = "black", linetype = 2) + 
  l_points(shape = 19, size = 2, alpha = 0.1) +
  SAE_theme +
  theme(plot.title = element_blank()) +
  xlab("Gaussian Quantiles of Site") + 
  labs(y=expression(CO[2]*" (log("*mu*"M))"))


DO <- plot(sm(CO2_5, 2), seWithMean = T)
DO <- DO + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(O[2]*" (%)")) + 
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-2, 2), breaks = c(-2, -1, 0, 1, 2)) 

DIN <- plot(sm(CO2_5, 3), seWithMean = T)
DIN <- DIN + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(DIN*" (log"[10]*"("*mu*'g N L'^-1*'))')) +
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1.25), breaks = c(-1, 0, 1)) 


DOC <- plot(sm(CO2_5, 4), seWithMean = T)
DOC <- DOC + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(DOC*" (log"[10]*"("*mu*'M))')) + 
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1), breaks = c(-1, 0, 1)) 

SRP <- plot(sm(CO2_5, 5), seWithMean = T)
SRP <- SRP + l_ciPoly() +l_fitLine(colour = "black") + 
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(SRP*" (log"[10]*"("*mu*"g P L"^-1*"))")) +
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1), breaks = c(-1, 0, 1)) 

CHL <- plot(sm(CO2_5, 6), seWithMean = T)
CHL <-CHL + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Chl *" a (log"[10]*"("*mu*"g L"^-1*"))")) +
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1), breaks = c(-1, 0, 1)) 

BF <- plot(sm(CO2_5, 7), seWithMean = T)
BF <- BF + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Buoyancy*" Frequency ("*sqrt(s^-2)*")")) +
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1), breaks = c(-1, 0, 1)) 

D18O <- plot(sm(CO2_5, 8), seWithMean = T)
D18O <- D18O + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(delta^18*"O (%"[o]*")")) +
  labs(y=expression(CO[2]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1), breaks = c(-1, 0, 1)) 

gridPrint(site, DO, DIN, DOC, SRP, CHL, BF, D18O, ncol = 2)

CCDO <- gridPrint(DO)
CCDIN <- gridPrint(DIN)
CCDOC <- gridPrint(DOC)
CCSRP <- gridPrint(SRP)
CCCHL <- gridPrint(CHL)
CCBF <- gridPrint(BF)
CC18O <- gridPrint(D18O)

require(cowplot)
CO2 <- plot_grid(CCDO, 
                 CCDIN,
                 CCDOC,
                 CCSRP,
                 CCCHL,
                 CCBF, NULL,
                 CC18O, NULL,
                 ncol = 3, align = 'hv')

ggsave("CO2_PartialEffects.png", CO2,  height = 17.15, width = 19, units = 'cm')

#CH4 seasonal control model
CH4_10 <- gam(CH4.uM ~ s(Site_ID, bs = 're') + s(Surface_DO.sat, k = 8) +
                s(log_DIN, k = 8) + s(log_DOC, k = 8) +
                s(log_SRP, k = 8) + s(log_CHL, k = 8) + s(sqrt(Surface_Temp), k = 8)  + s(log_Cond, k = 8) ,
              data = master, family = Gamma(link = 'log'), select = T,
              method = 'REML')

layout(matrix(1:4, ncol = 2))
gam.check(CH4_10, rep = 1000)
layout(1)

summary(CH4_10)
plot.gam(CH4_10, pages = 1)

CH4_10 <- getViz(CH4_10)
check(CH4_10,
      a.qq = list(method = "tnorm", CI = 'normal', 
                  a.cipoly = list(fill = "grey")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10, color = 'black', fill = 'grey'))

#testing concurvity of the best AIC model
concurvityCH4 <- concurvity(CH4_10, full = FALSE)

write.csv(concurvityCH4, "CH4_concurvity.csv")

#Making partial effects plots look better
CH4_10 <- getViz(CH4_10)

csite <- plot(sm(CH4_10, 1))
csite <- csite + l_fitLine(colour = "black") + 
  l_ciLine(mul = 5, colour = "black", linetype = 2) + 
  l_points(shape = 19, size = 2, alpha = 0.1) +
  SAE_theme +
  theme(plot.title = element_blank()) +
  xlab("Gaussian Quantiles of Site") + 
  labs(y=expression(CH[4]*" (log("*mu*"M))"))


cDO <- plot(sm(CH4_10, 2), seWithMean = T)
cDO <- cDO + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(O[2]*" (%)")) + 
  labs(y=expression(CH[4]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-2, 2)) 

cDIN <- plot(sm(CH4_10, 3), seWithMean = T)
cDIN <- cDIN + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(DIN*" (log"[10]*"("*mu*'g N L'^-1*'))')) +
  labs(y=expression(CH[4]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 2)) 


cDOC <- plot(sm(CH4_10, 4), seWithMean = T)
cDOC <- cDOC + l_ciPoly() + l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(DOC*" (log"[10]*"("*mu*'M))')) + 
  labs(y=expression(CH[4]*" (log("*mu*"M))"))  

cSRP <- plot(sm(CH4_10, 5), seWithMean = T)
cSRP <- cSRP + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(SRP*" (log"[10]*"("*mu*"g P L"^-1*"))")) +
  labs(y=expression(CH[4]*" (log("*mu*"M))")) +
  scale_y_continuous(breaks = c(-1, 0, 1)) 

cCHL <- plot(sm(CH4_10, 6), seWithMean = T)
cCHL <- cCHL + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Chl *" a (log"[10]*"("*mu*"g L"^-1*"))")) +
  labs(y=expression(CH[4]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-1, 1), breaks = c(-1, 0, 1)) 

ctemp <- plot(sm(CH4_10, 7), seWithMean = T)
ctemp <- ctemp +l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Water*" Temperature ("*sqrt(degree*C)*")")) +
  labs(y=expression(CH[4]*" (log("*mu*"M))")) +
  scale_y_continuous(breaks = c(-1, 0, 1)) 

ccond <- plot(sm(CH4_10, 8), seWithMean = T)
ccond <- ccond + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Conductivity *" (log"[10]*"("*mu*"S cm"^-1*"))")) +
  labs(y=expression(CH[4]*" (log("*mu*"M))")) +
  scale_y_continuous(lim = c(-2, 2), breaks = c(-2, -1, 0, 1, 2)) 

gridPrint(csite, cDO, cDIN, cDOC, cSRP, cCHL, ctemp, ccond, ncol = 2)

CDO <- gridPrint(cDO)
CDIN <- gridPrint(cDIN)
CDOC <- gridPrint(cDOC)
CSRP <- gridPrint(cSRP)
CCHL <- gridPrint(cCHL)
CTEMP <- gridPrint(ctemp)
CCOND <- gridPrint(ccond)

require(cowplot)

CH4 <- plot_grid(CDO, 
                 CDIN,
                 CDOC,
                 CSRP,
                 CCHL,
                 CTEMP, NULL, 
                 CCOND,NULL, ncol = 3, align = 'hv')

ggsave("CH4_PartialEffects.png", CH4,  height = 17.15, width = 18.07, units = 'cm')

#N2O seasonal control models
N2O_7 <- gam(list(N2O.nM ~ s(Site_ID, bs = 're') + s(Surface_DO.sat, k = 8) +
                    s(log_DIN, k = 8) + s(log_DOC, k = 8) +
                    s(log_SRP, k = 8) + s(log_CHL, k = 8) + 
                    s(sqrt_bf, k =8) + s(Surface_pH, k = 8), 
                  ~ group), 
             data = master, family = gammals,
             method = "REML", select = T)

plot(N2O_7, pages = 1, scheme = 1, all.terms = TRUE, seWithMean = F)

anova(N2O_7)

layout(matrix(1:4, ncol = 2))
gam.check(N2O_7, rep = 100)
layout(1)

summary(N2O_7)
plot.gam(N2O_7, pages = 1)


N2O_7 <- getViz(N2O_7)
check(N2O_7,
      a.qq = list(method = "tnorm", CI = 'normal', 
                  a.cipoly = list(fill = "grey")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10, color = 'black', fill = 'grey'))


#checking concurvity of the best fit model
concurvityN2O <- concurvity(N2O_7, full = FALSE)

write.csv(concurvityN2O, "N2O_concurvity.csv")

#Making partial effects plots look better
N2O_7 <- getViz(N2O_7)

nsite <- plot(sm(N2O_7, 1))
nsite <- nsite + l_ciPoly() +l_fitLine(colour = "black") +
  l_points(shape = 19, size = 2, alpha = 0.1) +
  SAE_theme +
  theme(plot.title = element_blank()) +
  xlab("Gaussian Quantiles of Site") + 
  labs(y=expression(N[2]*"O (log(nM))"))


nDO <- plot(sm(N2O_7, 2), seWithMean = T)
nDO <- nDO +l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) + 
  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(O[2]*" (%)")) +
  labs(y=expression(N[2]*"O (log(nM))")) +
  scale_y_continuous(lim = c(-1.5, 1), breaks = c(-2, -1, 0, 1)) 

nDIN <- plot(sm(N2O_7, 3), seWithMean = T)
nDIN <- nDIN + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(DIN*" (log"[10]*"("*mu*'g N L'^-1*'))')) + 
  labs(y=expression(N[2]*"O (log(nM))")) +
  scale_y_continuous(lim = c(-1, 1.25), breaks = c( -1, 0, 1)) 


nDOC <- plot(sm(N2O_7, 4), seWithMean = T)
nDOC <- nDOC + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(DOC*" (log"[10]*"("*mu*'M))')) + 
  labs(y=expression(N[2]*"O (log(nM))")) +
  scale_y_continuous(lim = c(-1.25, 1.25), breaks = c( -1, 0, 1)) 

nSRP <- plot(sm(N2O_7, 5), seWithMean = T)
nSRP <- nSRP + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(SRP*" (log"[10]*"("*mu*"g P L"^-1*"))")) +
  labs(y=expression(N[2]*"O (log(nM))")) +
  scale_y_continuous(lim = c(-1.25, 1.25), breaks = c( -1, 0, 1)) 

nCHL <- plot(sm(N2O_7, 6), seWithMean = T)
nCHL <- nCHL +l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Chl *" a (log"[10]*"("*mu*"g L"^-1*"))")) + 
  labs(y=expression(N[2]*"O (log(nM))")) +
  scale_y_continuous(lim = c(-1.25, 1.25), breaks = c( -1, 0, 1)) 

nBF <- plot(sm(N2O_7, 7), seWithMean = T)
nBF <- nBF + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Buoyancy*" Frequency ("*sqrt(s^-2)*")")) +
  labs(y=expression(N[2]*"O (log(nM))")) +
  scale_y_continuous(lim = c(-1.25, 1.25), breaks = c( -1, 0, 1)) 

npH <- plot(sm(N2O_7, 8), seWithMean = T)
npH <- npH + l_ciPoly() +l_fitLine(colour = "black") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(pH)) +
  labs(y=expression(N[2]*"O (log(nM))"))  +
  scale_y_continuous(lim = c(-1.25, 1.25), breaks = c( -1, 0, 1)) 

group <- plot(pterm(N2O_7, 1))
group <- group + l_ciBar(colour = "black") + l_fitPoints(colour = "grey") +
  l_rug(sides = 'b', alpha = 0.8) +  SAE_theme +
  theme(plot.title = element_blank()) + 
  labs(x=expression(Group)) +
  labs(y=expression(N[2]*"O (log(nM))"))

gridPrint(nDO, nDIN, nDOC, nSRP, nCHL, nBF, ncol = 2)

NDO <- gridPrint(nDO)
NDIN <- gridPrint(nDIN)
NDOC <- gridPrint(nDOC)
NSRP <- gridPrint(nSRP)
NCHL <- gridPrint(nCHL)
NBF <- gridPrint(nBF)
NPH <- gridPrint(npH)

require(cowplot)

N2O <- plot_grid(NDO, 
          NDIN,
          NDOC,
          NSRP,
          NCHL,
          NBF, NULL, NPH, NULL, ncol = 3, align = 'hv')

ggsave("N2O_PartialEffects.png", N2O, height = 17.15, width = 19, units = 'cm')
