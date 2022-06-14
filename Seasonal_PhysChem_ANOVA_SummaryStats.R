getwd()

setwd("/Users/sydneyemmons/Google Drive/Dugouts/2018")
setwd("C:/Users/limno/Google Drive/Dugouts/2018")

library(readxl)
library(ggplot2)
library(stats)
library(agricolae)
library(vtable)
library(tidyverse)


dugouts <- read_xlsx("Dugout_master2018.xlsx")
master <- subset(dugouts, (Month == "April" | Month == "May" | Month == "July" | Month == "September"))

master <- master %>% 
  mutate(SRP.ug.P.L = SRP.mg.P.L*1000)

master %>% 
  summarise(mean = mean(SRP.ug.P.L, na.rm = TRUE),
            sd = sd(SRP.ug.P.L, na.rm = T))

master %>% 
  summarise(mean = mean(TN.ug.N.L, na.rm = TRUE),
            sd = sd(TN.ug.N.L, na.rm = T))

master %>% 
  summarise(mean = mean(NH3.ug.N.L, na.rm = TRUE),
            sd = sd(NH3.ug.N.L, na.rm = T))

#surface temperature 
aov.temp <- aov(Surface_Temp ~ Month, data = master)
summary(aov.temp)

tukey.temp <- HSD.test(aov.temp, trt = 'Month')
plot(tukey.temp)

#depth
aov.depth <- aov(Depth.m ~ Month, data = master)
summary(aov.depth)

tukey.depth <- HSD.test(aov.depth, trt = 'Month')


#secchi
aov.secchi <- aov(Secchi.m ~ Month, data = master)
summary(aov.secchi)

tukey.secchi <- HSD.test(aov.secchi, trt = 'Month')

plot(tukey.secchi)

#b.f. 
aov.bf <- aov(b.f.max ~ Month, data = master)
summary(aov.bf)

tukey.bf <- HSD.test(aov.bf, trt = 'Month')


#pH
aov.pH <- aov(Surface_pH ~ Month, data = master)
summary(aov.pH)

tukey.pH <- HSD.test(aov.pH, trt = 'Month')
plot(tukey.pH)


#dissolved oxygen
aov.do <- aov(Surface_DO.sat ~ Month, data = master)
summary(aov.do)

tukey.do <- HSD.test(aov.do, trt = 'Month')

aov.ddo <- aov(Deep_DO.sat ~ Month, data = master)
summary(aov.ddo)

tukey.ddo <- HSD.test(aov.ddo, trt = 'Month')

#conductivity
aov.cond <- aov(Surface_Cond ~ Month, data = master)
summary(aov.cond)

tukey.cond <- HSD.test(aov.cond, trt = 'Month')

plot(tukey.cond)
#salinity
aov.sal <- aov(Surface_Sal.ppt ~ Month, data = master)
summary(aov.sal)

tukey.sal <- HSD.test(aov.sal, trt = 'Month')
plot(tukey.sal)

#DIC
aov.dic <- aov(DIC.mg.L ~ Month, data = master)
summary(aov.dic)

tukey.dic <- HSD.test(aov.dic, trt = 'Month')
plot(tukey.dic)

#DOC
aov.doc <- aov(DOC.mg.L ~ Month, data = master)
summary(aov.doc)

tukey.doc <- HSD.test(aov.doc, trt = 'Month')

#TDP
aov.tdp <- aov(TP.mg.P.L ~ Month, data = master)
summary(aov.tdp)

tukey.tdp <- HSD.test(aov.tdp, trt = 'Month')

#SRP
aov.srp <- aov(SRP.mg.P.L ~ Month, data = master)
summary(aov.srp)

tukey.srp <- HSD.test(aov.tdp, trt = 'Month')

#TDN
aov.tdn <- aov(TN.ug.N.L ~ Month, data = master)
summary(aov.tdn)

tukey.tdn <- HSD.test(aov.tdn, trt = 'Month')

#NOx
aov.nox <- aov(Nitrate_Nitrite.ug.N.L ~ Month, data = master)
summary(aov.nox)

tukey.nox <- HSD.test(aov.nox, trt = 'Month')
plot(tukey.nox)

#NH3
aov.nh3 <- aov(NH3.ug.N.L ~ Month, data = master)
summary(aov.nh3)

tukey.nh3 <- HSD.test(aov.nh3, trt = 'Month')

#Chl
aov.chl <- aov(Chla ~ Month, data = master)
summary(aov.chl)

tukey.chl <- HSD.test(aov.chl, trt = 'Month')
plot(tukey.chl)

filtered1 <- master %>% 
  select(Month, Surface_Temp, Depth.m, Secchi.m, b.f.max, Surface_pH, Surface_DO.sat, Deep_DO.sat, Surface_Cond)

sumtable(data = filtered1, group = 'Month', group.test = TRUE, out = 'csv', file = 'YSI_Summary_Stats.csv')

filtered2 <- master %>% 
  select(Month, DIC.mg.L, DOC.mg.L, TP.mg.P.L, SRP.mg.P.L, TN.ug.N.L, Nitrate_Nitrite.ug.N.L, NH3.ug.N.L, Chla)

filtered2 <- filtered2 %>% mutate(TP.ug.P.L = TP.mg.P.L*1000,
                     SRP.ug.P.L = SRP.mg.P.L*1000)

sumtable(data = filtered2, group = 'Month', group.test = TRUE, out = 'csv', file = 'Summary_Stats.csv')


