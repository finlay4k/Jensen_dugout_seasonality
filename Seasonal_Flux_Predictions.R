getwd()
setwd("/Users/sydneyemmons/Google Drive/Dugouts/2018")
setwd("C:/Users/limno/Google Drive/Dugouts/2018")

library(mgcv)
library(readxl)
library(ggplot2)
library(dplyr)
library(mgcViz)
library(tidymv)
library(viridis)
library(gratia)
library(tibble)

library(randomcoloR)
n <- 20
palette <- distinctColorPalette(n)
pie(rep(1, n), col=palette)
col_vector <- unname(distinctColorPalette(n))


SAE_theme  <- theme_bw(base_size = 12) 

master <- read_xlsx("Dugout_master2018.xlsx", na = c("#VALUE!", "NA", "", "#N/A"))
master$Site_ID <- as.factor(master$Site_ID)

#CO2 seasonal model - Site ID and DOY####
CO2_seasonal_gam <- gam(CO2.uM ~ s(DOY, k = 20) + s(DOY, Site_ID, bs = 'fs', k = 3),
                        data = master, family = Gamma(link = 'log'), method = 'REML')

#Predicted values from CO2 GAM plot ####
df.co2 <- with(master, expand.grid(DOY = seq(min(DOY), max(DOY), length = 158),
                               Site_ID=levels(Site_ID)))

ilink.co2 <- family(CO2_seasonal_gam)$linkinv

L.co2 <- predict(CO2_seasonal_gam, newdata = df.co2, type = "link", se.fit = T)

pred.co2 <- cbind(df.co2, L.co2)

pred.co2 <- transform(pred.co2, fitted = ilink.co2(fit), upper = ilink.co2(fit + (2*se.fit)), lower = ilink.co2(fit - (2*se.fit)))

CO2_2018 <- ggplot(pred.co2, aes(x = DOY, y = fitted, group = Site_ID, color = Site_ID))+
  geom_line(size = 0.75)+
  geom_hline(yintercept=18.09, color = "black", linetype = "dashed", size = 1) +
  labs(y=expression(Predicted*" CO"[2]*" ("*mu*"M)")) +
  SAE_theme +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank()) +
  scale_colour_manual(values = col_vector) 

CO2_2018 

#Read in 2018 seasonal flux calcs to calcualte flux

calcs <- read_xlsx("Dugout_GHG_calcs_v4_2018.xlsx", na = c("#VALUE!", "NA", "", "#N/A")) #raw calculations with 4 replicates per site

k600.co2.avg <- mean(calcs$`k600-CO2...67`)
CO2air.avg <- mean(calcs$CO2_uM_atm)

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.co2 <- pred.co2 %>%
  mutate(Flux_CC = (fitted-CO2air.avg)*k600.co2.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.CO2.site <- pred.co2 %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = sum(Flux_CC))


#CH4 seasonal model - Site ID and DOY####
master3 <- filter(master, CH4.uM < 80) #removed outlier
CH4_seasonal_gam <- gam(CH4.uM ~ s(DOY, k = 10) + s(DOY, Site_ID, bs = 'fs', k = 3),
                        data = master3, family = Gamma(link = "log"), method = 'REML')

#Predicted values from CH4 GAM plot ####
df.ch4 <- with(master3, expand.grid(DOY = seq(min(DOY), max(DOY), length = 158),
                               Site_ID=levels(Site_ID)))

ilink.ch4 <- family(CH4_seasonal_gam)$linkinv

L.ch4 <- predict(CH4_seasonal_gam, newdata = df.ch4, type = "link", se.fit = T)

pred.ch4 <- cbind(df.ch4, L.ch4)

pred.ch4 <- transform(pred.ch4, fitted = ilink.ch4(fit), upper = ilink.ch4(fit + (2*se.fit)), lower = ilink.ch4(fit - (2*se.fit)))

CH4_2018 <- ggplot(pred.ch4, aes(x = DOY, y = fitted, group = Site_ID, color = Site_ID))+
  geom_line(size = 0.75)+
  scale_colour_manual(values = col_vector)+
  geom_hline(yintercept=0.003, color = "black", linetype = "dashed", size = 1) +
  labs(y=expression(Predicted*" CH"[4]*" ("*mu*"M)")) +
  SAE_theme +
  theme(legend.position = "none",
        axis.title.x = element_blank()) 

CH4_2018 

#calcualte seasonal k600 and ch4 air con

k600.ch4.avg <- mean(calcs$`k600-CH4...68`)
CH4air.avg <- mean(calcs$CH4_uM_atm)

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.ch4 <- pred.ch4 %>%
  mutate(Flux_CC = (fitted-CH4air.avg)*k600.ch4.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.ch4.site <- pred.ch4 %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = sum(Flux_CC))


# calculating co2 equivalence for CH4
pred.ch4 <- pred.ch4 %>% 
  mutate(CO2_equiv = ifelse(Flux_CC > 0, 45*Flux_CC, 203*Flux_CC))


#N2O seasonal model - Site ID and DOY####
master <- transform(master,
                    group = factor(ifelse(Site_ID %in% c("14A", "68A"), 
                                          "A", "B")))

N2O_seasonal_gls <- gam(list(N2O.nM ~ s(DOY, k = 10) + s(DOY, Site_ID, bs = 'fs', k = 3),
                             ~ group), 
                        data = master, family = gammals,
                        method = "REML") 

#Predicted values from CO2 GAM plot ####
df.n2o <- with(master, expand.grid(DOY = seq(min(DOY), max(DOY), length = 158), 
                                       Site_ID=levels(Site_ID),
                                       group = levels(group)))

L.n2o <- predict(N2O_seasonal_gls, newdata = df.n2o, type = "link", se.fit = T)

pred.n2o <- cbind(df.n2o, logmean = L.n2o[["fit"]][,1], selogmean = L.n2o[["se.fit"]][,1])

pred.n2o <- transform(pred.n2o, fitted = exp(logmean),
                   upper = exp(logmean + (2*selogmean)),
                   lower = exp(logmean - (2*selogmean)))

N2O_2018 <- ggplot(pred.n2o, aes(x = DOY, y = fitted, group = Site_ID, color = Site_ID))+
  geom_line(size = 0.75)+
  scale_colour_manual(values = col_vector)+
  geom_hline(yintercept=10.99, color = "black", linetype = "dashed", size = 1) +
  labs(y=expression(Predicted*" N"[2]*"O (nM)")) +
  SAE_theme +
  theme(legend.position = "none") +
  xlab("Day of Year") 

N2O_2018

#calculate seasonal average k600 and n2o air concentration

k600.n2o.avg <- mean(calcs$`k600-N2O...69`)
n2oair.avg <- mean(calcs$N2O_nM_atm)

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.n2o <- pred.n2o %>% filter(group == "A") %>% 
  mutate(Flux_CC = (fitted-n2oair.avg)*k600.n2o.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.n2o.site <- pred.n2o %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = (sum(Flux_CC)/1000)) #convert umol to mmol to compare to CH4 and CO2 directly


# calculating co2 equivalence for N2O
pred.n2o <- pred.n2o %>%
  mutate(CO2_equiv = ifelse(Flux_CC > 0, 270*Flux_CC, 349*Flux_CC))


# combine CO2, CH4, and N2O sum dfs into one file and sum

#keeps the contribution of each gas
TotalCO2Equiv.gas <- bind_rows(list(CO2 = co2eq.CO2.site, CH4 = co2eq.ch4.site, N2O = co2eq.n2o.site), .id = "id") %>% 
  group_by(Site_ID, id) %>% 
  summarise(CO2Equiv.gas = sum(Co2eqTotal))

#loses the contribution of each gas and just gives total per site
TotalCO2Equiv.site <- bind_rows(list(CO2 = co2eq.CO2.site, CH4 = co2eq.ch4.site, N2O = co2eq.n2o.site), .id = "id") %>% 
  group_by(Site_ID) %>% 
  summarise(CO2Equiv.site = sum(Co2eqTotal))

#plot total CO2 equiv 

#shows just the total by site
co2.eq.site <-  ggplot(TotalCO2Equiv.site, aes(x = reorder(Site_ID, CO2Equiv.site), y = CO2Equiv.site))+
  geom_bar(stat = "identity", alpha = 0.75)+
  ylab(expression("Total Seasonal CO"[2]*'-eq Flux (mmol m'^2*")")) +
  SAE_theme +
  theme(legend.position = "top") +
  xlab("Site") +
  theme(axis.title.x = element_blank()) +
  annotate("text", x = 1, y = 10250, label = "a", size = 8)
co2.eq.site

#working to get this to show the total per site with the contribution of each gas 
co2.eq.gas <-  ggplot(TotalCO2Equiv.gas, aes(x =factor(Site_ID, levels = c('23A', '32C', '56A', '32B', '4C', '4D', 
                                                                              '61C', '4A','62C', '62B', '56B', '14B',
                                                                              '66B', '32A', '66C', '68A', '14A', '61B',
                                                                              '62E', '66A')), y = CO2Equiv.gas, fill = id))+ #reorder the sites to match the first panel
  geom_bar(position = 'dodge', stat = "identity", alpha = 0.75, color = 'black')+
  ylab(expression("Total Seasonal CO"[2]*'-eq Flux (mmol m'^2*")")) +
  SAE_theme +
  xlab("Site") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("#FDE725FF", "#2D708EFF", "#55C667FF"), 
                    breaks=c("CH4", "CO2", "N2O"),
                    labels=c(expression(paste(CH[4])), expression(paste(CO[2])), expression(paste(N[2]*'O')))) +
  theme(legend.position=c(0.1, 0.88),
        legend.background = element_blank()) +
  annotate("text", x = 1, y = 10250, label = "b", size = 8)

co2.eq.gas            

require(cowplot)

both <- plot_grid(co2.eq.site, co2.eq.gas, ncol = 1, align = 'hv')
both

library(lubridate)
library(tidyr)

#compare 3 methods - predicted, calculated from one value in july, calculated from 4 sampling days 
predicted <- TotalCO2Equiv.site

#pull out flux data from calcs df to prepare for calculations
flux <- calcs %>% select("Site_ID", "Date", "Flux_CO2_CC", "Flux_CH4_CC", "Flux_N2O_CC") %>% drop_na() %>% mutate(Date = ymd(Date)) %>% 
  group_by(Site_ID, Date) %>% 
  summarize(Flux_CO2_CC = mean(Flux_CO2_CC),
            Flux_CH4_CC = mean(Flux_CH4_CC),
            Flux_N2O_CC = mean(Flux_N2O_CC)) %>% 
  mutate(Month = month(Date)) %>% 
  mutate(CO2eq.co2 = Flux_CO2_CC, 
         CO2eq.ch4 = ifelse(Flux_CH4_CC > 0, 45*Flux_CH4_CC, 203*Flux_CH4_CC),
         CO2eq.n2o = (ifelse(Flux_N2O_CC > 0, 270*Flux_N2O_CC, 349*Flux_N2O_CC))/1000)

#calculate july only
#pull out july data
july.flux <- flux %>% filter(Month == 7)

july.calc <- july.flux %>% mutate(CO2eq.co2.year = CO2eq.co2*158,   #157 days in sampling period
                                  CO2eq.ch4.year = CO2eq.ch4*158,
                                  CO2eq.n2o.year = CO2eq.n2o*158) %>% 
  group_by(Site_ID) %>% 
  mutate(CO2Equiv.site = CO2eq.co2.year+CO2eq.ch4.year+CO2eq.n2o.year) %>% 
  select("Site_ID", "CO2Equiv.site")

#calculate seasonal 
#filter out months with only 5 observations - so june and august - so all sites can be treated the same
seasonal.flux <- flux %>% filter(Month == 4|Month == 5|Month == 7|Month == 9)

#create column sorting into ice-off, spring, summer and fall sampling periods using the date
#Ice off - 21 days between beginning of sampling and start of the may sampling (Apr 24 to May 14)
#spring - 30 days between start of may sampling and mid-point between may and july sampling (May 15 to Jun 13)
#Summer - 68 days between mid-point of may and july sampling and mid-point between july and september sampling (Jun 14 to Aug 20)
#Fall - 39 days between mid-point of july and septebmer sampling and the end of the september sampling (Aug 21 to Sept 28)
seasonal.flux <- seasonal.flux %>% 
  mutate(Season = ifelse(Date >= "2018-04-20" & Date <= "2018-05-14", "Ice-off",
                         ifelse(Date >= "2018-05-14" & Date < "2018-06-13", "Spring",
                                ifelse(Date >= "2018-06-14" & Date < "2018-08-20", "Summer",
                                       ifelse(Date >= "2018-08-21" & Date < "2018-09-28", "Fall", "Ice-off"))))) %>% 
  mutate(co2eq.co2.year = ifelse(Season == "Ice-Off", CO2eq.co2*21,
                            ifelse(Season == "Spring", CO2eq.co2*30,
                              ifelse(Season == "Summer", CO2eq.co2*68,
                                ifelse(Season == "Fall", CO2eq.co2*39, CO2eq.co2*21))))) %>% 
  mutate(co2eq.ch4.year = ifelse(Season == "Ice-Off", CO2eq.ch4*21,
                            ifelse(Season == "Spring", CO2eq.ch4*30,
                              ifelse(Season == "Summer", CO2eq.ch4*68,
                                ifelse(Season == "Fall", CO2eq.ch4*39, CO2eq.ch4*21))))) %>% 
  mutate(co2eq.n2o.year = ifelse(Season == "Ice-Off", CO2eq.n2o*21,
                            ifelse(Season == "Spring", CO2eq.n2o*30,
                              ifelse(Season == "Summer", CO2eq.n2o*68,
                                ifelse(Season == "Fall", CO2eq.n2o*39, CO2eq.n2o*21)))))

seasonal.calc <- seasonal.flux %>%   group_by(Site_ID) %>%
  mutate(CO2Equiv= co2eq.co2.year+co2eq.ch4.year+co2eq.n2o.year) %>% 
  summarize(CO2Equiv.site = sum(CO2Equiv)) %>% 
  select("Site_ID", "CO2Equiv.site")

D23A <- seasonal.calc %>% filter(Site_ID == "23A") #methane in the spring is very high 

#combine three methods into one df to be able to compare and plot
compare.methods <- bind_rows(list(Predicted = predicted, "July Only" = july.calc, Seasonal = seasonal.calc), .id = "id")

co2.eq.methods <-  ggplot(compare.methods, aes(x =factor(Site_ID, levels = c('23A', '32C', '56A', '32B', '4C', '4D', 
                                                                           '61C', '4A','62C', '62B', '56B', '14B',
                                                                           '66B', '32A', '66C', '68A', '14A', '61B',
                                                                           '62E', '66A')), y = CO2Equiv.site, fill = id))+ #reorder the sites to match the other CO2 equiv plots from lines 191 and 202
  geom_bar(position = 'dodge', stat = "identity", alpha = 0.75, color = 'black')+
  ylab(expression("Total Seasonal CO"[2]*'-eq Flux (mmol m'^2*")")) +
  SAE_theme +
  xlab("Site") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("#FDE725FF", "#2D708EFF", "#55C667FF")) +
  theme(legend.position=c(0.2, 0.88),
        legend.background = element_blank()) 

co2.eq.methods

#redo GAM prediction using the upper confidence intervals to see how that changes 
  
#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.co2.upper <- pred.co2 %>%
  mutate(Flux_CC = (upper-CO2air.avg)*k600.co2.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.CO2.site.upper <- pred.co2.upper %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = sum(Flux_CC))

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.ch4.upper <- pred.ch4 %>%
  mutate(Flux_CC = (upper-CH4air.avg)*k600.ch4.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.ch4.site.upper <- pred.ch4.upper %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = sum(Flux_CC))


# calculating co2 equivalence for CH4
pred.ch4.upper <- pred.ch4.upper %>% 
  mutate(CO2_equiv = ifelse(Flux_CC > 0, 45*Flux_CC, 203*Flux_CC))

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.n2o.upper <- pred.n2o %>% filter(group == "A") %>% 
  mutate(Flux_CC = (upper-n2oair.avg)*k600.n2o.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.n2o.site.upper <- pred.n2o.upper %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = (sum(Flux_CC)/1000)) #convert umol to mmol to compare to CH4 and CO2 directly


# calculating co2 equivalence for N2O
pred.n2o.upper <- pred.n2o.upper %>%
  mutate(CO2_equiv = ifelse(Flux_CC > 0, 270*Flux_CC, 349*Flux_CC))

# combine CO2, CH4, and N2O sum dfs into one file and sum

#keeps the contribution of each gas
TotalCO2Equiv.gas.upper <- bind_rows(list(CO2 = co2eq.CO2.site.upper, CH4 = co2eq.ch4.site.upper, N2O = co2eq.n2o.site.upper), .id = "id") %>% 
  group_by(Site_ID, id) %>% 
  summarise(CO2Equiv.gas = sum(Co2eqTotal))

#loses the contribution of each gas and just gives total per site
TotalCO2Equiv.site.upper <- bind_rows(list(CO2 = co2eq.CO2.site.upper, CH4 = co2eq.ch4.site.upper, N2O = co2eq.n2o.site.upper), .id = "id") %>% 
  group_by(Site_ID) %>% 
  summarise(CO2Equiv.site = sum(Co2eqTotal))

upper.predicted <- TotalCO2Equiv.site.upper

#redo GAM prediction using the lower confidence intervals to see how that changes 

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.co2.lower <- pred.co2 %>%
  mutate(Flux_CC = (lower-CO2air.avg)*k600.co2.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.CO2.site.lower <- pred.co2.lower %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = sum(Flux_CC))

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.ch4.lower <- pred.ch4 %>%
  mutate(Flux_CC = (lower-CH4air.avg)*k600.ch4.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.ch4.site.lower <- pred.ch4.lower %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = sum(Flux_CC))


# calculating co2 equivalence for CH4
pred.ch4.lower <- pred.ch4.lower %>% 
  mutate(CO2_equiv = ifelse(Flux_CC > 0, 45*Flux_CC, 203*Flux_CC))

#calculate flux from predicted concentration using seasonal average k600 and CO2 air 

pred.n2o.lower <- pred.n2o %>% filter(group == "A") %>% 
  mutate(Flux_CC = (lower-n2oair.avg)*k600.n2o.avg) %>% 
  mutate(CO2_equiv = Flux_CC) 

#sum CO2 equiv by site
co2eq.n2o.site.lower <- pred.n2o.lower %>% group_by(Site_ID) %>% 
  summarise(Co2eqTotal = (sum(Flux_CC)/1000)) #convert umol to mmol to compare to CH4 and CO2 directly


# calculating co2 equivalence for N2O
pred.n2o.lower <- pred.n2o.lower %>%
  mutate(CO2_equiv = ifelse(Flux_CC > 0, 270*Flux_CC, 349*Flux_CC))

# combine CO2, CH4, and N2O sum dfs into one file and sum

#keeps the contribution of each gas
TotalCO2Equiv.gas.lower <- bind_rows(list(CO2 = co2eq.CO2.site.lower, CH4 = co2eq.ch4.site.lower, N2O = co2eq.n2o.site.lower), .id = "id") %>% 
  group_by(Site_ID, id) %>% 
  summarise(CO2Equiv.gas = sum(Co2eqTotal))

#loses the contribution of each gas and just gives total per site
TotalCO2Equiv.site.lower <- bind_rows(list(CO2 = co2eq.CO2.site.lower, CH4 = co2eq.ch4.site.lower, N2O = co2eq.n2o.site.lower), .id = "id") %>% 
  group_by(Site_ID) %>% 
  summarise(CO2Equiv.site = sum(Co2eqTotal))

lower.predicted <- TotalCO2Equiv.site.lower

compare.methods <- bind_rows(list(Predicted = predicted, "July Only" = july.calc, Seasonal = seasonal.calc, "Upper CI" = upper.predicted, "Lower CI" = lower.predicted), .id = "id")

ci.comparison <- compare.methods %>% filter(id == "Upper CI"| "Lower CI" | "Predicted") 

co2.eq.methods.CI <-  ggplot(compare.methods, aes(x =factor(Site_ID, levels = c('23A', '32C', '56A', '32B', '4C', '4D', 
                                                                             '61C', '4A','62C', '62B', '56B', '14B',
                                                                             '66B', '32A', '66C', '68A', '14A', '61B',
                                                                             '62E', '66A')), y = CO2Equiv.site, fill = id))+ #reorder the sites to match the other CO2 equiv plots from lines 191 and 202
  geom_bar(position = 'dodge', stat = "identity", alpha = 0.75, color = 'black')+
  ylab(expression("Total Seasonal CO"[2]*'-eq Flux (mmol m'^2*")")) +
  SAE_theme +
  xlab("Site") +
  theme(legend.title = element_blank()) +
  theme(legend.position=c(0.2, 0.88),
        legend.background = element_blank()) 

co2.eq.methods.CI
