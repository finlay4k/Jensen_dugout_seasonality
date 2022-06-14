

setwd("C:/Users/limno/Google Drive/Dugouts/2018")

library(readxl)
library(ggplot2)

SAE_theme  <- theme_bw() 

diel <- read_xlsx("24 Hour Dugout Sampling.xlsx", sheet = "Master")

CO2 <- ggplot(diel, aes(x=Samp_Time, y=CO2.uM, group=Site_ID, colour=Site_ID)) +
       geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey') +
       geom_line(size=1.25) + geom_point(size=2) + SAE_theme + 
       labs(y=expression(CO[2]*" ("*mu*"M)")) +
       xlab(element_blank()) +
       theme(legend.position = 'none') +
       scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF")) +
       ylim(0, 125) +
       scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24)) +
       geom_hline(yintercept=15.867922, color = "black", linetype = "dashed", size = 1)
CO2 

CH4 <- ggplot(diel, aes(x=Samp_Time, y=CH4.uM, group=Site_ID, colour=Site_ID)) +
  geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey')+ 
  geom_line(size=1.25) +geom_point(size=2) + SAE_theme + 
  labs(y=expression(CH[4]*" ("*mu*"M)")) +
  xlab(element_blank()) +
  geom_hline(yintercept=0.0722807, color = "black", linetype = "dashed", size = 1) +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF")) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24)) 

CH4

N2O <- ggplot(diel, aes(x=Samp_Time, y=N2O.nM, group=Site_ID, colour=Site_ID)) +
  geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey')+ 
  geom_line(size=1.25) +geom_point(size=2) + SAE_theme + 
  labs(y=expression(N[2]*"O (nM)")) +
  xlab("Elapsed Time (Hours)") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF")) +
  geom_hline(yintercept=12.86800463, color = "black", linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24))

N2O

ysi <- read_xlsx("Diel_YSI_Readings.xlsx")

temp <- ggplot(ysi, aes(x = Time.hr, y = Temp, group = Site_ID, color = Site_ID)) +
  geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey')+
  geom_point(size = 2) +
  geom_line(size = 1.25) +
  labs(y=expression(Water*" Temperature"*" ("*degree*"C)")) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24)) +
  SAE_theme +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF")) +
  theme(axis.title.x = element_blank()) 
temp

DO <- ggplot(ysi, aes(x = Time.hr, y = DO, group = Site_ID, color = Site_ID)) +
  geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey')+
  geom_point(size = 2) +
  geom_line(size = 1.25) +
  labs(y=expression(Dissolved*" Oxygen (mg L"^-1*")")) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24)) +
  SAE_theme +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_blank()) +
  scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF"))
DO

pH <- ggplot(ysi, aes(x = Time.hr, y = pH, group = Site_ID, color = Site_ID)) +
  geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey')+
  geom_point(size = 2) +
  geom_line(size = 1.25) +
  labs(y=expression(pH)) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24)) +
  SAE_theme +
  theme(legend.position = 'none') +
  theme(axis.title.x = element_blank()) +
  scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF"))
pH

cond <- ggplot(ysi, aes(x = Time.hr, y = log10(Cond), group = Site_ID, color = Site_ID)) +
  geom_rect(xmin = 11, xmax =19, ymin = -100, ymax = 400,alpha = 0.25, color = NA, fill = 'grey')+
  geom_point(size = 2) +
  geom_line(size = 1.25) +
  labs(y=expression(Conductivity*" (log"[10]*"("*mu*"S cm"^-1*"))")) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), ) +
  SAE_theme +
  xlab("Elapsed Time (Hours)") +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_colour_manual(values = c("#481567FF", "#2D708EFF", "#55C667FF", "#FDE725FF"), 
                  labels=c(expression(paste(Site*" 1")),
                           expression(paste(Site*" 2")),
                           expression(paste(Site*" 3")),
                           expression(paste(Site*" 4")))) 
cond

library(patchwork)

layout <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
GGHH
GGHH
"

library(ggpubr)

legend <- get_legend(cond)

all <- CO2 + temp + CH4 + pH + N2O + DO + legend + cond  + plot_layout(design = layout)
all

ggsave("Diel_YSI&GHG_Plots.png", all)

library(tidyverse)

wq <- diel %>% 
  mutate(TDP.ug.P.L = TP.mg.P.L*1000,
         SRP.ug.P.L = SRP.mg.P.L*1000,
         NH3.ug.N.L = NH3.mg.N.L*1000) %>% 
  select(c("Site_ID", "Chla", "TN.ug.N.L", "Nitrate_Nitrite.ug.N.L", "NH3.ug.N.L", "TDP.ug.P.L",
           "SRP.ug.P.L",  "DOC.mg.L", "DIC.mg.L")) %>% 
  
  drop_na() %>% 
  group_by(Site_ID)

#rename to more generic site values - may change back 
wq$Site_ID <- gsub("14A", "Site 1", wq$Site_ID)
wq$Site_ID <- gsub("23", "Site 2", wq$Site_ID)
wq$Site_ID <- gsub("4C", "Site 3", wq$Site_ID)
wq$Site_ID <- gsub("56A", "Site 4", wq$Site_ID)

library(vtable)
#summary table with mean and standard deviation exported to CSV to create attractive table in excel/powerpoint
sumtable(data = wq, group = "Site_ID", group.test = FALSE, out = 'csv', file = 'Diel_WQ_ Summary_Stats.csv')
 
