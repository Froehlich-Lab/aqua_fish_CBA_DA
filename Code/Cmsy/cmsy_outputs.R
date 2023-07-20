##-------
#Created by H.E. Froehlich
#Plot catch-MSY outputs for 2016
##-----------
rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(wesanderson)
#devtools::install_github("dill/beyonce")
library(beyonce)

#Read in cmsy outputs
c.msy_fish<-read.csv("cmsy_output.csv")

head(c.msy_fish)
length(unique(c.msy_fish$sci_name)) #95 total species
length(unique(c.msy_fish$stock_id)) #162 unique stocks
max(na.omit(c.msy_fish$year))

cmsy.2016<-
  c.msy_fish %>%
  filter(year == 2016)

head(cmsy.2016)
length(cmsy.2016$stock_id) #143
#write.csv(cmsy.2016, "cmsy_2016.csv")

## 143 unique stocks, 113 w/ median B/Bmsy < 1
fishmsy<-read.csv("cmsy_2016.csv")
head(fishmsy)

##Plot F/Fmsy v B/Bmsy
ggplot(fishmsy, aes(x = bbmsy_q50, y = ffmsy_q50, color=Major_Group))+ 
  geom_pointrange(aes(ymin = ffmsy_q25, ymax = ffmsy_q75, alpha=0.1)) +
  geom_errorbarh(aes(xmax = bbmsy_q75, xmin = bbmsy_q25, height = 0, alpha=0.1))+
  labs(y="F/Fmsy", x = "B/Bmsy" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=15),axis.text.y  = element_text(size=15),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "gray", fill=NA)) +
  geom_hline(yintercept=1, lty=2)+
  geom_vline(xintercept=1, lty=2)+
  facet_wrap( ~  Dom_level)+
  scale_color_manual(values = beyonce_palette(18))+
  quartz()
#annotate("text", x=3, y=15.5, label= "a", size=10) 
#stat_smooth(col = "dark gray")

