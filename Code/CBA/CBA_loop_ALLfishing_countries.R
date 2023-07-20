##-------
#Created by H.E. Froehlich
#Model CBA species catch replacement
##-----------

rm(list=ls())

library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(beyonce)
library(stringr)
library(matrixStats) #for rowSds
library(effects) #plot(allEffects(model))
library(arm) #se.coef(model)
library(cowplot)
library(patchwork)

fish_all<-read.csv("ALL_CBA_WildCatch_2016.csv") #Zero catch removed
head(fish_all)

fish<-filter(fish_all, Year == 2016)
head(fish)

#Create data banks
n=1
run = 10000
r.length = length(fish$Common) 
net_fish<-matrix(NA,nrow=r.length,ncol=run)
AQcap_fish<-matrix(NA,nrow=r.length,ncol=run)
ONLYAQactlost_fish<-matrix(NA,nrow=r.length,ncol=run)
SUBlost_fish<-matrix(NA,nrow=r.length,ncol=run)
  
set.seed(22)

#Calculate the biomass left in the ocean if CBA replaced catch for the same species
  #Early_M = ealier stage natural discrete mortality
  #Late_M = later stage natural discrete mortality

for(i in 1:run){
  
  S_cap <- round(runif(n, min=0.5, max=0.9),2) #determined post-capture survival uniform range from literature
  S_cap
  
  fish$AQpreS<-round(fish$AQSUM_tonnes/S_cap,1) #amount needed to be captured to result in FAO aquaculture production reported
  fish$WLost<-round((fish$AQpreS-(fish$AQpreS*fish$Early_M)),1) #amount of wild biomass ACTUALLY lost from CBA (accounting for M)
  fish$WLost_aM<-round((fish$WLost-(fish$WLost*fish$Late_M)),1)
  fish$SUBAQpreS<-round(fish$TOTAL_tonnes/S_cap,1) #TOTAL seafood (wild + farm)
  fish$SUBWLost<-round((fish$SUBAQpreS-(fish$SUBAQpreS*fish$Early_M)),1) #Wild biomass "lost" to capture for CBA (compare to 'Current_Lost')
  fish$SUBWLost_aM<-round((fish$SUBWLost-(fish$SUBWLost*fish$Late_M)),1) 
  fish$Current_Lost<-fish$WLost_aM+(fish$CAPSUM_tonnes-(fish$CAPSUM_tonnes*fish$Late_M))
  
  ONLYAQactlost_fish[,i]<-fish$WLost_aM #current biomass ACTUALLY lost to CBA ONLY
  
  AQcap_fish[,i] <-fish$Current_Lost #current biomass lost to capture and CBA
  SUBlost_fish[,i] <-fish$SUBWLost_aM #amount of wild biomass ACTUALLY lost from CBA if it replaced all wild capture (accounting for M) -- USE THIS AS 'Ct' in SURPLUS MODEL
  net_fish[,i]<-fish$Current_Lost-fish$SUBWLost_aM #net fish spared in the wild
  
}

#Current lost biomass from wild (wild capture + CBA)
AQcap_fish[,1]
mean.aq.now = rowMeans(AQcap_fish)
sd.aq.now = rowSds(AQcap_fish)
median.aq.now =rowMedians(AQcap_fish)
quant.aq.now = rowQuantiles(AQcap_fish)
quant.aq25.now = quant.aq.now[,2]
quant.aq75.now = quant.aq.now[,4]

#New (replace all wild with CBA) 'actual' catch for surplus 'Ct' input
SUBlost_fish[,1]
mean.aq = rowMeans(SUBlost_fish)
sd.aq = rowSds(SUBlost_fish)
median.aq =rowMedians(SUBlost_fish)
quant.aq = rowQuantiles(SUBlost_fish)
quant.aq25 = quant.aq[,2]
quant.aq75 = quant.aq[,4]

#Total 'spared/returned' in the wild if CBA went from capture & CBA to only CBA
net_fish[,1]
mean = rowMeans(net_fish)
sd = rowSds(net_fish)
median =rowMedians(net_fish)
quant=rowQuantiles(net_fish)
quant.25 = quant[,2]
quant.75 = quant[,4]


#Bind outputs
final_fish <- cbind(as.vector(fish$Common),as.vector(fish$Sci_Name),as.vector(fish$Sp.grp),
                    as.vector(fish$ISSCAAP),as.vector(fish$Country), as.vector(fish$Fishing_name),
                    median.aq,quant.aq25,quant.aq75,median.aq.now,quant.aq25.now,quant.aq75.now,
                    median,quant.25,quant.75, as.vector(fish$Early_M),as.vector(fish$Late_M))
final_df<-as.data.frame(final_fish)
head(final_df)

#Rename columns
names(final_df)<- c("Sp_name","Sci_name","Group","ISSCAAP","Country","Fishing_area","AQ_median","AQ_q25","AQ_q75",
                    "AQ_medianNOW","AQ_q25NOW","AQ_q75NOW","Net_Median","Net_q25","Net_q75","Early_M", "Late_M")
head(final_df)


##-------------
#SAVE OUTPUTS
  #write.csv(final_df,file = "MEDIAN_Spared_outputs.csv") #postive column (i.e., # net median gains +)
output.fish<-read.csv("MEDIAN_Spared_outputs.csv")

  head(output.fish)
  all.rm<-sum(output.fish$AQ_medianNOW) #ALL REMOVED FROM WILD 
  sp<-sum(output.fish$Net_Median) #ALL SPARED
  ql.sp<-sum(output.fish$Net_q25) #= Q25
  qu.sp<-sum(output.fish$Net_q75) #= Q75
  
  sp/all.rm #percent remaining in the ocean
  ql.sp/all.rm #Q25
  qu.sp/all.rm #Q75

  
##---PLOT------ 
quartz()
all_grp<-ggplot(output.fish, aes(y = Net_Median, x = ISSCAAP, color=Group, 
                   ymin = Net_q25, ymax = Net_q75))+
  geom_pointrange(aes(alpha=0.1))+
  #geom_boxplot(alpha=0.5)+
  #geom_jitter(position=position_jitter(0.2))+
  coord_flip() +
  labs(x="", y = " " )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=10),
        axis.title.x = element_text(size=11), axis.title.y = element_text(size=10),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA),)+
  geom_hline(yintercept=0, lty=2, color="red")+
  scale_color_manual(values = beyonce_palette(18))

zoom_grp<-ggplot(output.fish, aes(y = Net_Median, x = ISSCAAP, color=Group, 
                                  ymin = Net_q25, ymax = Net_q75))+
  geom_pointrange(aes(alpha=0.1))+
  coord_flip() +
  labs(x="", y = "Net Biomass CBA replacement (tonnes)" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=10),
        axis.title.x = element_text(size=11), axis.title.y = element_text(size=10),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA),legend.position="none")+
  geom_hline(yintercept=0, lty=2, color="red")+
  scale_y_continuous(limits = c(-500, 500))+
  scale_color_manual(values = beyonce_palette(18)) 


(all_grp/zoom_grp) 