##---------
#Created by H.E. Froehlich
#Surplus production model of cmasy "overexploited" spp
##-----------

#B = new biomass at time t (2016) with added return from aquaculture
#r = maximum intrinsic population growth rate (assumed constant, from catch-MSY model)
#K = population carrying capacity (assumed constant, from catch-MSY model)
#C = catch at time t
#Bmsy = maximum sustainable biomass
#Bt = population biomass at next time step (t+1)
#process error, normal distribution

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lme4)
library(stringr)
library(RColorBrewer)
library(matrixStats) #for rowSds


#surplus production model
  schaefer.mod <- function(B,r,K,C,sigmaR,zlog.sd){
  
  #Process error
  w = rnorm(1, mean = 0, sd = sigmaR)
  
  #Catch error as log normal distribution
  z = rlnorm(1,meanlog = 0, sdlog = zlog.sd)
  
  #Population abundance
  Bt = B + r*B*(1-B/K)*exp(w)-C*z
  
  print(Bt)
}

#Constant inputs
yrs = seq(1,50, by=1)
runs = 500
l.yrs<-length(yrs)
future_fish<-matrix(NA,nrow=l.yrs,ncol=runs)



#Constants paramters
cmsy2016 <- read.csv("overfished_2016_cmsy_semivalidated.csv")
head(cmsy2016)

stock.num<-length(cmsy2016$stock_id)
BtoBmsy<-matrix(NA,nrow=l.yrs,ncol=stock.num)

#Catch-msy values
stock <- cmsy2016$stock_id
spp <- cmsy2016$sp_name
Bmsy = cmsy2016$k/2 #K/2
BB = cmsy2016$bbmsy_q50
r = cmsy2016$r
K = cmsy2016$k
Cmsy = cmsy2016$msy #want to use MSY to determine how much time to recover -- but can result in NA
B_q50 = cmsy2016$B_q50 
B_q75 = cmsy2016$B_q75 

#Error
sigmaR = 0.1 # overall process error for CMSY; SD=0.1 is the default
dataUncert = 0.1  # set observation error as uncertainty in catch - default is SD=0.1
zlog.sd = sqrt(log(1+(dataUncert)^2))

sp = 1

spp[sp]
Bmsy_sp<-Bmsy[sp]
  Bmsy_sp
r_sp<-r[sp]
  r_sp
K_sp<-K[sp]
  K_sp
C_sp<-0#Cmsy[sp]
  C_sp
B_0 = B_q50[sp]#Bmsy_sp*BB[sp]
  B_0

##*********
#RUN MODEL
for(k in 1:stock.num){
  sp = k
  
  spp_id<-spp[sp]
  print(spp_id)
  Bmsy_sp<-Bmsy[sp]
  r_sp<-r[sp]
  K_sp<-K[sp]
  C_sp<-Cmsy[sp]
  B_0 = B_q75[sp]
  
for(j in 1:runs){
    
    #re-intialize Bo
    B = B_0
    
    for(i in 1:l.yrs){
      
      if(i < 2){
        
      C_sp=0
      
      future_fish[i,j] <- B
      
      B<-schaefer.mod(B,r_sp,K_sp,C_sp,sigmaR,zlog.sd)
      
      B/Bmsy_sp
      
      } else{
        
        future_fish[i,j] <- B
        
        B<-schaefer.mod(B,r_sp,K_sp,C_sp,sigmaR,zlog.sd)
        
        B/Bmsy_sp
      }
    }
  }
  
  median.B<-rowMedians(future_fish)
  BtoBmsy[,k] <- median.B/Bmsy_sp
}
  
##**PLOT**
one_run<-future_fish[,100]
plot(one_run, type='l')

recover <- as.data.frame(rowMeans(BtoBmsy))
recover_sd <-as.data.frame(rowSds(BtoBmsy))
years <- seq(1,50,by=1)
recover_output <- cbind(years,recover,recover_sd)
names(recover_output)<-c("years","BBmsy","sd")
head(recover_output)

#write.csv(recover_output, "Years_to_Recovery_atMSY.csv") #added year 0 = B/Bmsy values of overfished 2016 stocks
recover_output_all<-read.csv("Years_to_Recovery_atMSY.csv")
head(recover_output_all)

mean.B <- rowMeans(future_fish)
sd.B <- rowSds(future_fish)
B_K<-mean.B/K_sp
median.B<-rowMedians(future_fish)
quant.B<-rowQuantiles(future_fish)
q.B25<-quant.B[,2]
q.B75<-quant.B[,4]

final_B <- cbind(yrs,mean.B,sd.B,median.B,q.B25,q.B75,B_K)
data.B<-as.data.frame(final_B)
data.B

ggplot(recover_output_all, aes(x = years, y = BBmsy))+
  geom_line(size=1) +
  labs(x="Years", y = "B/Bmsy" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=15),axis.text.y  = element_text(size=15),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "gray", fill=NA))+
  geom_ribbon(aes(ymin=BBmsy-sd, ymax=BBmsy+sd, color="gray", alpha=0.2))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 2))+
  geom_hline(yintercept=1, lty=2)


