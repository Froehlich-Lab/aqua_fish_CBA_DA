##-------
##Code created by H.E. Froehlich
## Set up data to classify as CBA and DA of 203 species

rm(list=ls())

library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(beyonce)
library(AICcmodavg)
library(lme4)

##Criteria for identifying "domesticated" versus capture-based aquaculture
    #<10 year = dependent on wild stock
      #if < 5 = level 1
      #if > 5 with increasing trend or is grouper, tuna, eels, yellowtail, and mussels
              #= assume capture based
              #Though Taiwan groupers (versus from Indonesia and Malaysia) more likely closed-cycle
      #if > 5 with increasing or above exp(8) tonnes prod trend = check status (literature check, eg. Common sole)
      #if > 5 with decreasing trend that drops below exp(8) threshold the last 10 years 
              #= assume capture based, level 2 if NA (or previously assigned CBA level)
    #>10 years & >2981 tonnes a year (aka exp(8) tonnes), IF not previously ID as level < 4 = domesticated
    #otherwise check status.
    #Abalone and geoduck spp. DA
    

#Based on and compared to "domesticated" table from SI of Teletchae & Pascal 2014 paper (finfish only)
data<-read.csv("TS_FI_AQUACULTURE.csv", header=TRUE) #read in aquaculture data from FAO
dom_levs<-read.csv("SI_2009_Dom_Levels.csv", header=TRUE) #read in known classification

#look at data
head(data)
unique(data$Sp._grp)
head(dom_levs)
min(data$Year)

#finfish only
new_dom_levs <- dom_levs[,-c(1,3,5)]
head(new_dom_levs)

#Remove non-sp specific rows & group sp
#count number of years produced (n)
#for mollusc remove species associated with ornimental use by exclude ISSCAPP "pearls"
#turn on and off filter of the different taxonimic groups
dom.yrs<-
  data %>%
  filter(Sp_name != 0 & !str_detect(Sp_name,"nei") & Envt!="FRESHWATER" & Envt!="BRACKISHWATER" &Sp._grp=="PISCES") %>%
  #filter(Sp_name != 0 & !str_detect(Sp_name,"nei") & Envt!="FRESHWATER" & Envt!="BRACKISHWATER" & Sp._grp=="MOLLUSCA" & !str_detect(Sp_ISSCAAP,"Pearls")) %>%
  #filter(Sp_name != 0 & !str_detect(Sp_name,"nei") & Envt!="FRESHWATER" & Envt!="BRACKISHWATER" & Sp._grp=="CRUSTACEA")%>%
  group_by_(.dots=c("Sp_name","Sci_name", "Sp._grp","Sp_ISSCAAP", "Year")) %>%
  summarise(SUM_tonnes = sum(Quantity)) %>%
  filter(SUM_tonnes != 0) %>%
  count(Sp_name)
head(dom.yrs) # n = number of years produced

#sum total aquaculture production
prod<-
  data %>%
  filter(Sp_name != 0 & !str_detect(Sp_name,"nei")) %>%
  group_by_(.dots=c("Sp_name", "Year"))  %>%
  #group_by_(.dots=c("Sp_name")) %>%  
  summarise(SUM_tonnes = sum(Quantity)) %>%
  filter(SUM_tonnes != 0)
head(prod) 

#join columns
dom.comb <-
  left_join(dom.yrs, new_dom_levs,by="Sp_name")
head(dom.comb)
dom.comb_prod <-
  left_join(dom.yrs, prod,by="Sp_name")
head(dom.comb_prod)
length(unique(dom.comb_prod$Sp_name))

write.csv(dom.comb_prod, "DomesticFish_output.csv")


##----LOGISTIC FIGURE  SI----Based on Finfish because we know what is Domestic vs CBA a priori
checked_prod<-read.csv("DomesticFish_output.csv") #finfish
head(checked_prod)
length(unique(checked_prod$Sp_name)) # 106 species

checked_prod[is.na(checked_prod)]<-0 #replace NAs with 0

#Designation determined in 2009, so used that year
yr2009 <- filter(checked_prod, Year==2009) 
unique(yr2009$X10yr_prod) #all 2009 finfish production has 10+ yrs, but not all "domestic"
median(yr2009$n) #= 23.5 yrs
mean(yr2009$n) # = 29.9 yrs
sd(yr2009$n) # = 19.7 yrs
length(unique(yr2009$Sp_name)) # 66/106 reported species (62%)

#Compare models
  mod0<-glm(Binary.~ n + log(SUM_tonnes+1) , data=yr2009, family=binomial(link="logit"))
  mod1<-glm(Binary.~ n , data=yr2009, family=binomial(link="logit"))
  mod2<-glm(Binary.~ log(SUM_tonnes+1) , data=yr2009, family=binomial(link="logit")) #Best Model
  mod3<-glm(Binary.~ 1 , data=yr2009, family=binomial(link="logit"))
  AICc(mod0)
  AICc(mod1)
  AICc(mod2)
  AICc(mod3)

  summary(mod2)

  x=8 #= >50% prob domestic
  odds=exp(-5.0807 +0.6388*x) #"de-logarithimize" gives you the 'odds'
  odds/(1+odds) #for probability use prob = odds/(1+odds)

#plot model
ggplot(yr2009, aes(y = Binary., x = log(SUM_tonnes+1), col=n))+
  geom_point(aes(size=0.001,alpha=0.1)) +
  labs(y="Probability of Domestication", x = "LN(Biomass Tonnes)" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=15),axis.text.y  = element_text(size=15),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "gray", fill=NA)) +
  geom_hline(yintercept=0.5079393, lty=2, col="red")+ 
  geom_vline(xintercept=8, lty=2, col="red")+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), col = "dark gray")


