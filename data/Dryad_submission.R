#==========================#
#  IDE MANUSCRIPT ANALYSIS #
#         & FIGURES        #
#          UPDATED:        #
#         JUL 2023         #
#        1YR DROUGHT       #
#==========================#
#load necessary packages
my_packages<-c("tidyverse","devtools", "tidyr","data.table","nlme","ggplot2",
               "ggpubr","ggthemes","raster","rgdal", "extrafontdb","extrafont",
               "ggmap","grid","gridExtra","mapdata","maps", "GGally",
               "tidyr","tidyverse","sf","lme4","lmerTest",
               "MuMIn","AICcmodavg","ppcor","emmeans",
               "mctest", "segmented","nlstools","Hmisc","ggpubr","ggstance","sjPlot",
               "broom","purrr","nlme","loo","devtools","stats","gam")

# Check for and install required packages
for (package in my_packages) {
  if (!require(package, character.only=T, quietly=T)) {
    try(install.packages(package))
    library(package, character.only=T)
  }
}


#######################################
site_info <- read.csv("~Dryad_data_site_info.csv")
dt3.all <- read.csv("~dt3.all.csv")
drt.ave <- read.csv("~drt.ave.csv")

#Linear model
lm.drtall<-lm(mean_DS3~drtpct_map,data=drt.ave)
summary(lm.drtall) #p=0.01

#Non-linear models
#Asymptotic 
asym.drtall <- nls(mean_DS3 ~ SSasymp(drtpct_map, Asym, resp0, lrc), control = nls.control(maxiter = 1000, warnOnly=TRUE), data = drt.ave)
summary(asym.drtall)

#Generalized additive model (spline=3)
gam.drtall <- gam(mean_DS3 ~ s(drtpct_map, 3), data = drt.ave) #number picks how much wiggle
summary(gam.drtall)

#AIC testing
aic_drt<-as.data.frame(AIC(lm.drtall,asym.drtall,gam.drtall))
aic_drt$deltAIC<-(aic_drt$AIC-138.5622) #First one is zero, but we don't have all decimal places
aic_drt

#Since linear model is best, we proceeded with that for further analyses

#Part of Table S6

#Table S6
coeffs.all <- as.data.frame(coef(summary(lm.drtall))) # get estimates, etc...

coeffs.all$model<-"All_data"


#FIGURE 1------------------------------------------------------------------
#Figure 1B: Climatic space figure (n=100)
climdat2 <- read.csv("~climdat2.csv")

Fig1B<-ggplot(climdat2, aes(x=precip, y=temp))+
  #geom_text(data=climdat2,aes(label=site_code))+
  geom_point(aes(color=Eco.Extr,shape=Eco.Extr),size=5,position=position_jitter(w=0,h=0.9))+              
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme (n=35)","Grassland_NotExtreme"="Grassland, nominal (n=39)","Shrubland_Extr"="Shrubland, extreme (n=9)","Shrubland_NotExtreme"="Shrubland, nominal (n=17)"),
                     values=c("mediumseagreen","rosybrown4","mediumseagreen","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme (n=35)","Grassland_NotExtreme"="Grassland, nominal (n=39)","Shrubland_Extr"="Shrubland, extreme (n=9)","Shrubland_NotExtreme"="Shrubland, nominal (n=17)"),values=c(19,17,21,24))+ 
  ylab("Mean Annual\nTemperature (C)\n")+
  xlab("Mean Annual\n Precipitation (mm)")+
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"), 
        panel.background = element_blank(),
        text=element_text(size=14,  family="Calibri",color="black"),
        legend.position="none")
Fig1B


#FIGURE 2------------------------------------------------------------------

#Average by habitat type
data.hab <- read.csv("~data.hab.csv")

#Linear mixed effects model testing DS3 by habitat type
#and extremity
lm.hab<-lm(mean_DS3~habitat.type,data=data.hab)
summary(lm.hab) #p=0.25
drt.emmhab <- emmeans(lm.hab, "habitat.type")


dt3.120 <- read.csv("~dt3.120.csv")
#Percent increase at gmgranite.us (DS3=0.38)
(exp(0.3831087)*100)-100 #46.68375

#Percent reduction at sgsdrt.us (DS3=-2.07)
(exp(-2.123565)*100)-100 #-88.03955


#Ecosystem-level log resistance
dt4.120<-dt3.120%>%
  dplyr::group_by(Eco.Extr)%>%
  dplyr::summarize(mean_hab_DS3 = mean(mean_DS3,na.rm=T), n = n(),sd = sd(mean_DS3,na.rm=T), se = sd/sqrt(n),
                   LowerCI = mean_hab_DS3 - qt(1 - (0.05 / 2), n - 1) * se,
                   UpperCI = mean_hab_DS3 + qt(1 - (0.05 / 2), n - 1) * se)%>%
  dplyr::as_tibble()

grmean120<-dt3.120%>%
  dplyr::group_by(Extreme)%>%
  dplyr::summarize(mean_hab_DS3 = mean(mean_DS3,na.rm=T), n = n(),sd = sd(mean_DS3,na.rm=T), se = sd/sqrt(n),
                   LowerCI = mean_hab_DS3 - qt(1 - (0.05 / 2), n - 1) * se,
                   UpperCI = mean_hab_DS3 + qt(1 - (0.05 / 2), n - 1) * se)%>%
  dplyr::rename(Eco.Extr=Extreme)%>%
  dplyr::as_tibble()

dt4.120<-dt4.120[order(dt4.120$mean_hab_DS3),]
RRhabitat120<-rbind(grmean120,dt4.120)

RRhabitat120$label<-as.character(RRhabitat120$Eco.Extr)

RRhabitat120$label[RRhabitat120$label %in% c("Extr","NotExtreme")]<-"All sites (n=100)"
RRhabitat120$label[RRhabitat120$label %in% c("Grassland_Extr","Grassland_NotExtreme")]<-"Grassland (n=74)"
RRhabitat120$label[RRhabitat120$label %in% c("Shrubland_Extr","Shrubland_NotExtreme")]<-"Shrubland (n=26)"

RRhabitat120$Eco.Extr <- factor(RRhabitat120$Eco.Extr, levels = c("Shrubland_NotExtreme","Grassland_NotExtreme","Shrubland_Extr","Grassland_Extr","NotExtreme","Extr"))
RRhabitat120$label <- factor(RRhabitat120$label, levels = c("Shrubland (n=26)","Grassland (n=74)","All sites (n=100)"))

#Percent reductions
RRhabitat120$Pct.red<-format(round(((exp(RRhabitat120$mean_hab_DS3)*100)-100),1),nsmall=1)
RRhabitat120$Pct.red <- paste0(RRhabitat120$Pct.red,'%')

#12.6-19% decline in ANPP from meta-analyses
#with large sample sizes (23-55 experiments)
#Song et al. 2019 (~19%, n = 23) 
#Wang et al. 2021 (~13%, n = 55)
(exp(-0.1347)*100)-100 #-12.60
(exp(-0.2108)*100)-100 #-19.01

str(RRhabitat120)

RRhabitat120$label<-as.factor(RRhabitat120$label)

#Figure 2A: Log Response Ratio (Drought Resistance) by Habitat
Fig2A<-ggplot(RRhabitat120,aes(x=mean_hab_DS3,y=label))+
  geom_rect(aes(xmin = -0.2108, xmax =-0.1347, ymin = -Inf, ymax = Inf, fill = T),
            fill = 'gray80', alpha = 0.3)+
  geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI,color=Eco.Extr,linetype=Eco.Extr),width=0.05,position=ggstance::position_dodgev(height=0.9)) + 
  geom_point(aes(color=Eco.Extr,shape=Eco.Extr),size=5,position=ggstance::position_dodgev(height=0.9))+
  scale_color_manual(name="Ecosystem Type",labels = c("Shrubland_NotExtreme","Grassland_NotExtreme","Shrubland_Extr","Grassland_Extr","NotExtreme","Extr"),values=c("rosybrown4","mediumseagreen","rosybrown4","mediumseagreen","black","black"))+
  scale_shape_manual(name="Ecosystem Type",labels =c("Shrubland_NotExtreme","Grassland_NotExtreme","Shrubland_Extr","Grassland_Extr","NotExtreme","Extr"),values=c(24,21,17,19,5,18))+
  scale_linetype_manual(name="Ecosystem Type",labels =c("Shrubland_NotExtreme","Grassland_NotExtreme","Shrubland_Extr","Grassland_Extr","NotExtreme","Extr"),values=c(2,2,1,1,2,1))+
  labs(x = "Drought Response", y="Ecosystem Type")+
  geom_vline(xintercept=0,lty=2,size=1.5)+
  theme_tufte(ticks=T,base_size = 15)+ 
  theme(text=element_text(size=20,  family="Calibri",color="black"),axis.line.x = element_line(colour ="black", size = 0.5),
        axis.line.y = element_line(colour ="black", size = 0.5),legend.position="none",
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, color = "black"))
Fig2A

#Figure 2B: Drought response by site
RRdata<-dt3.all

RRdata.sort <- RRdata[order(-(RRdata$mean_DS3)),]
RRdata.sort$site_code <- factor(RRdata.sort$site_code, levels=RRdata.sort$site_code)
RRdata.sort

length(unique(RRdata.sort$site_code)) #100
sum(RRdata.sort$mean_DS3<0) #79 sites with negative drought response
sum(RRdata.sort$mean_DS3>0) #21 sites with no or positive drought response

Fig2B<-ggplot(RRdata.sort,aes(x = mean_DS3,y=site_code))+
  geom_vline(xintercept=0,size=2.5,lty=2)+
  geom_segment(aes(x=LowerCI,y=site_code,xend=UpperCI,yend=site_code,color=Eco.Extr),size=1)+
  geom_point(aes(color=Eco.Extr,shape=Eco.Extr),size=6)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme (n=34)","Shrubland_Extr"="Shrubland, extreme (n=8)","Grassland_NotExtreme"="Grassland, Not extreme (n=40)","Shrubland,Not extreme (n=18)"),
                     values=c("mediumseagreen","rosybrown4","mediumseagreen","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme (n=34)","Shrubland_Extr"="Shrubland, extreme (n=8)","Grassland_NotExtreme"="Grassland, Not extreme (n=40)","Shrubland,Not extreme (n=18)"),values=c(19,17,21,24))+ 
  labs(x = "Drought Response", y="Site (n=100)")+
  geom_hline(yintercept=0)+
  theme_tufte(ticks=T,base_size = 15)+ 
  theme(legend.position="none",axis.line.x = element_line(colour ="black", size = 0.5),
        axis.line.y = element_line(colour ="black", size = 0.5),
        axis.text.x = element_text(color="black",size=30,family="Calibri"),
        axis.text.y = element_text(color="black",size=15,family="Calibri"),
        axis.title.x = element_text(color="black",size=30,family="Calibri"),
        axis.title.y = element_text(color="black",size=30,family="Calibri"),
        legend.text = element_text(color="black",size=30,family="Calibri"),
        legend.title = element_text(color="black",size=30,family="Calibri"))
Fig2B.1<-Fig2B+xlim(-7.0,7.0)
Fig2B.1

#Figure 2A and B merged in PowerPoint because ggarrange too awkward

#FIGURE 3------------------------------------------------------------------
#Drought response versus abiotic and biotic variables

#Fig3a: Proportion Graminoids

gram.ave <- read.csv("~Dryad_submission_gram.ave.csv")


datapptgram1 <- read.csv("~datapptgram1.csv")
#Test for differences in proportion graminoids among habitats
datagram.ave<-datapptgram1%>%
  dplyr::group_by(site_code,habitat.type)%>%
  dplyr::summarise(prop.ave2=mean(prop.ave))

anova(lm(prop.ave2~habitat.type,data=datagram.ave))
summary(lm(prop.ave2~habitat.type,data=datagram.ave))

#lm:Proportion graminoids
str(datapptgram1)

#Even though proportion graminoids is significantly different among habitat types,
#Drought response does not differ significantly between them, so moving forward 
#Without interaction models

#Average Proportion graminoids
datapptgram2<-merge(dt3.all,gram.ave,by=c('site_code')) #Merge prop graminoids (site characteristic)
length(datapptgram2$site_code) #77
count(datapptgram2, Eco.Extr)

datapptgram2$Eco.Extr <- factor(datapptgram2$Eco.Extr, levels = c("Grassland_Extr","Grassland_NotExtreme","Shrubland_Extr","Shrubland_NotExtreme"))


#Linear model (no need to test non-linear model because data doesn't look non-linear)

#All sites (not significant)
lm.gram<-lm(mean_DS3~prop.ave,data=datapptgram2)
summary(lm.gram) #p=0.35

#Average drought response of sites with ANPP to functional group
dsave.gram0<-datapptgram2 %>%
  dplyr::group_by(Eco.Extr)%>%
  dplyr::summarise(mean.ds=mean(mean_DS3))%>%
  dplyr::as_tibble()

grmean.gram<-datapptgram2 %>%
  dplyr::group_by(Extreme)%>%
  dplyr::summarise(mean.ds=mean(mean_DS3))%>%
  dplyr::rename(Eco.Extr=Extreme)%>%
  dplyr::as_tibble()

dsave.gram<-rbind(dsave.gram0,grmean.gram)
dsave.gram$Data<-"Prop.Graminoids"

#Graminoids figure
Figgram <- ggplot(datapptgram2,aes(x=prop.ave,y=mean_DS3))+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="Proportion of Graminoids")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
Figgram

#Fig3b: Site-reported MAP
ds.map <- read.csv("~ds.map.csv")
datappt_MAP2 <- read.csv("~datappt_MAP2.csv")
#Linear vs. non-linear models
#Linear model
lm.mapall<-lm(mean_DS3~precip,data=datappt_MAP2)
summary(lm.mapall) #p=0.08

#Asymptotic (not converging)
asym.map <- nls(mean_DS3 ~ SSasymp(precip, Asym, resp0, lrc), control = nls.control(maxiter = 1000, warnOnly=TRUE), data = datappt_MAP2)
summary(asym.map) #p=0.83

#Genaralized additive model
gam.map <- gam(mean_DS3 ~ s(precip, 3), data = datappt_MAP2) #number picks how much wiggle
summary(gam.map)

#AIC testing
aic_map<-as.data.frame(AIC(lm.mapall,asym.map,gam.map))
aic_map$deltAIC<-(aic_map$AIC- 142.4297) #First one is zero, but we don't have all decimal places
aic_map

#Figure
Figmap <- ggplot(datappt_MAP2,aes(x=precip,y=mean_DS3))+
  geom_smooth(method = "lm", se=TRUE,col="black",fill = "gray60",lty=1)+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="MAP (mm)")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
Figmap

#Fig3c:%SAND (From ISRIC soil grid code)
datapptsand2 <- read.csv("~Dryad_submission_datapptsand2.csv")

#Not comparing linear and Non-linear Models
lm.allsand<-lm(mean_DS3~sand_mean,data=datapptsand2)
summary(lm.allsand) #p=0.39

#Average drought response of sites with %sand
dsave.sand0<-datapptsand2 %>%
  dplyr::group_by(Eco.Extr)%>%
  dplyr::summarise(mean.ds=mean(mean_DS3))%>%
  dplyr::as_tibble()

grmean.sand<-datapptsand2 %>%
  dplyr::group_by(Extreme)%>%
  dplyr::summarise(mean.ds=mean(mean_DS3))%>%
  dplyr::rename(Eco.Extr=Extreme)%>%
  dplyr::as_tibble()

dsave.sand<-rbind(dsave.sand0,grmean.sand)
dsave.sand$Data<-"Sand"


Figsand <- ggplot(datapptsand2,aes(x=sand_mean,y=mean_DS3))+
  #geom_smooth(method = "lm", se=TRUE,col="black",fill = "gray60",lty=3)+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=96)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=96)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="Sand in Soil (%)")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
Figsand

#Fig3d:CV from MSWEP (Multi-Source Weighted-Ensemble Precipitation)
#Using INTERannual cv
#Coefficient of variation vs. Drought response
datappt_cv2 <- read.csv("~Dryad_submission_datappt_cv2.csv")
length(datappt_cv2$site_code) #100
datappt_cv2$Eco.Extr <- factor(datappt_cv2$Eco.Extr, levels = c("Grassland_Extr","Grassland_NotExtreme","Shrubland_Extr","Shrubland_NotExtreme"))

#Linear model (figure doesn't look like we need to test a non-linear model)
lm.cv<-lm(mean_DS3~cv_ppt_inter,data=datappt_cv2)
summary(lm.cv) #p=0.13

FigCV <- ggplot(datappt_cv2,aes(x=cv_ppt_inter,y=mean_DS3))+
  #geom_smooth(method = "lm", se=TRUE,col="black",fill = "gray60",lty=3)+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="CV of MAP (%)")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
FigCV

#Fig3e: Richness
#Looking at the effects of richness on drought response
datappt_rich2 <- read.csv("~Dryad_submission_datappt_rich2.csv")


length(datappt_rich2$site_code) #68
datappt_rich2$Eco.Extr <- factor(datappt_rich2$Eco.Extr, levels = c("Grassland_Extr","Grassland_NotExtreme","Shrubland_Extr","Shrubland_NotExtreme"))

#Linear vs. non-linear models
#Linear model
lm.rich<-lm(mean_DS3~average.richness,data=datappt_rich2)
summary(lm.rich) #p=0.04

#Asymptotic
asym.rich <- nls(mean_DS3 ~ SSasymp(average.richness, Asym, resp0, lrc), control = nls.control(maxiter = 1000, warnOnly=TRUE), data = datappt_rich2)
summary(asym.rich) #p=0.17

#Genaralized additive model
gam.rich <- gam(mean_DS3 ~ s(average.richness, 3), data = datappt_rich2) #number picks how much wiggle
summary(gam.rich)

#AIC testing
aic_rich<-as.data.frame(AIC(lm.rich,asym.rich,gam.rich))
aic_rich$deltAIC<-(aic_rich$AIC-80.10577) #First one is zero, but we don't have all decimal places
aic_rich


#Average drought response of sites with richness
dsave.rich0<-datappt_rich2 %>%
  dplyr::group_by(Eco.Extr)%>%
  dplyr::summarise(mean.ds=mean(mean_DS3))%>%
  dplyr::as_tibble()

grmean.rich<-datappt_rich2 %>%
  dplyr::group_by(Extreme)%>%
  dplyr::summarise(mean.ds=mean(mean_DS3))%>%
  dplyr::rename(Eco.Extr=Extreme)%>%
  dplyr::as_tibble()

dsave.rich<-rbind(dsave.rich0,grmean.rich)
dsave.rich$Data<-"Richness"

#Richness figure
FigRich <- ggplot(datappt_rich2,aes(x=average.richness,y=mean_DS3))+
  geom_smooth(method = "lm", se=TRUE,col="black",fill = "gray60",lty=1)+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="Average Richness")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
FigRich

datappt_pryr <- read.csv("~datappt_pryr.csv")
#Linear vs. non-linear models
#Linear model
lm.pryr<-lm(mean_DS3~drtpct_map730,data=datappt_pryr)
summary(lm.pryr) #p=0.37


#Genaralized additive model
gam.pryr <- gam(mean_DS3 ~ s(drtpct_map730, 3), data = datappt_pryr) #number picks how much wiggle
summary(gam.pryr)

#AIC testing
aic_pryr<-as.data.frame(AIC(lm.pryr,gam.pryr))
aic_pryr$deltAIC<-(aic_pryr$AIC-144.6594) #First one is zero, but we don't have all decimal places
aic_pryr

FigPYr<- ggplot(datappt_pryr,aes(x=drtpct_map730,y=mean_DS3))+
  #geom_smooth(method = "lm", se=TRUE,col="black",fill = "gray60",lty=1)+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="Previous Year's Precip (mm)*")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
FigPYr

datappt_ai3 <- read.csv("~datappt_ai3.csv")

#Linear vs. non-linear models
#Linear model
lm.ai<-lm(mean_DS3~logAI,data=datappt_ai3)
summary(lm.ai) #p=0.02


#Genaralized additive model
gam.ai <- gam(mean_DS3 ~ s(logAI, 3), data = datappt_ai3) #number picks how much wiggle
summary(gam.ai)

#AIC testing
aic_ai<-as.data.frame(AIC(lm.ai,gam.ai))
aic_ai$deltAIC<-(aic_ai$AIC-139.5943) #First one is zero, but we don't have all decimal places
aic_ai


#Figure: AI
FigAI<- ggplot(datappt_ai3,aes(x=logAI,y=mean_DS3))+
  geom_smooth(method = "lm", se=TRUE,col="black",fill = "gray60",lty=1,fullrange=TRUE)+
  geom_hline(yintercept=0,lty=2,size=1)+
  geom_point(aes(shape=Eco.Extr,color=Eco.Extr),size=3)+
  scale_color_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),
                     values=c("mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="IDE Sites (n=100)",labels = c("Grassland_Extr"="Grassland, extreme","Grassland_NotExtreme"="Grassland, nominal","Shrubland_Extr"="Shrubland, extreme","Shrubland_NotExtreme"="Shrubland, nominal"),values=c(19,21,17,24))+ 
  labs(y = "Drought Response", x="ln(Aridity Index)")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=14,  family="Calibri",color="black"),axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.y.right = element_line(color="steelblue", size = 1),
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.title.y.right = element_text(colour = "steelblue"),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(colour = "steelblue"),legend.position = "top",legend.title = element_blank())
FigAI


#Response to reviewers: Mean drought response of sites with functional group ANPP, sand, and richness

# Excluding richness from full model due to too few sites 
#drtsev

#Table Sabio.bio (likely Table S7)
#DrtSev
coeffs.drt <- as.data.frame(coef(summary(lm.drtall)))
coeffs.drt$model<-"DrtSev"
lm.drtall$df.residual #98
summary(lm.drtall)$adj.r.squared #0.05

#MAP
coeffs.map <- as.data.frame(coef(summary(lm.mapall)))
coeffs.map$model<-"MAP"
lm.mapall$df.residual #98
summary(lm.mapall)$adj.r.squared #0.02

#Previous-year's precip
coeffs.pyr<- as.data.frame(coef(summary(lm.pryr)))
coeffs.pyr$model<-"Prev Yr Precip"
lm.pryr$df.residual #98
summary(lm.pryr)$adj.r.squared #-0.002

#CV in MAP
coeffs.cv <- as.data.frame(coef(summary(lm.cv)))
coeffs.cv$model<-"CV"
lm.cv$df.residual #98
summary(lm.cv)$adj.r.squared #0.01

#Aridity
coeffs.ai <- as.data.frame(coef(summary(lm.ai)))
coeffs.ai$model<-"logAI"
lm.ai$df.residual #98
summary(lm.ai)$adj.r.squared #0.05

#%Sand
coeffs.sand <- as.data.frame(coef(summary(lm.allsand)))
coeffs.sand$model<-"Sand"
lm.allsand$df.residual #94
summary(lm.allsand)$adj.r.squared #-0.003

#Prop Graminoids
coeffs.gram<- as.data.frame(coef(summary(lm.gram)))
coeffs.gram$model<-"Graminoid"
lm.gram$df.residual #75
summary(lm.gram)$adj.r.squared #-0.002

#Richness
coeffs.rich <- as.data.frame(coef(summary(lm.rich)))
coeffs.rich$model<-"Richness"
lm.rich$df.residual #66
summary(lm.rich)$adj.r.squared #0.05

#Combine into one object
coeffs.abio.bio<-dplyr::bind_rows(coeffs.drt,coeffs.map,coeffs.pyr,coeffs.cv,coeffs.ai,coeffs.sand,coeffs.gram,coeffs.rich)

#Create table of AIC comparisons for linear and non-linear models
AICall<-rbind(aic_drt,aic_map,aic_pryr,aic_ai,aic_rich)
AICall<-setDT(AICall, keep.rownames = TRUE)[]

#Merge MAP, CV of MAP, Previous Year's Precip, Aridity, and percent Sand to run multiple regression
abioa<-merge(datappt_MAP2[,c('site_code','precip')],datappt_cv2,by=c('site_code'))
abio2<-merge(abioa, datapptsand2[,c('site_code','sand_mean')],by=c('site_code'))

lm.abio<-lm(mean_DS3~precip*sand_mean,data=abio2)
summary(lm.abio) #not significant
lm.abio$df.residual #92
summary(lm.abio)$adj.r.squared #0.01

#Table S8 MapSand
coeffs.mapsand <- as.data.frame(coef(summary(lm.abio)))
coeffs.mapsand$model<-"All sites"

#Table S9: lm for full model
data.Drtsite <- read.csv("~data.Drtsite.csv")

data.Drtmean<-data.Drtsite%>%
  dplyr::group_by(Extreme)%>%
  dplyr::summarize(mean_drt = mean(mean_drt2,na.rm=T), n = n(),sd = sd(mean_drt2,na.rm=T), se = sd/sqrt(n),
                   drtLowerCI = mean_drt - qt(1 - (0.05 / 2), n - 1) * se,
                   drtUpperCI = mean_drt + qt(1 - (0.05 / 2), n - 1) * se)%>%
  dplyr::as_tibble()

data.Drtmean<-data.Drtmean%>% dplyr::select(Extreme,mean_drt,drtLowerCI,drtUpperCI)

#Overall average drought severity
Drt.sevmean<-data.Drtsite%>%
  dplyr::summarize(mean_drt = mean(mean_drt2,na.rm=T), n = n(),sd = sd(mean_drt2,na.rm=T), se = sd/sqrt(n),
                   drtLowerCI = mean_drt - qt(1 - (0.05 / 2), n - 1) * se,
                   drtUpperCI = mean_drt + qt(1 - (0.05 / 2), n - 1) * se)%>%
  dplyr::as_tibble()

#Merge meanDS3 and mean drt_sev together
data.mean1 <- read.csv("~data.mean1.csv")

#Drought Resistance versus drtpct_map: Grasslands and Shrublands combined

#With means for extremities

#ANPP Percent reduction at sites with ambient precip at or above average
(exp(-0.2569669)*100)-100 #-22.66

#ANPP Percent reduction at sites with ambient precip below average
(exp(-0.4224695)*100)-100 #-34.46

#Reverse order so above and below zero points appear on top
data.mean1$Extreme <- factor(data.mean1$Extreme , levels = c("Below zero","Above zero","Grassland_Extr","Grassland_NotExtreme","Shrubland_Extr","Shrubland_NotExtreme"))

Fig4<-ggplot(data.mean1, aes(x = mean_drt, y = mean_DS3))+
  geom_hline(yintercept=0,lty=2,color="black",size=1,show.legend = FALSE) +
  geom_vline(xintercept=0,lty=1,color="gray30",size=1,show.legend = FALSE)+
  geom_hline(yintercept=-0.69,lty=2,color="red2",size=1.5,show.legend = FALSE)+
  geom_hline(yintercept=-1.39,lty=2,color="red2",size=1.5,show.legend = FALSE)+
  geom_smooth(aes(group=1),method = "lm", formula=y~x, fullrange=TRUE,se=TRUE,col="black",fill = "gray60",show.legend = FALSE)+
  geom_point(data=data.mean1,aes(x=mean_drt,y=mean_DS3,shape=Extreme,color=Extreme,size=Extreme),stroke = 1) + 
  scale_size_manual(name="Ecosystem Type",labels = c('Below zero'='All sites, extreme','Above zero'='All sites, nominal','Grassland_Extr'='Grassland, extreme','Grassland_NotExtreme'='Grassland, nominal','Shrubland_Extr'='Shrubland, extreme','Shrubland_NotExtreme'='Shrubland, nominal'),values=c(8,8,3,3,3,3,3,3))+
  scale_color_manual(name="Ecosystem Type",labels = c('Below zero'='All sites, extreme','Above zero'='All sites, nominal','Grassland_Extr'='Grassland, extreme','Grassland_NotExtreme'='Grassland, nominal','Shrubland_Extr'='Shrubland, extreme','Shrubland_NotExtreme'='Shrubland, nominal'),values=c("coral","coral","mediumseagreen","mediumseagreen","rosybrown4","rosybrown4"))+
  scale_shape_manual(name="Ecosystem Type",labels = c('Below zero'='All sites, extreme','Above zero'='All sites, nominal','Grassland_Extr'='Grassland, extreme','Grassland_NotExtreme'='Grassland, nominal','Shrubland_Extr'='Shrubland, extreme','Shrubland_NotExtreme'='Shrubland, nominal'),values=c(15,0,19,21,17,24))+ 
  labs(y = "Drought Response", x="Drought Severity")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=16,  family="Calibri",color="black"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        legend.position="none") 
Fig4


#How many sites greater than 50% loss and greater than 75% loss
#First remove meanDS3, since not a site
data.mean2<-data.mean1 %>% 
  filter(!site_code %in% ("mean_DS3"))
length(unique(data.mean2$site_code))
sum(data.mean2$mean_DS3>-0.69) #85 sites did not lose more than 50% of biomass
sum((data.mean2$mean_DS3>(-1.39) & data.mean2$mean_DS3<(-0.69))) #10 sites between 50-75% loss
sum(data.mean2$mean_DS3<(-1.39)) #5 sites lost more than 75% of biomass



#Average ANPP reduction for sgs, naposta, capwhite,broken hill, urat
data.ppt3<-data.mean2%>%filter(site_code %in% c('sgsdrt.us','naposta.ar', 'brokenh.au','capwhite.us','urat.cn'))
(exp(mean(data.ppt3$mean_DS3))*100)-100
#-82.74

