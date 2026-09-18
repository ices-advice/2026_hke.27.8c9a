#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHAKE combine CPUEs with inverse variance #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo  #
# Marta Cousido Rocha  #
# Santiago Cervino     #
# 03/2025           #
#~~~~~~~~~~~~~~~~~~~~~~~

## To see the outline press: Ctrl + shift + O

## https://es.abcdef.wiki/wiki/Inverse-variance_weighting
## In this document we combine CPUE indices by the inverse variance
## Finally we get the combined CPUE_trawlers and CPUE_volpal

# Clean env --------------------------------------------------------------------

## Clean environment
rm(list=ls())

## Set directory
dir<-paste(getwd(), "/boot/data/indices_CPUEs/", sep="") # dir

dir_save<-paste(getwd(), "/data/indices", sep="")


mkdir(dir_save)

# Packages 
library(readODS)
library(gridExtra)
library(ggplot2)
library(grid)

# CPUE_trawlers ----------------------------------------------------------------

## In order to make the indices comparable, we divide them by their own mean
## X=Indice; Var (X/mean(x)) <- (1/mean(x))^2 * var (x)

        
## Baka
ind_SpCPUE_baka<-read_ods(path = paste(dir, "HKEstdTrendsMetier.ods",sep=""), 
                             sheet="bacaOABtrend with INLA")
ind_SpCPUE_baka$mean<-ind_SpCPUE_baka$Mean/mean(ind_SpCPUE_baka$Mean)
ind_SpCPUE_baka$sd<-sqrt(((1/mean(ind_SpCPUE_baka$Mean))^2)*(ind_SpCPUE_baka$SD^2))
head(ind_SpCPUE_baka)

## Jurelera
ind_SpCPUE_jure<-read_ods(path = paste(dir, "HKEstdTrendsMetier.ods", sep=""), 
                          sheet="jureleraOABtrend with INLA")
ind_SpCPUE_jure$mean<-ind_SpCPUE_jure$Mean/mean(ind_SpCPUE_jure$Mean)
ind_SpCPUE_jure$sd<-sqrt(((1/mean(ind_SpCPUE_jure$Mean))^2)*(ind_SpCPUE_jure$SD^2))
head(ind_SpCPUE_jure)

## Pareja
ind_SpCPUE_pare<-read_ods(path = paste(dir, "HKEstdTrendsMetier.ods",sep=""), 
                          sheet="parejaOABtrend with INLA")
ind_SpCPUE_pare$mean<-ind_SpCPUE_pare$Mean/mean(ind_SpCPUE_pare$Mean)
ind_SpCPUE_pare$sd<-sqrt(((1/mean(ind_SpCPUE_pare$Mean))^2)*(ind_SpCPUE_pare$SD^2))
head(ind_SpCPUE_pare)

## Index mean value
x=ind_SpCPUE_baka$mean
y=ind_SpCPUE_jure$mean
z=ind_SpCPUE_pare$mean

## Index SD
sigma_x=ind_SpCPUE_baka$sd
sigma_y=ind_SpCPUE_jure$sd
sigma_z=ind_SpCPUE_pare$sd

weighted_average=function(x,y,z,sigma_x,sigma_y,sigma_z){
  
  num=(x/sigma_x^2)+(y/sigma_y^2)+(z/sigma_z^2)
  
  den=(1/sigma_x^2)+(1/sigma_y^2)+(1/sigma_z^2)
  
  return(list(index=(num/den),sd=sqrt(1/den))) # var=(1/den)
}

(comb=weighted_average(x,y,z,sigma_x,sigma_y,sigma_z))

## SS input format
##    year, season, index, obs, se  
ind_SpCPUE_trawlers<-cbind(ind_SpCPUE_baka$Year, as.data.frame(comb))
ind_SpCPUE_trawlers$seas<-rep(6)
ind_SpCPUE_trawlers$obs<-ind_SpCPUE_trawlers$index
ind_SpCPUE_trawlers$index<-rep("SpCPUE_trawlers")
ind_SpCPUE_trawlers<-ind_SpCPUE_trawlers[,c(1,4,2,5,3)]
colnames(ind_SpCPUE_trawlers)<-c("year","seas","index","obs","se")
head(ind_SpCPUE_trawlers)

## plot ------------------------------------------------------------------------

## baka

p1<-ggplot(data=ind_SpCPUE_baka, aes(x=(Year), y=mean, fill="#f37735"))+
  geom_line(aes(y = mean), color = "coral4", size=0.6) +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  ggtitle("CPUE otter 1 (baka)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE") + theme(legend.position="none")

## jurelera
p2<-ggplot(data=ind_SpCPUE_jure, aes(x=(Year), y=mean, fill="#f37735"))+
  geom_line(aes(y = mean), color = "coral4", size=0.6) +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  ggtitle("CPUE otter 2 (jurelera)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE") + theme(legend.position="none")

## pareja

p3<-ggplot(data=ind_SpCPUE_pare, aes(x=(Year), y=mean, fill="#f37735"))+
  geom_line(aes(y = mean), color = "coral4", size=0.6) +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  ggtitle("CPUE pair")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE") + theme(legend.position="none")

## Combined

p4<-ggplot(data=ind_SpCPUE_trawlers, aes(x=(year), y=obs))+
  geom_line(aes(y = obs), color = "dodgerblue2", size=0.7) +
  geom_ribbon(aes(y = obs, ymin = obs - se, ymax = obs + se), fill="gray22", alpha = .2) +
  ggtitle("Combined CPUE trawlers")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE")+ theme(legend.position="none")

library(gridExtra)


grid.arrange(
  arrangeGrob(p1, p2, p3, ncol = 3), 
  p4,                                
  nrow = 2,                           
  heights = c(2, 2)                   
)


p <- grDevices::recordPlot()  
grDevices::jpeg(paste(dir_save,"/Figure 10.4b.jpeg",sep=""),width=2500, height=1000,res=300)    
grDevices::replayPlot(p)    
grDevices::dev.off()


# CPUE_vol ----------------------------------------------------------------

## In order to make the indices comparable, we divide them by their own mean
## X=Indice; Var (X/mean(x)) <- (1/mean(x))^2 * var (x)


## Volanta (delete the first year because palangre starts one latter)
ind_SpCPUE_vol<-read_ods(path = paste(dir, "/HKEstdTrendsMetier.ods",sep=""), 
                         sheet="volantaOABtrend with INLA")[-1,]

# Complete the data!
vec<-c(2012,	201.5,	136,	NA,	NA)
ind<-which(ind_SpCPUE_vol==2012)
ind_SpCPUE_vol[ind,]<-vec



ind_SpCPUE_vol$mean<-ind_SpCPUE_vol$Mean/mean(ind_SpCPUE_vol$Mean, na.rm=TRUE)
ind_SpCPUE_vol$sd<-sqrt(((1/mean(ind_SpCPUE_vol$Mean,na.rm=TRUE))^2)*(ind_SpCPUE_vol$SD^2))
head(ind_SpCPUE_vol)

## Palangre
ind_SpCPUE_pal<-read_ods(path = paste(dir,"/HKEstdTrendsMetier.ods",sep=""), 
                          sheet="palangreDEAtrend_LLS+PAL with INLA")
ind_SpCPUE_pal$mean<-ind_SpCPUE_pal$Mean/mean(ind_SpCPUE_pal$Mean)
ind_SpCPUE_pal$sd<-sqrt(((1/mean(ind_SpCPUE_pal$Mean))^2)*(ind_SpCPUE_pal$SD^2))
head(ind_SpCPUE_pal)

## Index mean value
x=ind_SpCPUE_vol$mean
y=ind_SpCPUE_pal$mean

## Index SD
sigma_x=ind_SpCPUE_vol$sd
sigma_y=ind_SpCPUE_pal$sd

weighted_average=function(x,y,sigma_x,sigma_y){
  
  num=(x/sigma_x^2)+(y/sigma_y^2)
  
  den=(1/sigma_x^2)+(1/sigma_y^2)
  
  return(list(index=(num/den),sd=sqrt(1/den))) # var=(1/den)
}

(comb=weighted_average(x,y,sigma_x,sigma_y))

## SS input format
##    year index      obs seas        se
ind_SpCPUE_volpal<-cbind(ind_SpCPUE_vol$Year, as.data.frame(comb))
ind_SpCPUE_volpal$seas<-rep(6)
ind_SpCPUE_volpal$obs<-ind_SpCPUE_volpal$index
ind_SpCPUE_volpal$index<-rep("SpCPUE_volpal")
ind_SpCPUE_volpal<-ind_SpCPUE_volpal[,c(1,4,2,5,3)]
colnames(ind_SpCPUE_volpal)<-c("year","seas","index","obs","se")
head(ind_SpCPUE_volpal)

## plot ------------------------------------------------------------------------

## volanta

p1<-ggplot(data=ind_SpCPUE_vol, aes(x=(Year), y=mean, fill="#f37735"))+
  geom_line(aes(y = mean), color = "coral4", size=0.6) +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  ggtitle("CPUE gillnets")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE") + theme(legend.position="none")

## palangre

p2<-ggplot(data=ind_SpCPUE_pal, aes(x=(Year), y=mean, fill="#f37735"))+
  geom_line(aes(y = mean), color = "coral4", size=0.6) +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2) +
  ggtitle("CPUE longliners")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE") + theme(legend.position="none")


## Combined

p3<-ggplot(data=ind_SpCPUE_volpal, aes(x=(year), y=obs))+
  geom_line(aes(y = obs), color = "dodgerblue2", size=0.7) +
  geom_ribbon(aes(y = obs, ymin = obs - se, ymax = obs + se), fill="gray22", alpha = .2) +
  ggtitle("Combined CPUE volpal")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("year")+ylab("CPUE")+ theme(legend.position="none")

grid.arrange(
  arrangeGrob(p1, p2, ncol = 2),  
  p3,                              
  nrow = 2,                        
  heights = c(2, 2)                
)

p <- grDevices::recordPlot()  
grDevices::jpeg(paste(dir_save,"/Figure 10.4a.jpeg",sep=""),width=2500, height=1000,res=300)    
grDevices::replayPlot(p)    
grDevices::dev.off()


# save * -----------------------------------------------------------------------
# change the year!
save(ind_SpCPUE_trawlers,ind_SpCPUE_volpal, file=paste(dir_save, "/ind CPUEs combined 2003-2025.RData",sep=""))



