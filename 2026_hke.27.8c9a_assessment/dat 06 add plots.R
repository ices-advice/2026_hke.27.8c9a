#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHAKE Survey plots      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido, Anxo Paz and Santiago Cerviño #
#~~~~~~~~~~~~~~~~~~~~~~
# 24/02/2023         #
#~~~~~~~~~~~~~~~~~~~~~~

dir<-getwd()

assessment_year<-2025
# Table 10_4 surveys info ------------------------------------------------------

Tab10_4 <- read_excel("boot/data/Report tables last year/Tab10.4.xlsx")

# SpGFS-WIBTS-Q4 (G2784) (/30 min)					
# Bio and Abun
sp_2<- read_excel("boot/data/Surveys/SpGFS-WIBTS-Q4 (G2784).xlsx", sheet="Abundance indices")

ind=which(sp_2[,1]==assessment_year)

bio=sp_2[ind[1],c(8,9)]
abu=sp_2[ind[2],c(8,9)]

# Hauls 

sp_1<- read_excel("boot/data/Surveys/SpGFS-WIBTS-Q4 (G2784).xlsx", sheet="Effort")
ind=which(sp_1[,1]==assessment_year)
hauls=sp_1[ind,8]

# Rec

sp_4<- read_excel("boot/data/Surveys/SpGFS-WIBTS-Q4 (G2784).xlsx", sheet="Lengths")

ind=which(sp_4[5,]==assessment_year)

ind2=which(sp_4[,1]==19)

rec=sum(sp_4[6:ind2,ind])

vec1=c(bio,hauls,abu,rec)

# SPGFS-caut-WIBTS-Q4 (G4309) (/hour)

cad<-read_excel("boot/data/Surveys/SPGFScaut-WIBTS-Q4 (G4309).xlsx", sheet="__tabla años ARSA noviembre")

ind_y=which(cad[,1]==assessment_year)

ind=which(colnames(cad)==assessment_year)

ind2=which(cad$TALLA==19)

rec=sum(cad[1:ind2,ind],na.rm=TRUE)


vec2<-c(cad[ind_y,]$`Biomasa (kg/h)`,
        cad[ind_y,]$`σ Biomasa`,
        cad[ind_y,]$`Lances Validos`,
        rec
        
)


# SPGFS-cspr-WIBTS-Q1 (G7511) (/hour)

# cad2<-read_excel("boot/data/Surveys/SPGFScaut-WIBTS-Q4 (G4309).xlsx", sheet="__tabla años ARSA marzo")
# 
# ind=which(colnames(cad2)==assessment_year)
# 
# ind2=which(cad2$TALLA==19)
# 
# rec=sum(cad2[1:ind2,ind],na.rm=TRUE)
# 
# ind=which(cad2[,1]==assessment_year)

# In 2025 the survey was not carried out!
# vec3<-c(cad2[ind,]$`Biomasa (kg/h)`,
#         cad2[ind,]$`σ Biomasa`,
#         cad2[ind,]$`Lances Validos`,
#         rec
# )

vec3<-c(NA,
         NA,
         NA,
         NA
 )

vec=c(assessment_year,vec1,vec2,vec3)
names(vec)<-colnames(Tab10_4)
Tab10_4=rbind(Tab10_4,vec)

num_columnas <- ncol(Tab10_4)


columnas_excluidas <- c(1, 4, 7, 10,14)


for (i in 2:num_columnas) {
  
  if (!(i %in% columnas_excluidas)) {
    Tab10_4[[i]] <- as.numeric(Tab10_4[[i]])
  }
}

Tab10_4[[7]] <- as.numeric(Tab10_4[[7]])
Tab10_4[[14]] <- as.numeric(Tab10_4[[14]])
write.xlsx(Tab10_4, paste(dir,"/data/indices/Tab10.4.xlsx",sep=""),rowNames = FALSE)



  
# Figure 10.3 (Surveys plot)

## PtSurv ----------------------------------------------------------------------
library(readxl)
PtGFS<- read_excel(paste(dir,"/boot/data/Surveys/ptGFS-WIBTS-Q4 (G8899).xlsx",sep=""),sheet="PT PGFS index",skip=1)
PtGFS=subset(PtGFS,PtGFS$...1>=1985)
Biomass_PtGFS=as.numeric(PtGFS[,2]$Mean...2)
se_PtGFS=as.numeric(PtGFS[,3]$s.e....3)
Rec_PtGFS=as.numeric(PtGFS[,6]$`n/hour <20cm`)
years_PtGFS=as.numeric(PtGFS[,1]$...1)

## SpSurv and CdSurv ---------------------------------------------------------------


SpCdSurveys <- read_excel(paste0(dir,"/data/indices/Tab10.4.xlsx"))
ind=which(SpCdSurveys$Year>=1983 & SpCdSurveys$Year<=assessment_year)
SpCdSurveys=SpCdSurveys[ind,]

Biomass_Sp=as.numeric(SpCdSurveys$`G2784 Bio Mean`)*(50/12)
se_Sp=as.numeric(SpCdSurveys$`G2784 Bio s.e.`)*(50/12)
Rec_Sp=as.numeric(SpCdSurveys$`G2784 Rec Mean`)*0.5
years_Sp=as.numeric(SpCdSurveys$Year)

Biomass_Cd=as.numeric(SpCdSurveys$`G4309 Bio Mean`)
se_Cd=as.numeric(SpCdSurveys$`G4309 Bio s.e.`)
Rec_Cd=as.numeric(SpCdSurveys$`G4309 Rec Mean`)

bio=c(Biomass_PtGFS,Biomass_Sp,Biomass_Cd)
se=c(se_PtGFS,se_Sp,se_Cd)
rec=c(Rec_PtGFS,Rec_Sp,Rec_Cd)
year=c(years_PtGFS,years_Sp,years_Sp)

survey=c(rep("PtGFS-WIBTS-Q4",length(years_PtGFS)),
         rep("SpGFS-WIBTS-Q4",length(years_Sp)),
         rep("SpGFS-caut-WIBTS-Q4",length(years_Sp)))
dat=data.frame(bio,se,rec, year,survey)

# Introduce missing NA's in "PtGFS-WIBTS-Q4"

ind=which(dat$survey=="PtGFS-WIBTS-Q4" & dat$year==2018)

dat1=dat[1:ind,]

dat2=dat[(ind+1):dim(dat)[1],]

dat_int=dat1[c(1,2),]
dat_int$bio=NA
dat_int$se=NA
dat_int$rec=NA
dat_int$year=c(2019,2020)


dat=rbind(dat1,dat_int,dat2)


library(ggplot2)
bio=ggplot(dat,aes(x=year,y=bio,colour=survey,group=survey,fill=survey) )+
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=bio-se, ymax=bio+se), alpha=.3, linetype=0) +
  theme_minimal()+
  scale_y_continuous(name="PtGFS and SpGFS-caut (Kg/h)",sec.axis=sec_axis(~.*(12/50),name = "SpGFS (Kg/30min)"))+
  scale_x_continuous(name ="Years",  breaks=1983:assessment_year)+
  theme(axis.text.x = element_text(face="bold",  angle=90),legend.position="none",axis.title.y.right = element_text(angle=90))+
  ggtitle("Biomass indices")

rec=ggplot(dat,aes(x=year,y=rec,colour=survey,group=survey,fill=survey) )+
  geom_point() +
  geom_line() +
  #geom_ribbon(aes(ymin=rec-se, ymax=rec+se), alpha=.3, linetype=0) +
  theme_minimal()+
  scale_y_continuous(name="PtGFS and SpGFS-caut (n/h)",sec.axis=sec_axis(~.*(2),name = "SpGFS (n/30min)"))+
  scale_x_continuous(name ="Years",  breaks =1983:assessment_year)+
  theme(axis.text.x = element_text(face="bold",  angle=90),legend.position = c(0.2, 0.8),legend.title=element_blank(),axis.title.y.right = element_text(angle=90))+
  ggtitle("Recruitment indices (<20cm)")

bio
rec



library(gridExtra)
library(ggplot2)
library(grid)
rec_bio=grid.arrange(rec,bio,nrow=2)
print(rec_bio)
p <- grDevices::recordPlot()  
grDevices::jpeg(paste(dir,"/data/indices/figure 10.3.png",sep=""),width=2500, height=2500,res=300)    
grDevices::replayPlot(p)    
grDevices::dev.off()



# means <- aggregate(. ~ survey, data = dat, FUN = mean)
# 
# bio_means <- bio + geom_hline( data = means, aes( yintercept = bio, col = survey) ,linetype = 'dashed')
# rec_means <- rec + geom_hline( data = means, aes( yintercept = rec, col = survey) ,linetype = 'dashed')
# 
# rec_bio_means <- grid.arrange(rec_means, bio_means, nrow=2)
# print(rec_bio_means)
# 
# p2 <- grDevices::recordPlot()  
# grDevices::jpeg( paste0(getwd(),'/../Extra thinking/rec_bio_means.png'),width=2500, height=2500,res=300)    
# grDevices::replayPlot(p2)    
# grDevices::dev.off()

