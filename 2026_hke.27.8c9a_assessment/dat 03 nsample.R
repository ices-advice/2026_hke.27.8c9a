#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHAKE LFD Number of samples (NSample) #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
# Marta Cousido       #
# Santiago Cervino    #
# 31/04/2023          #
#~~~~~~~~~~~~~~~~~~~~~~

## To see the outline press: Ctrl + shift + O

## NSample is used in SS in order to give weights to the LFDs by fleet and Year
## In this script we will make an aproximation of NSample coming from ICES reports
## For the surveys, we have the number of hauls and we trust that surveys have a
## good sampling coverage

# Clean env --------------------------------------------------------------------

## Clean environment
rm(list=ls())

library(icesTAF)
assessment_year<-2025
# Fleets -----------------------------------------------------------------------

# 2014-2024 -------------------------------------------------------------------

## Fleets NSamp

## INTERCATCH fleet names must be translated to:
## OT        trawlers 
## PT        trawlers 
## PS        artisanal
## GNS 100   artisanal
## GNS 60-79 artisanal 
## MIS       artisanal
## GNS 80-99 volpal
## LLS       volpal


data=data.frame(matrix(0,ncol=7,nrow=1))
colnames(data)<-c("Year","Season","Fleets","Country", "Catch.Cat.", "NumLengthMeasurements", "NumSamplesLength")


x=paste0(getwd(),"/boot/data/intercatch/", "NumbersAtAgeLength.txt", sep="")
dat=read.table(x, header = TRUE, sep = "\t",skip=2)
aux<-dat[, c("Year","Season","Fleets","Country", "Catch.Cat.", "NumLengthMeasurements", "NumSamplesLength")]
data=rbind(data,aux)
data<-data[-1,]

## Fleets
data.frame(Fleets = sort(unique(data$Fleets))) 
dim(data)
data<-subset(data,data$Country!="France")
dim(data)

## Rename fleets
library(plyr)
data_mut<-mutate(data, Fleets2 = revalue(Fleets, c(  "GNS_DEF_>=100_0_0" = "artisanal",
                                                     "GNS_DEF_60-79_0_0" = "artisanal",
                                                     "GNS_DEF_80-99_0_0" = "volpal",
                                                     "GTR_DEF_60-79_0_0" = "artisanal",
                                                     "LLS_DEF_0_0_0" = "volpal",
                                                   #  "LLS_DEF"=  "volpal",
                                                     "MIS_MIS_0_0_0"="artisanal",
                                                     "OTB"="trawlers",
                                                     "OTB_CRU_>=55_0_0"="trawlers",
                                                     "OTB_DEF_>=55_0_0"="trawlers",
                                                     "OTB_MCD_>=55_0_0"="cdTrawl",
                                                     "OTB_MPD_>=55_0_0"="trawlers",
                                                     #"PS_SPF_0_0_0"="artisanal",
                                                     "PTB_MPD_>=55_0_0"="trawlers")))
## Aggregate data
sampLev <- aggregate(data_mut[, c("NumLengthMeasurements", "NumSamplesLength")],
                     by=data_mut[, c("Year","Season", "Catch.Cat.", "Fleets2")], sum)  
sampLev<-sampLev[order(sampLev$Year), ]


# Join with previous information

sampLev_previous<-read.csv(paste(getwd(), "/boot/data/LFDs nsample intercatch/nsample fleets.csv",sep=""))
max_NumLengthMeasurements_2014_2023<- 13350
max_NumSamplesLength_2014_2023<- 209
## Standardize (divide by max value) the columns LengthMeasurements and NumSamples
## After this, we do the average to get a oMean (NSample) reference column
library(dplyr)
sampLev$oMeasurements<-(sampLev$NumLengthMeasurements/max_NumLengthMeasurements_2014_2023*100)
sampLev$oLength<-(sampLev$NumSamplesLength/max_NumSamplesLength_2014_2023*100)
sampLev$oMean <- rowMeans(sampLev[,c('oMeasurements', 'oLength')], na.rm=TRUE)
head(sampLev)



dataL<-sampLev%>%filter(Catch.Cat.!="D")
dataL<-dataL[ c(1,2,9,4)]
colnames(dataL)<-colnames(sampLev_previous)

dataD<-sampLev%>%filter(Fleets2=="trawlers" & Catch.Cat.!="L")
dataD$Fleets2 <-rep("disc")
dataD<-dataD[ c(1,2,9,4)]
colnames(dataD)<-colnames(sampLev_previous)

sampLev<-rbind(sampLev_previous,dataL,dataD)

# Save -----------------------------------------------------------------------

head(sampLev);tail(sampLev)
mkdir("data/LFDs/nsample")
write.csv(sampLev, file=paste(getwd(),"/data/LFDs/nsample/nsample fleets.csv",sep=""), row.names=FALSE, quote=FALSE)


# Surveys ----------------------------------------------------------------------

## Surveys are weighted by the number of hauls

## We think that surveys are more trustable than fleets samples


nsample=read.csv(paste(getwd(),"/boot/data/LFDs nsample surveys/nsample surveys.csv",sep=""))

NSample_spsurv<-subset(nsample,nsample$Fleet=="SpSurv")[,-3]

# Extract assessment year
# Hauls 

sp_1<- read_excel("boot/data/Surveys/SpGFS-WIBTS-Q4 (G2784).xlsx", sheet=1)
ind=which(sp_1[,1]==assessment_year)
hauls=sp_1[ind,8]
vec<-c(assessment_year,hauls)
names(vec)<-colnames(NSample_spsurv)
NSample_spsurv<-rbind(NSample_spsurv,vec)

NSample_spsurv$fleet<-rep("SpSurv")
colnames(NSample_spsurv)<-c("Year","NSamp","Fleet")


NSample_ptsurv<-subset(nsample,nsample$Fleet=="PtSurv")[,-3]

# Extract assessment year
ptGFS_WIBTS_Q4_G8899<- read_excel("boot/data/Surveys/ptGFS-WIBTS-Q4 (G8899).xlsx", sheet=3)

ind=which(ptGFS_WIBTS_Q4_G8899[,1]==assessment_year)

vec=ptGFS_WIBTS_Q4_G8899[ind,-1]
vec<-c(assessment_year,vec$...7)
names(vec)<-colnames(NSample_ptsurv)

NSample_ptsurv<-rbind(NSample_ptsurv,vec)

NSample_ptsurv$fleet<-rep("PtSurv")
colnames(NSample_ptsurv)<-c("Year","NSamp","Fleet")

NSamp_surv<-rbind(NSample_spsurv,NSample_ptsurv)

# Save ------------------------------------------------------------------------
head(NSamp_surv)


write.csv(NSamp_surv, file=paste(getwd(),"/data/LFDs/nsample/nsample surveys.csv",sep=""), row.names=FALSE, quote=FALSE)
