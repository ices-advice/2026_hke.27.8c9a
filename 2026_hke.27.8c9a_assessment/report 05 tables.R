#~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Report Tables ~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido          #
# Anxo Paz               #
# Santiago Cervino       #
# 26/03/2024             #
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

rm(list=ls()) 
library(r4ss)
library(tidyverse)
library(tidyverse) 
library(plyr) 
library(dplyr)
library(conflicted)
library(openxlsx)
library(writexl)
library(readr)
library(readxl)
library(icesAdvice)
conflict_prefer("summarise", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarize", "dplyr")

## Model path
run <- 'model/final' ## *CHANGE name
mod_path <- paste0(getwd(), "/",run, sep="") 

## Create plots folder
tabledir<-paste(getwd(), '/report/table', sep="")
dir.create(path=tabledir, showWarnings = T, recursive = T)

## Indicate dir
data.file <- 'shake_data.ss'
data.file  <- file.path(mod_path, data.file)
run <- paste0("_", run)


## Model datafile
start <- r4ss::SS_readstarter(file = file.path(mod_path, "starter.ss"), 
                              verbose = FALSE)
ss3Dat <- r4ss::SS_readdat(file = file.path(mod_path, start$datfile),
                           verbose = FALSE)


# 1) Update table 10.1. CatchTrends --------------------------------------------
assessment_year=2025

Tab10_1_sheet_1 <- read_excel("boot/data/Report tables last year/Tab10.1.xlsx", sheet = 1,col_names=TRUE)
Tab10_1_sheet_1 <- as.data.frame(Tab10_1_sheet_1)



# a)


tab1 <- read_csv("data/Intercatch/tab1.csv")


# Create the last year vector


ll=dim(Tab10_1_sheet_1)[2]
vec=c(assessment_year,tab1[,1:9],sum(tab1[,1:7],na.rm=TRUE),tab1[,10:12],sum(tab1[,10:11],na.rm=TRUE), 
      tab1[,13],tab1[,14])
names(vec) <- colnames(Tab10_1_sheet_1)[-((ll-2):ll)]

land=vec$`sp LAND`+vec$`pt LAND`+vec$`fr TOTAL`+vec$UNALLOCATED
vec=c(vec,vec$`sp DISC`+vec$`pt DISC`, #+vec$`sp BMS`,
     land ,
      vec$`sp DISC`+vec$`pt DISC`+land)#vec$`sp BMS`)

names(vec)<-colnames(Tab10_1_sheet_1)
Tab10_1_sheet_1<-rbind(Tab10_1_sheet_1,vec)

Tab10_1_sheet_1 <- as.data.frame(lapply(Tab10_1_sheet_1, as.numeric))
colnames(Tab10_1_sheet_1)<-names(vec)

Tab10_1_sheet_1_round <- round(Tab10_1_sheet_1, 2)

write_xlsx(Tab10_1_sheet_1, "report/table/Tab10.1.xlsx")


# b) Sumsheet table -----------------------------------------------------------

tabledir_Sumsheet<-paste(getwd(), '/report/table/sumsheet', sep="")
dir.create(path=tabledir_Sumsheet, showWarnings = T, recursive = T)

data <- data.frame(Year=Tab10_1_sheet_1$YEAR,
  Sp_Discards = rowSums(cbind(Tab10_1_sheet_1$`sp DISC` * 1000,  Tab10_1_sheet_1$`sp BMS` * 1000), na.rm = TRUE),
  Sp_Landings = Tab10_1_sheet_1$`sp LAND`*1000,
  Pt_Discards = Tab10_1_sheet_1$`pt DISC`*1000,
  Pt_Landings = Tab10_1_sheet_1$`pt LAND`*1000,
  Fr_Landings = Tab10_1_sheet_1$`fr TOTAL`*1000,
  Unallocated = Tab10_1_sheet_1$UNALLOCATED*1000,
  Tot_Discards = Tab10_1_sheet_1$`tot DISC`*1000,
  Tot_Landings = Tab10_1_sheet_1$`tot LAND`*1000,
  Tot_Catch = Tab10_1_sheet_1$`tot CATCH`*1000,
  stringsAsFactors = FALSE
)
data_rounded <- round(data, 0)
write_xlsx(data_rounded, paste(tabledir_Sumsheet, "/catchtrends.xlsx", sep=""), col_names = TRUE)

# 2) Tab 10.2. Length distributions -------------------------------------------

ldfs=ss3Dat$lencomp

ldfs=subset(ldfs,ldfs$FltSvy==1 |ldfs$FltSvy==2 |ldfs$FltSvy==3|ldfs$FltSvy==4)
ldfs=subset(ldfs,ldfs$Yr==assessment_year)

## Landings --------------------------------------------------------------------

landings=subset(ldfs,ldfs$Part==2 | ldfs$Part==0)

land=colSums(landings[,7:dim(landings)[2]])

land=land[1:(length(land)/2)]

nam=names(land)

ind=which(nam=="f39")

dat=data.frame(land[1:ind],sort(c(1:(ind/2),1:(ind/2))))
colnames(dat)=c("land","group")

sumland=plyr::ddply(dat, .(group), summarize,  sum.y=sum(land))

len=seq(4,100,by=2)
land=c(sumland[,2],land[(ind+1):length(land)])

landings=data.frame(len, land)

## Discards --------------------------------------------------------------------

discards=subset(ldfs,ldfs$Part==1)

discards=colSums(discards[,7:dim(discards)[2]])

discards=discards[1:(length(discards)/2)]

nam=names(discards)

ind=which(nam=="f39")

dat=data.frame(discards[1:ind],sort(c(1:(ind/2),1:(ind/2))))
colnames(dat)=c("disc","group")

sumdisc=plyr::ddply(dat, .(group), summarize,  sum.y=sum(disc))

len=seq(4,100,by=2)
discards=c(sumdisc[,2],discards[(ind+1):length(discards)])

Table_lds=cbind(landings,discards)

ll=dim(Table_lds)[1]

Table_lds[ll-1,2]=Table_lds[ll-1,2]+Table_lds[ll,2]

Table_lds=Table_lds[-ll,]

ll=dim(Table_lds)[1]
Table_lds$Catch=Table_lds$land+Table_lds$discards
ind_last=dim(Tab10_1_sheet_1)[1]
Table_lds=rbind(Table_lds,c("TOTAL",colSums(Table_lds[,-1])))
Table_lds=rbind(Table_lds,c("Weight (000' tons)",c( Tab10_1_sheet_1$`tot LAND`[ind_last],
                                                   Tab10_1_sheet_1$`tot DISC`[ind_last],
                                                   Tab10_1_sheet_1$`tot CATCH`[ind_last])))
# Weight length parameters 
a=	0.000003770
b=	3.168260000

wei=a*(as.numeric(Table_lds$len[1:ll])+1)^b
sop_land=sum(as.numeric(Table_lds$land[1:ll])*wei)/1000
sop_dis=sum(as.numeric(Table_lds$discards[1:ll])*wei)/1000
sop_catch=sum(as.numeric(Table_lds$Catch[1:ll])*wei)/1000

Table_lds=rbind(Table_lds,c("SOP",c( sop_land,
                                     sop_dis,
                                     sop_catch)))

ll2=dim(Table_lds)[1]
Table_lds=rbind(Table_lds,c("SOP/NW",c( as.numeric(Table_lds[ll2,2])/as.numeric(Table_lds[ll2-1,2]),
                                        as.numeric(Table_lds[ll2,3])/as.numeric(Table_lds[ll2-1,3]),
                                        as.numeric(Table_lds[ll2,4])/as.numeric(Table_lds[ll2-1,4]))))


le=(as.numeric(Table_lds$len[1:ll]))
le_land=sum(as.numeric(Table_lds$land[1:ll])*le)/as.numeric(Table_lds$land[ll+1])
le_dis=sum(as.numeric(Table_lds$discards[1:ll])*le)/as.numeric(Table_lds$discards[ll+1])
le_catch=sum(as.numeric(Table_lds$Catch[1:ll])*le)/as.numeric(Table_lds$Catch[ll+1])


Table_lds=rbind(Table_lds,c("Mean length (cm)",1+c(le_land,le_dis,le_catch)))

ind_ll<-dim(Table_lds)[1]
                

for (i in 2:4) {
  for (j in 1:(ll+1)) {
    if (!is.na(Table_lds[j, i])) {
      Table_lds[j, i] <- as.integer(round(as.numeric(Table_lds[j, i]), 0))
    }
  }
}


for (i in 2:4) {
  for (j in (ll+2):(ll+6)) {
    if (!is.na(Table_lds[j, i])) {
      Table_lds[j, i] <- round(as.numeric(Table_lds[j, i]), 2)
    }
  }
}


write.xlsx(Table_lds, paste(tabledir, "/Tab10.2.xlsx", sep=""),rowNames = FALSE)
# Part 1=discards, 2=retained, 0=mixed  

# 3) PtSurvey ------------------------------------------------------------------

Tab10_3 <- read_excel("boot/data/Report tables last year/Tab10.3.xlsx")

 ptGFS_WIBTS_Q4_G8899<- read_excel("boot/data/Surveys/ptGFS-WIBTS-Q4 (G8899).xlsx", sheet="PT PGFS index")
 
 ind=which(ptGFS_WIBTS_Q4_G8899[,1]==assessment_year)
 
 vec=ptGFS_WIBTS_Q4_G8899[ind,-1]
 
 vec=c(assessment_year,rep(NA,10),as.numeric(vec))
 names(vec)<-colnames(Tab10_3)
 Tab10_3=rbind(Tab10_3,vec)
 

 num_columnas <- ncol(Tab10_3)
 
 columnas_excluidas <- c(1, 6, 11, 17)
 
 
 for (i in 2:num_columnas) {
 
   if (!(i %in% columnas_excluidas)) {
 
     Tab10_3[[i]] <- (as.numeric(Tab10_3[[i]]))
  }
 }
 

write.xlsx(Tab10_3, paste(tabledir, "/Tab10.3.xlsx", sep=""),rowNames = FALSE)




# 4) SpSurvey ------------------------------------------------------------------

ruta_origen <- file.path(getwd(), "data", "indices", "Tab10.4.xlsx")
ruta_destino <- file.path(tabledir, "Tab10.4.xlsx")

file.rename(from = ruta_origen, to = ruta_destino)



# 5) CPUE ----------------------------------------------------------------------


load(paste(getwd(), "/data/all data.RData", sep=""))

tail(ind_SpCPUE_trawlers) 
tail(ind_SpCPUE_volpal) 

aux_volpal=ind_SpCPUE_volpal[1:6,-(2:3)]
aux_volpal$year=0
aux_volpal$obs=0
aux_volpal$se=0

ind_SpCPUE_volpal=rbind(aux_volpal,ind_SpCPUE_volpal[,-(2:3)])

Table_cpue=cbind(ind_SpCPUE_trawlers[,-(2:3)],ind_SpCPUE_volpal[,-1])

colnames(Table_cpue)=c("years","Trawlers","s.e","volpal","s.e")
write.xlsx(Table_cpue, paste(tabledir, "/Tab10.5.xlsx", sep=""),rowNames = FALSE)

# 6) Assessment summary ---------------------------------------------------------

output <- SS_output(dir = mod_path, repfile = "Report.sso", 
                    compfile = "CompReport.sso",covarfile = "covar.sso", 
                    ncols = 200,forefile="forecast.ss",warn = TRUE,covar = TRUE, 
                    verbose = TRUE,
                    printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE,
                    aalmaxbinrange=0)

dtyr <- output$endyr
years     <- output$startyr:dtyr
yearsfore <- output$startyr:(dtyr+output$N_forecast_yrs) 
nyears    <- length(years)

## SSB, REC and F
ssbrecf <- output$derived_quants

## SSB ------------------------------------------------------------------------
spb <- ssbrecf[substr(ssbrecf$Label,1,3)=="SSB",]
ssb <- spb[3:(3+length(yearsfore)-1), 2:3]
# upperssb <- ssb[(1:length(years)),1] +  2 * ssb[(1:length(years)),2]
# lowerssb <- ssb[(1:length(years)),1] -  2 * ssb[(1:length(years)),2]

upperssb  <- exp(log(ssb[(1:length(years)),1])+sqrt(log(1+(1.6449*ssb[(1:length(years)),2]/ssb[(1:length(years)),1])^2)))
lowerssb <- exp(log(ssb[(1:length(years)),1])-sqrt(log(1+(1.6449*ssb[(1:length(years)),2]/ssb[(1:length(years)),1])^2)))

ssb=ssb[(1:length(years)),1]

# Last year + 1
ssb_last <- spb[length(yearsfore), 2:3]
# upperssb_last <- ssb_last[,1] +  2 * ssb_last[,2]
# lowerssb_last <- ssb_last[,1] -  2 * ssb_last[,2]

upperssb_last  <- exp(log(ssb_last[,1])+sqrt(log(1+(1.6449*ssb_last[,2]/ssb_last[,1])^2)))

lowerssb_last <- exp(log(ssb_last[,1])-sqrt(log(1+(1.6449*ssb_last[,2]/ssb_last[,1])^2)))

ssb_last=ssb_last[,1]


names=c("years","rec_low", "rec_value", "rec_upp", "ssb_low",
  "ssb_val", "ssb_upp", "Bio",
  "F_low", "F_val", "F_upp","catch", "landings", "discards")
vec=rep(NA,length(names))
names(vec)=names
vec["years"]=years[length(years)]+1
vec["ssb_low"]=lowerssb_last
vec["ssb_val"]=ssb_last
vec["ssb_upp"]=upperssb_last

## Rec -------------------------------------------------------------------------
recr <- ssbrecf[substr(ssbrecf$Label,1,4)=="Recr",]
recr <- recr[(1:length(years))+2,2:3]

# upperrecr<- recr[,1] +  2 * recr[,2]
# lowerrecr <- recr[,1] -  2 * recr[,2]

upperrecr  <- exp(log(recr[,1])+sqrt(log(1+(1.6449*recr[,2]/recr[,1])^2)))

lowerrecr <- exp(log(recr[,1])-sqrt(log(1+(1.6449*recr[,2]/recr[,1])^2)))

rec=recr[,1]

# Last year + 1 (take care if you use geom mean in the forecast)
# val=paste("Recr_",years[length(years)]+1,sep="")
# rec_last <- ssbrecf[substr(ssbrecf$Label,1,9)==val,]

# Read one of the forecasts with hessian

output_fore <- SS_output(dir = paste0(getwd(),"/model/final/Forecast/Fmult0"), repfile = "Report.sso", 
                    compfile = "CompReport.sso",covarfile = "covar.sso", 
                    forefile="forecast.ss",warn = TRUE,covar = TRUE, 
                    verbose = TRUE,
                    printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE,
                    aalmaxbinrange=0)
ssbrecf_fore <- output_fore$derived_quants

val=paste("Recr_",years[length(years)]+1,sep="")
rec_last <- ssbrecf_fore[substr(ssbrecf_fore$Label,1,9)==val,]

upperrec_last  <- exp(log(rec_last[,2])+sqrt(log(1+(1.6449*rec_last[,3]/rec_last[,2])^2)))

lowerrec_last <- exp(log(rec_last[,2])-sqrt(log(1+(1.6449*rec_last[,3]/rec_last[,2])^2)))

rec_last=rec_last[,2]

vec["rec_low"]=lowerrec_last
vec["rec_value"]=rec_last
vec["rec_upp"]=upperrec_last

## F ---------------------------------------------------------------------------

fratenum <- ssbrecf[substr(ssbrecf$Label,1,2)=="F_",]; 
fratenum <- fratenum[,2:3]

# upperf <- fratenum[1:nyears,1]+ 2 * fratenum[1:nyears,2]
# lowerf <- fratenum[1:nyears,1]- 2 * fratenum[1:nyears,2]

upperf  <- exp(log(fratenum[1:nyears,1])+sqrt(log(1+(1.6449*fratenum[1:nyears,2]/fratenum[1:nyears,1])^2)))

lowerf <- exp(log(fratenum[1:nyears,1])-sqrt(log(1+(1.6449*fratenum[1:nyears,2]/fratenum[1:nyears,1])^2)))

f=fratenum[1:nyears,1]



## Land and discards------------------------------------------------------------

catch=ss3Dat$catch

catch=ddply(catch, .(year, fleet), summarize, catch=sum(catch))

catch_sum=catch[order(catch$fleet),]

ind=which(catch_sum$year==-999)

catch_sum=catch_sum[-ind,]

# trawlers, volpal, artisanal and cdTrw.


catch_total=ddply(catch_sum, .(year), summarize, catch=sum(catch))

disc=ss3Dat$discard_data

discards=ddply(disc, .(Yr, Flt), summarize, Discard=sum(Discard))

# IMPORTANT!!! Input 2020 estimated discard by hand (the value is in output 01 script)
# ADG decided to eliminated it!
ll=dim(discards)[1]
ind=which(discards$Yr==2019)+1
disc=c(rep(0,length(1960:1993)),discards[1:(ind-1),3],684,discards[c((ind):ll),3])
catches=disc+catch_total[,2]


Bio=as_tibble(output$timeseries) %>%
  
  filter(Seas == 1) %>%
  
  select("Yr", "Era", "Seas", 'Bio_all')
ind=which(Bio$Yr==years[1])
ind2=which(Bio$Yr==years[length(years)])
Bio=Bio$Bio_all[ind:ind2]

Table_assessment=cbind(years,lowerrecr,rec,upperrecr,lowerssb,ssb,upperssb,Bio,
                       lowerf,f,upperf,
                       catches,  catch_total[,2],disc)

colnames(Table_assessment)=c("years","rec_low", "rec_value", "rec_upp", "ssb_low",
                             "ssb_val", "ssb_upp", "Bio",
                "F_low", "F_val", "F_upp","catch", "landings", "discards")

# Add 2026

Table_assessment=rbind(Table_assessment,vec)
rownames(Table_assessment)=NULL
Table_assessment<-as.data.frame(Table_assessment)

# Correct round

Table_assessment$F_low<-icesRound(Table_assessment$F_low)
Table_assessment$F_val<-icesRound(Table_assessment$F_val)
Table_assessment$F_upp<-icesRound(Table_assessment$F_upp)
nam<-colnames(Table_assessment)[-(9:11)]
Table_assessment[,c(nam)]<-round(Table_assessment[,c(nam)])
write.xlsx(Table_assessment, paste(tabledir, "/Tab10.6.xlsx", sep=""),rowNames = FALSE)


