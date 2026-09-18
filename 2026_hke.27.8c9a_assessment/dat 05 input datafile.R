#~~~~~~~~~~~~~~~~~~~~~~~~~
# Input    SS datafile   #
# Southern hake stock    #
# Sex separated settings #
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
# Marta Cousido       #
# Santiago Cerviño    #
# ~~~~~~~~~~~~~~~~~~~~~
# 31/01/2023  #
# ~~~~~~~~~~~~~

## To see the outline press: Ctrl + shift + O

## In this script we arrange all the shake available data to match SS input form
## Data is divided in: catch (and landings), indices (surveys and CPUEs) and LFDs
## Run the script sequentially as a whole (Ctrl+A) (not in separated steps)
## Input original data is "all data.RData" and contains different structures
## We read the last ss model files and update them in the model folder

## The LFD for SpSurvey and PtSurvey are sex separated
## Sex separated model changes with respect to the single sex in:
## a) Datafile: Specify Genders = 2  
## b) Datafile: LFD option Sex = 3 (page 53 Manual) and new specific structure
## For all fleets, sex=0, LFD female data (67 bins) + male data(67 0s)
## For spSurv sex separated, sex=3, LFD female(67) + LFD male(67)
## c) Controlfile: Duplicate/check growth parameter lines for male and female (page 89)

# Clean env --------------------------------------------------------------------

## Clean environment
rm(list=ls())
library(r4ss)

## Load all data.RData
load("./data/all data.RData")

# 1) read shake_data.ss--------------------------------------------------------------

start <- r4ss::SS_readstarter(file = "./boot/data/SS files template/starter.ss",
                              verbose = FALSE)
cdata <- r4ss::SS_readdat(file = "./boot/data/SS files template/shake_data.ss",
                        verbose = FALSE)

# Main settings:
cdata$Nsexes
cdata$Nsexes<-2 # combined, male and female
cdata$styr
cdata$styr<-1960 
cdata$endyr
cdata$endyr<-2025 # change the year!!!
cdata$nseas
cdata$nseas<-4 # 4 seasons or quarters
cdata$months_per_seas
cdata$months_per_seas<-c(3, 3, 3, 3)
cdata$Nsubseasons
cdata$Nsubseasons<-2
cdata$spawn_month
cdata$spawn_month<-1 # January (it's better than the real one in May)
cdata$Nages<-15 # 15 years
cdata$N_areas
cdata$N_areas<-1

# 4 Fleets:
# ~~~~~~~~~~~~~~~~~~~
# trawlers          1
# volpal            2
# artisanal         3
# cdTrw           4
# 
# 3 Surveys:
# ~~~~~~~~~~~~~~~~~~~
# SpSurv            5 * female, male, indet
# PtSurv            6 * female, male, indet
# CdSurv            7 * female
# 
# 2 CPUEs:
# ~~~~~~~~~~~~~~~~~~~

# SpCPUE trawlers
# SpCPUE volpal


cdata$Nfleets
cdata$Nfleets<-9
cdata$fleetinfo
cdata$fleetinfo<-data.frame(type =c(1,1,1,1,3,3,3,3,3), # 1 for commercial fleets 3 for surveys
                     surveytiming = c(-1,-1,-1,-1,10.3,10.3,11.3,5.5,5.5),  # 8.5 for surveys 5.5 for cpue
                     area = c(1,1,1,1,1,1,1,1,1), # single area   
                     units = c(1,1,1,1,1,1,1,1,1), # 1 = biomass (tons) for fleets, 2 = numbers (thousands of fish) ignored for surveys which are defined later  
                     need_catch_mult = c(0,0,0,0,0,0,0,0,0),
                     fleetname= c("trawlers","volpal","artisanal","cdTrw",
                                  "SpSurv","PtSurv","CdSurv","SpCPUE_trawlers","SpCPUE_volpal"))  #

cdata$CPUEinfo # CPUE and survey abundance observations
cdata$CPUEinfo<-data.frame(Fleet =c(1,2,3,4,5,6,7,8,9), 
                            Units = c(1,1,1,1,1,1,1,1,1),  # 0 = numbers, 1 = biomass, 2 = F
                            Errtype = c(0,0,0,0,4,4,4,4,4), # -1 = normal, 0=lognormal, >0 = t-student   
                            SD_Report = c(0,0,0,0,0,0,0,0,0)) # 0 = SD Report not enabled, 1 = enabled SD Report
cdata$units_of_catch
cdata$units_of_catch<-c(1,1,1,1,1,1,1,1,1) # 1 for all fleets, all in biomass 
cdata$comp_tail_compression

cdata$len_info

LenInfo=data.frame(mintailcomp =rep(-0.001,cdata$Nfleets), 
           addtocomp = rep(1e-09,cdata$Nfleets), 
           combine_M_F = c(0,0,0,0,17,17,0,0,0), # combine MF for indeterminate data bins (17 bins = from 4 cm to 20 cm)  
           CompressBins = c(11,6,11,11,11,11,11,11,6), # compress last LFD bins
           CompError=rep(0,cdata$Nfleets),
           ParmSelect=rep(0,cdata$Nfleets),
           minsamplesize=rep(0.01,cdata$Nfleets))

rownames(LenInfo)=c("trawlers","volpal","artisanal","cdTrw",
                    "SpSurv","PtSurv","CdSurv","SpCPUE_trawlers","SpCPUE_volpal")

cdata$len_info<-LenInfo

# 2) INPUT Catch ---------------------------------------------------------------

# Important: add equilibrium catch values if needed 
# Important: keep always the same fleet name and correspondent order number

library(tidyverse) 
library(plyr) 
library(dplyr)
library(conflicted)
conflict_prefer("mutate", "dplyr")


## 2.1) Commercial fleets ------------------------------------------------------

cdata$fleetinfo[ ,6]
# trawlers (1948-2024)     
# volpal (1948-2024)        
# artisanal (1994-2024)        
# cdtrw (1994-2024)          

final<-list() # final list


### trawlers ------------------------------------------------------------------------

flt<-"trawlers"# SELECT THE FLEET
fltn<-1 # Set the fleet number

## 1948-1981
c_hist<-c_hist_trawlers
length(c_hist$fleet)-48
c_hist$catch_se<-c(rep(0.2,48), rep(0.1,88))
c_hist=c_hist[,-3] # delete area column

## 1982-1993
#c_o_spTrw
#c_o_ptTrw
#c_o_sum <- c_o_spTrw %>% mutate(amount= amount + c_o_ptTrw$amount)  # sum fleets
c_o_sum<-c_seas_1982_1993_trawlers
c_o_sum$catch_se<-rep(0.075)
c_o_sum=c_o_sum[,-3] # delete area column

## 1994-2025
c_pairTrw
c_baka
c_ptTrw
c_sum <- c_pairTrw %>% mutate(amount= amount + c_baka$amount + c_ptTrw$amount)  # sum fleets
c_sum$amount<-(c_sum$amount/1000) # same units
c_sum$catch_se<-rep(0.05)
c_sum=c_sum[,-3] # delete area column

## Equilibrium catch values for (mean of previous 10 years, starting in 1960)
ind=which(c_hist$year<1960 & c_hist$year>=1950)
aux=c_hist[ind,]
conflict_prefer("summarize", "plyr")
c_eq<-plyr::ddply(aux, .(step), summarize,  sum.y=mean(amount))
eq_catch<-c_eq$sum.y

(eq_fleet<-data.frame(year =rep(-999,cdata$nseas), 
           step = (1:cdata$nseas),  # 0 = numbers, 1 = biomass, 2 = F
           fleet = rep(fltn,cdata$nseas), # -1 = normal, 0=lognormal, >0 = t-student   
           amount = eq_catch,
           catch_se=rep(0.2,cdata$nseas)))

## CUT time series historical >=1960
ind=which(c_hist$year<1960)
c_hist<-c_hist[-ind,]

## Reshape data:
fleet<-rbind(eq_fleet,c_hist,c_o_sum,c_sum)
fleet$fleet<-rep(fltn, length(fleet$year))
colnames(fleet)<-c("year", "seas", "fleet", "catch", "catch_se") # Change names

head(fleet,8);tail(fleet,4) #final check
final[[fltn]]=fleet # include in final


### volpal --------------------------------------------------------------------------

flt<-"volpal"# fleet name
fltn<-2 # fleet number

## 1948-1981
c_hist<-c_hist_volpal
length(c_hist$fleet)-48
c_hist$catch_se<-c(rep(0.2,48), rep(0.1,88))
c_hist=c_hist[,-3] # delete area column

## 1982-1993
#c_o_vol
#c_o_pal
#c_o_sum <- c_o_vol %>% mutate(amount= amount + c_o_pal$amount)  # sum fleets
c_o_sum<-c_seas_1982_1993_volpal
c_o_sum$catch_se<-rep(0.075)
c_o_sum=c_o_sum[,-3] # delete area column

## 1994-2025
c_volanta
c_palangre
c_sum <- c_volanta %>% mutate(amount= amount + c_palangre$amount)  # sum fleets
c_sum$amount<-(c_sum$amount/1000) # same units
c_sum$catch_se<-rep(0.05)
c_sum=c_sum[,-3] # delete area column

## Equilibrium catch values for (mean of previous 10 years, starting in 1960)
ind=which(c_hist$year<1960 & c_hist$year>=1950)
aux=c_hist[ind,]
c_eq<-plyr::ddply(aux, .(step), summarize,  sum.y=mean(amount))
eq_catch<-c_eq$sum.y

(eq_fleet<-data.frame(year =rep(-999,cdata$nseas), 
                      step = (1:cdata$nseas),  # 0 = numbers, 1 = biomass, 2 = F
                      fleet = rep(fltn,cdata$nseas), # -1 = normal, 0=lognormal, >0 = t-student   
                      amount = eq_catch,
                      catch_se=rep(0.2,cdata$nseas)))

## CUT time series historical >=1960
ind=which(c_hist$year<1960)
c_hist<-c_hist[-ind,]

## Reshape data:
fleet<-rbind(eq_fleet,c_hist,c_o_sum,c_sum)
fleet$fleet<-rep(fltn, length(fleet$year))
colnames(fleet)<-c("year", "seas", "fleet", "catch", "catch_se") # Change names

head(fleet,6);tail(fleet,4) #final check
final[[fltn]]=fleet # include in final

### art -----------------------------------------------------------------------------

flt<-"art"# SELECT THE FLEET
fltn<-3 # Set the fleet number

## 1982-1993
c_o_sum<-c_seas_1982_1993_art
c_o_sum$catch_se<-rep(0.075)

## 1994-2025
c_Art
c_ptArt
c_sum <- c_Art %>% mutate(amount= amount + c_ptArt$amount)  # sum fleets
c_sum$amount<-(c_sum$amount/1000) # same units
c_sum$catch_se<-rep(0.05)

# Reshape data:
fleet<-rbind(c_o_sum,c_sum)
fleet$fleet<-rep(fltn, length(fleet$year))
fleet<-fleet[,-(3)]
colnames(fleet)<-c("year", "seas", "fleet", "catch", "catch_se") # Change names

head(fleet,4);tail(fleet,4) #final check
final[[fltn]]=fleet # include in final

### cdTrw ---------------------------------------------------------------------------

flt<-"cdTrw"# SELECT THE FLEET
fltn<-4 # Set the fleet number

## 1982-1993
c_o_sum<-c_seas_1982_1993_cdTrw
c_o_sum$catch_se<-rep(0.075)

## 1994-2025
c_cdTrw
c_sum <- c_cdTrw
c_sum$amount<-(c_sum$amount/1000) # same units
c_sum$catch_se<-rep(0.05)

# Reshape data:
fleet<-rbind(c_o_sum,c_sum)
fleet$fleet<-rep(fltn, length(fleet$year))
fleet<-fleet[,-(3)]
colnames(fleet)<-c("year", "seas", "fleet", "catch", "catch_se") # Change names

head(fleet,4);tail(fleet,4) # final check
final[[fltn]]=fleet # include in final


# Final bind:
catch_input<-rbind(final[[1]],final[[2]], final[[3]], final[[4]])
head(catch_input,8);tail(catch_input,4) # final check

# Compare SS new catch data with previous data:
catch_input$year<-as.numeric(catch_input$year)
catch_input$seas<-as.numeric(catch_input$seas)
head(cdata$catch,8)
head(catch_input,8)
summary(cdata$catch)
summary(catch_input)

### Update SS datafile:
cdata$catch<-catch_input

## 2.2) Surveys ---------------------------------------------------------------------

cdata$fleetinfo[ ,c(2,6)]

# ~~~~~~~~~~~~~~~~~~~
# SpSurv            5 * female, male, indet
# PtSurv            6 * female, male, indet
# CdSurv            7 * female
# ~~~~~~~~~~~~~~~~~~~

head(cdata$CPUE,4)# CPUE/SURVEY data | year, season (month), index, obs, se_log
cdata$CPUEinfo #_Units: 0=numbers; 1=biomass; 2=F; ...etc.| #_Errtype:  -1=normal; 0=lognormal; >0=T-student

# Reshape data:
# Units, page 47 manual
tail(ind_SpGFS) 
tail(ind_PtGFS) 
tail(ind_CdSurv) 
tail(ind_SpCPUE_trawlers) 
tail(ind_SpCPUE_volpal) 

Indjoinbio<-rbind(ind_SpGFS,ind_PtGFS,ind_CdSurv,ind_SpCPUE_trawlers,ind_SpCPUE_volpal)
unique(Indjoinbio$index)
Indjoin<-mutate(Indjoinbio, index = revalue(index, c( "SpCPUE_trawlers" = 8,
                                                      "SpCPUE_volpal" = 9,
                                                      "SpGFS" = 5,
                                                      "PtGFS" = 6,
                                                      "bioCdSurv" = 7))) 

# Set SE as CV:
Indjoin$CV<-Indjoin$se/Indjoin$obs # CV; standarderror of the observation divided by the mean value of the observation.


# Associate season to each survey (8.5) or cpue (5.5):
Indjoin<-mutate(Indjoin, seas = revalue(index, c( "8" = "5.5", # SpCPUE_trawlers = seas
                                                  "9" = "5.5", # SpCPUE_volpal = seas
                                                  "5" = "10.3", # SpGFS = seas
                                                  "6" = "10.3", # PtGFS
                                                  "7" = "11.3" # CdSurv = seas
                                                  )))

Ind<-Indjoin[,c(1,2,3,4,6)]# Reorder: year, seas, index, obs, se_log
Ind$obs<-(Ind$obs) # Change units

# Check with SS data:
class(Ind$year)
class(Ind$seas)
class(Ind$index)
class(Ind$obs)
class(Ind$CV)
Ind$year<-as.numeric(Ind$year)
Ind$seas<-as.numeric(Ind$seas)
Ind$index<-as.numeric(as.character(Ind$index))

table(Ind$year)
Ind<-Ind[complete.cases(Ind), ] # REMOVE POSSIBLE NA's 
head(Ind,4); tail(Ind,4) # input data
head(cdata$CPUE,4)# SS: Year, season obs se_log
summary(Ind)# input data 
summary(cdata$CPUE) # SS data

### Update SS datafile:

cdata$CPUE<-Ind 

## 2.3) Discards --------------------------------------------------------------------

# Discard = fleet 1 = trawlers
head(cdata$discard_data,4) # SS Discard data | Yr Seas Flt  Discard Std_in
cdata$discard_fleet_info  #units 1 (1=same_as_catchunits(bio/num) #_discard_errtype:  >0 for DF of T-dist(read CV below) -2 for lognormal; -3 for trunc normal with CV
cdata$discard_fleet_info$Fleet<-1 #trawlers

# zero discards for period prev to 1991?

# Read initial data:

flt<-"disc"# SELECT THE FLEET
fltn<-1
fleet<-c_disc
head(fleet,4);tail(fleet,4) # init data | year, step, area, fleet, amount

# Reshape data to get |Yr Seas Flt  Discard Std_in:
fleet$Flt<-rep(fltn,times=length(fleet$year)) # Fleet 3 equal to trawlers
fleet$amount<-as.numeric(fleet$amount) ### Change units
fleet$amount<-(fleet$amount/1000) # to TONS
fleet<-fleet[,c(1,2,6,5)] # select desired vars
fleet$Std_in<-rep(0.2, times=length(fleet$year)) # In SS data, 0.4
colnames(fleet)<-c("Yr", "Seas", "Flt", "Discard", "Std_in") # Set same names than SS
library(plyr)
fleet$Seas<-as.factor(fleet$Seas)
fleet<-mutate(fleet, Seas = mapvalues(Seas, from=c("1","2","3","4"),
                                      to=c("2.5","5.5","8.5","11.5"))) ### SEASON in SS is NOT 1,2,3,4, but 2.5, 5.5, 8.5, 11.5
# Final check:
fleet$Seas<-as.numeric(as.character(fleet$Seas))
fleet$Discard
summary(cdata$discard_data)
summary(fleet)

### Update SS datafile:
cdata$discard_data<-fleet

# 3) INPUT LFDs  --------------------------------------------------------------------

# We have the historical fleet (1952-1981), all fleets are combined (no LFD)
# We have the land fleet (1982-1993), all fleets are combined (with LFD)
# The LFD of historical fleet (historical + land) is the LFD from land
# We have separated fleets (1994-2019) (with LFDs)
# LFD data is from 1 cm to 1 cm in all initial files
# We transform LFD data to SS sex separated format
# We make combined fleets
# spSurv fleet is sex separated
# Finally we will upload the new information to SS input data file
  
library(dplyr)
library(plyr) 
library(data.table)
library(reshape2) # from long to wide format

cdata$lbin_vector_pop # in population  
cdata$N_lbinspop
cdata$lbin_vector # in sampled data
cdata$N_lbins # in sampled data
final<-list() # final list

# trawlers  --------------------------------------------------------------------------

f1<-l_pairTrw
f2<-l_baka
f3<-l_ptTrw
flt<-"trawlers"# name for combined fleet
fltn<-1

# Reshape:
f1$number<-as.numeric(f1$number)
f2$number<-as.numeric(f2$number)
f3$number<-as.numeric(f3$number)
fleet<-f1 
sum <- fleet %>% mutate(sumrow= number + f2$number + f3$number) # same length both data frames
fleet$number<-sum$sumrow # sum number of both fleets
fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
                                      to=c("2.5","5.5","8.5","11.5")))
fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number/1000# change units
tail(fleet,4)


# # Check that all entries are 0 below 3cm, in other case solve it!
# sum(subset(fleet,fleet$len==1)$number)
# 
# sum(subset(fleet,fleet$len==2)$number)
# 
# sum(subset(fleet,fleet$len==3)$number)
# 
ind<-which(fleet$len==3 & fleet$number!=0)
fleet[ind,]
fleet[ind+1,]
fleet[ind+1,5]<-fleet[ind,5]+fleet[ind+1,5]

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"), timevar="bins")
length(fleet$Yr) 



#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) 
fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(2,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                    OJO !!!!!!
ns<-subset(ns_trawlers, ns_trawlers$Year>1993)
length(fleet$Yr)
length(ns$Year)
# Correct order!
new_order <- c(4, 1, 2, 3)
ns <- ns %>%
  arrange(Year, match(Season, new_order))

fleet$Nsamp<-ns$oMean
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder

# Sex settings:
mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = 67)    # Create zero-matrix
fleet<-cbind(fleet, mat_zero)
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
tail(fleet,4)

fleet2<-fleet


# discards --------------------------------------------------------------------------

f1<-l_disc
flt<-"disc"# name for combined fleet
fltn<-1

# Reshape:
f1$number<-as.numeric(f1$number)
fleet<-f1 
fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"), to=c("2.5","5.5","8.5","11.5")))
fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number/1000# change units
tail(fleet,4)


# # Check that all entries are 0 below 3cm, in other case solve it!
# sum(subset(fleet,fleet$len==1)$number)
# 
# sum(subset(fleet,fleet$len==2)$number)
# 
# sum(subset(fleet,fleet$len==3)$number)
# 
ind<-which(fleet$len==3 & fleet$number!=0)
fleet[ind,]
fleet[ind+1,]
fleet[ind+1,5]<-fleet[ind,5]+fleet[ind+1,5]


# Bins:
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS, 1 for land         
fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(1,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed 
ns<-subset(ns_disc, ns_disc$Year>1993)
ns<-ns[ns$Year %in% c(as.character(unique(fleet$Yr))), ]
unique(fleet$Yr)
length(fleet$Yr)
length(ns$Year)
# Correct order!
new_order <- c(4, 1, 2, 3)
ns <- ns %>%
  arrange(Year, match(Season, new_order))

fleet$Nsamp<-ns$oMean
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder

# Sex settings:
mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = 67)    # Create zero-matrix
fleet<-cbind(fleet, mat_zero)
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
head(fleet)

fleet<-rbind(fleet2,fleet) # join trawlers + discards
final[[fltn]]=fleet # include both, trawlers and discards in final


# volpal  ----------------------------------------------------------------------------

f1<-l_volanta
f2<-l_palangre
flt<-"volpal"# name for combined fleet
fltn<-2

# Reshape:
f1$number<-as.numeric(f1$number)
f2$number<-as.numeric(f2$number)
fleet<-f1 
sum <- fleet %>% mutate(sumrow= number + f2$number) # same length both data frames
fleet$number<-sum$sumrow # sum number of both fleets
fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
                                      to=c("2.5","5.5","8.5","11.5")))
fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number/1000# change units
tail(fleet,4)
# 
# # Check that all entries are 0 below 3cm, in other case solve it!
# sum(subset(fleet,fleet$len==1)$number)
# 
# sum(subset(fleet,fleet$len==2)$number)
# 
# sum(subset(fleet,fleet$len==3)$number)
# 

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS, 1 for land             OJO!!!!!
fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                           OJO !!!!!!
ns<-subset(ns_volpal, ns_volpal$Year>1993)
length(fleet$Yr)
length(ns$Year)
# Correct order!
new_order <- c(4, 1, 2, 3)
ns <- ns %>%
  arrange(Year, match(Season, new_order))
fleet$Nsamp<-ns$oMean
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder

# Sex settings:
mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = 67)    # Create zero-matrix
fleet<-cbind(fleet, mat_zero)
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
tail(fleet,4)

final[[fltn]]=fleet # include in final

#  art  ------------------------------------------------------------------------------

f1<-l_Art
f2<-l_ptArt
flt<-"art"# name for combined fleet
fltn<-3

# Reshape:
f1$number<-as.numeric(f1$number)
f2$number<-as.numeric(f2$number)
fleet<-f1 
sum <- fleet %>% mutate(sumrow= number + f2$number) # same length both data frames
fleet$number<-sum$sumrow # sum number of both fleets
fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
                                      to=c("2.5","5.5","8.5","11.5")))
fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number/1000# change units
tail(fleet,4)

# # Check that all entries are 0 below 3cm, in other case solve it!
# sum(subset(fleet,fleet$len==1)$number)
# 
# sum(subset(fleet,fleet$len==2)$number)
# 
# sum(subset(fleet,fleet$len==3)$number)


# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS, 1 for land             OJO!!!!!
fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                           OJO !!!!!!
ns<-subset(ns_artisanal, ns_artisanal$Year>1993)
length(fleet$Yr)
length(ns$Year)
# Correct order!
new_order <- c(4, 1, 2, 3)
ns <- ns %>%
  arrange(Year, match(Season, new_order))
fleet$Nsamp<-ns$oMean
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder

# Sex settings:
mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = 67)    # Create zero-matrix
fleet<-cbind(fleet, mat_zero)
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
tail(fleet,4)

final[[fltn]]=fleet # include in final

# cdTrw -----------------------------------------------------------------------------

f1<-l_cdTrw
flt<-"cdTrw"# name for combined fleet
fltn<-4

# Reshape:
f1$number<-as.numeric(f1$number)
fleet<-f1 
#sum <- fleet %>% mutate(sumrow= number + f2$number) # same length both data frames
#fleet$number<-sum$sumrow # sum number of both fleets
fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
                                      to=c("2.5","5.5","8.5","11.5")))
fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number/1000# change units
tail(fleet,4)

# # Check that all entries are 0 below 3cm, in other case solve it!
# sum(subset(fleet,fleet$len==1)$number)
# 
# sum(subset(fleet,fleet$len==2)$number)
# 
# sum(subset(fleet,fleet$len==3)$number)


# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS, 1 for land             OJO!!!!!
fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                           OJO !!!!!!
ns<-subset(ns_cdTrw, ns_cdTrw$Year>1993)
length(fleet$Yr)
length(ns$Year)
table(ns$Year)
# Correct the problem of missing nsample 
# 2019
subset(ns,ns$Year==2019)
ind<-which(ns$Year==2019 & ns$Season==3)
aux<-ns[ind,]
aux[,2]<-4
aux[,3]<-5
ns<-rbind(ns[1:ind,],aux,ns[(ind+1):(dim(ns)[1]),])

# 2020

subset(ns,ns$Year==2020)
ind<-which(ns$Year==2020 & ns$Season==3)
aux<-ns[ind,]
aux[,2]<-1
aux[,3]<-5
aux2<-ns[ind,]
aux2[,2]<-2
aux2[,3]<-5

ns<-rbind(ns[1:(ind-1),],aux,aux2,ns[(ind):(dim(ns)[1]),])

# Correct order!
new_order <- c(4, 1, 2, 3)
ns <- ns %>%
  arrange(Year, match(Season, new_order))

fleet$Nsamp<-  ns$oMean        #c(ns$oMean,5,5,5)# 2020 missing 
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder

# Sex settings:
mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = 67)    # Create zero-matrix
fleet<-cbind(fleet, mat_zero)
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
head(fleet,4)

# SP --------------------------------------------------------------------

## From dat 03.R: proportion for NSample in Super-periods
#Sdat[,2]/(max(Sdat[,2])/2)
vec=c(fleet$Nsamp[1], 1, 1, 1)# relative weights are 1 for all seasons. First number is the NSample.
vec=round(vec,2)

# Correction, years with 4 equal values, (1994-2000) sum and structure in Super-Periods
aux=fleet[1:24,]
year_aux=1994:1999
index=seq(1,4*length(year_aux),by=4)
i=1
for(i in index){
ind=which(index==i)

# seas 1
aux[i,]=fleet[ind,]
aux[i,]$Seas=-as.numeric(aux[i,]$Seas)
aux[i,]$Nsamp=vec[1]
aux[i,]$Yr=year_aux[ind]

# seas 2
aux[i+1,]$FltSvy=-aux[i+1,]$FltSvy
aux[i+1,]$Nsamp=vec[2]
aux[i+1,]$Seas=5.5
aux[i+1,]$Yr=year_aux[ind]

di=dim(aux)[2]
aux[i+1,7:di]=fleet[ind,-(1:6)]

# seas 3
aux[i+2,]$FltSvy=-aux[i+2,]$FltSvy
aux[i+2,]$Nsamp=vec[3]
aux[i+2,]$Seas=8.5
aux[i+2,]$Yr=year_aux[ind]

di=dim(aux)[2]
aux[i+2,7:di]=fleet[ind,-(1:6)]

# seas 4

aux[i+3,]$FltSvy=-aux[i+3,]$FltSvy
aux[i+3,]$Nsamp=vec[4]
aux[i+3,]$Seas=-11.5
aux[i+3,]$Yr=year_aux[ind]

di=dim(aux)[2]
aux[i+3,7:di]=fleet[ind,-(1:6)]
}

head(aux)
head(fleet)
fleet=rbind(aux,fleet[-(1:6),])
final[[fltn]]=fleet # include in final

# SpSurvey* --------------------------------------------------------------------------

# For all fleets, sex=0, LFD female data (67 bins) + male data(67 0s)
# For spSurv sex separated, sex=3, LFD female(67) + LFD male(67)
# Indeterminate data must go as females and apply COMPRESS BINS in datafile
# For shake the number of bins to compress is 20 cm
# We must combine indeterminate + female data from bin of 4cm to bin of 20 cm (16 uds)
# So we must set a value of 16 in COMPRESS BINS Combine MF
# After the indet + female (67 bins) vector we put the male (67 bins) vector

## indet ----------------------------------------------------------------

fleet<-l_SpGFS_indet
flt<-"SpSurv"# name for combined fleet
fltn<-5
tail(fleet,4)
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-fleet[,c(1,2,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number #/1000# change units


sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?
# Care sum l3+l4 
len=unique(fleet$Yr)


for (i in 1:length(len)){
ind2=which(fleet$Yr==len[i]&fleet$len==2)
ind=which(fleet$Yr==len[i]&fleet$len==3)
ind1=which(fleet$Yr==len[i]&fleet$len==4)
if(length(ind2)>0){
fleet[ind1,]$number=fleet[ind1,]$number+fleet[ind,]$number+fleet[ind2,]$number} else {
  fleet[ind1,]$number=fleet[ind1,]$number+fleet[ind,]$number
}
}

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS3, 1 for land         
fleet$Gender<-rep(3,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
ns<-ns_spsurv[-5,] # remove NA
length(fleet$Yr)
length(ns$Year)

fleet$Nsamp<-ns$NSamp
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam)
head(fleet,1)

fleet_indet<-fleet[,c(7:23)]# 16 bins from 4 cm to 20 cm

## females ------------------------------------------------------------------

fleet<-l_SpGFS_fem
flt<-"SpSurv"# name for combined fleet
fltn<-5
tail(fleet,4)
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-fleet[,c(1,2,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number#/1000# change units

sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS3, 1 for land         
fleet$Gender<-rep(3,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
ns<-ns_spsurv[-5,] # remove NA
length(fleet$Yr)
length(ns$Year)
fleet$Nsamp<-ns$NSamp
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam)
head(fleet,1)
dim(fleet)
fleet_f<-cbind(fleet[,c(1:6)],fleet_indet+fleet[,c(7:23)],fleet[,c(24:73)]) # combine indet + female
dim(fleet_f)

## males ------------------------------------------------------------------

fleet<-l_SpGFS_mal
flt<-"SpSurv"# name for combined fleet
fltn<-5
tail(fleet,4)
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-fleet[,c(1,2,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number #/1000# change units

sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS3, 1 for land         
fleet$Gender<-rep(3,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
ns<-ns_spsurv[-5,] # remove NA
length(fleet$Yr)
length(ns$Year)
fleet$Nsamp<-ns$NSamp
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam)
head(fleet,1)
dim(fleet)

## Join:
dim(fleet)

fleet_m<-fleet
fleet_f[,c(7:23)]=fleet_f[,c(7:23)]+fleet_m[,c(7:23)]
fleet_m[,c(7:23)][fleet_m[,c(7:23)] > 0] <- 0 # 0 for males in indet bins
fleet_sex<-cbind(fleet_f, fleet_m[,7:73])
final[[fltn]]=fleet_sex # include in final

# PtSurvey *  --------------------------------------------------------------------------

# For all fleets, sex=0, LFD female data (67 bins) + male data(67 0s)
# For spSurv sex separated, sex=3, LFD female(67) + LFD male(67)
# Indeterminate data must go as females and apply COMPRESS BINS in datafile
# For shake the number of bins to compress is 20 cm
# We must combine indeterminate + female data from bin of 4cm to bin of 20 cm (16 uds)
# So we must set a value of 16 in COMPRESS BINS Combine MF
# After the indet + female (67 bins) vector we put the male (67 bins) vector

## indet ----------------------------------------------------------------

fleet<-l_PtGFS_indet
flt<-"PtSurv"# name for combined fleet
fltn<-6
tail(fleet,4)
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-fleet[,c(1,2,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number#/1000# change units
fleet$Seas<-rep(10.3)

sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?

# Care sum l3+l4 
len=unique(fleet$Yr)


for (i in 1:length(len)){
  ind=which(fleet$Yr==len[i]&fleet$len==3)
  ind1=which(fleet$Yr==len[i]&fleet$len==4)
  
  fleet[ind1,]$number=fleet[ind1,]$number+fleet[ind,]$number
}

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS3, 1 for land         
fleet$Gender<-rep(3,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
ns<-na.omit(ns_ptsurv[-c(1,dim(ns_ptsurv)[1]),]) # Delete 2025! Take care next year!
length(fleet$Yr)
length(ns$NSamp)
fleet$Nsamp<-ns$NSamp
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam)
head(fleet,1)

fleet_indet<-fleet[,c(7:23)]# 16 bins from 4 cm to 20 cm

## females ------------------------------------------------------------------

fleet<-l_PtGFS_fem
flt<-"PtSurv"# name for combined fleet
fltn<-6
tail(fleet,4)
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-fleet[,c(1,2,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number#/1000# change units
fleet$Seas<-rep(10.3)

sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS3, 1 for land         
fleet$Gender<-rep(3,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
ns<-na.omit(ns_ptsurv[-c(1,dim(ns_ptsurv)[1]),])
length(fleet$Yr)
length(ns$NSamp)
fleet$Nsamp<-ns$NSamp
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam)
head(fleet,1)
dim(fleet)
fleet_f<-cbind(fleet[,c(1:6)],fleet_indet+fleet[,c(7:23)],fleet[,c(24:73)]) # combine indet + female
dim(fleet_f)

## males -----------------------------------------------------------------------

fleet<-l_PtGFS_mal
flt<-"PtSurv"# name for combined fleet
fltn<-6
tail(fleet,4)
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet<-fleet[,c(1,2,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number#/1000# change units
fleet$Seas<-rep(10.3)
sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS3, 1 for land         
fleet$Gender<-rep(3,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
ns<-na.omit(ns_ptsurv[-c(1,dim(ns_ptsurv)[1]),])
length(fleet$Yr)
length(ns$NSamp)
fleet$Nsamp<-ns$NSamp
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam)
head(fleet,1)
dim(fleet)

## Join:
dim(fleet)

fleet_m<-fleet
fleet_f[,c(7:23)]=fleet_f[,c(7:23)]+fleet_m[,c(7:23)]
fleet_m[,c(7:23)][fleet_m[,c(7:23)] > 0] <- 0 # 0 for males in indet bins
fleet_sex<-cbind(fleet_f, fleet_m[,7:73])
final[[fltn]]=fleet_sex # include in final

# CdSurvey --------------------------------------------------------------------------

f1<-l_CdSurv
flt<-"CdSurv"# name for combined fleet
fltn<-7

# Reshape:
f1$number<-as.numeric(f1$number)
fleet<-f1 
fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
fleet$step<-rep(4, times=length(fleet$step))
fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
                                      to=c("2.5","5.5","8.5","11.3")))
fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
fleet$number<-fleet$number#/1000# change units
tail(fleet,4)
sum(subset(fleet,fleet$len==1)$number) # all zeros?
sum(subset(fleet,fleet$len==2)$number) # all zeros?
sum(subset(fleet,fleet$len==3)$number) # all zeros?

len=unique(fleet$Yr)


for (i in 1:length(len)){
  ind=which(fleet$Yr==len[i]&fleet$len==3)
  ind1=which(fleet$Yr==len[i]&fleet$len==4)
  
  fleet[ind1,]$number=fleet[ind1,]$number+fleet[ind,]$number
}

# Bins: 
NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=100, by=2),170) # Desired bins (SS) 67
nam<-as.character(paste("l",NLbins, sep="")) # name for column
nam<-nam[-68] # remove l170 group
setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
fleet<-fleet[complete.cases(fleet), ] # remove NAS
fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"),
               timevar="bins")
length(fleet$Yr) # 104 rows = 1 seasons * 12 years

#SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
fleet$FltSvy<-rep(fltn,length(fleet$Yr)) # CHANGE from name to number in SS, 1 for land         
fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                          
#ns<-ns_cdSurv
fleet$Nsamp<-rep(70)
fleet<-fleet[,c(2,3,1,71,72,73,4:70)] # reorder

# Sex settings:
mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = 67)    # Create zero-matrix
fleet<-cbind(fleet, mat_zero)
colnames(fleet)<-c("Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
head(fleet,1)
fleet[is.na(fleet)] <- 0
final[[fltn]]=fleet # include in final


# save lencomp -----------------------------------------------------------------

## Input LFDs
## Check dims:
# Delete 2020 LFDs discard
aux=final[[1]]
ind<-which(aux$Yr==2020 &aux$Part==1)
aux<-aux[-ind,]
dim(aux) #trawlers + discards
final[[1]]<-aux
# Delete 2005 
aux=final[[2]]
ind<-which(aux$Yr==2005)
aux<-aux[-ind,]
dim(aux) #volpal
final[[2]]<-aux
dim(final[[3]]) # art 
dim(final[[4]]) # cdTrw
dim(final[[5]]) # SpSurvey has different dims (Indet, Female, Male)
dim(final[[6]]) # PtSurvey
dim(final[[7]]) # cdSurvey

LENCOMP<-rbind(final[[1]],final[[2]], final[[3]], final[[4]],final[[5]],
               final[[6]],final[[7]])

LENCOMP$Yr<-as.numeric(LENCOMP$Yr)
LENCOMP$Seas<-as.numeric(LENCOMP$Seas)
summary(LENCOMP)
cdata$lencomp<-LENCOMP

# 80+ sizefreq --------------------------------------------------------------------------
# 
# cdata$N_agebins # 0
# cdata$use_MeanSize_at_Age_obs # 0
# cdata$N_environ_variables # 0
# cdata$N_sizefreq_methods # 1
# cdata$nbins_per_method # number of bins for this 80 + group = 57
# cdata$scale_per_method # 3
# cdata$mincomp_per_method #1e-09
# cdata$sizefreq_data_list
# 
# freqbin<-list() # final list
# 
# 
# # trawlers   -----------------------------------------------------------------
# 
# f1<-l_o_spTrw
# colnames(f1)<-c("year","step","area","length", "age","number","fleet")
# f2<-l_o_ptTrw
# colnames(f2)<-c("year","step","area","length", "age","number","fleet")
# 
# flt<-"trawlers"# name for combined fleet
# fltn<-1
# 
# # Reshape:
# f1$number<-as.numeric(f1$number)
# f2$number<-as.numeric(f2$number)
# fleet<-f1 
# sum <- fleet %>% mutate(sumrow= number + f2$number ) # same length both data frames
# fleet$number<-sum$sumrow # sum number of both fleets
# fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
# fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
# fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
# fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
#                                       to=c("2.5","5.5","8.5","11.5")))
# fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
# colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
# fleet$number<-fleet$number/1000# change units
# #fleet<-subset(fleet, fleet$len<81)
# tail(fleet,4)
# # sum(subset(fleet,fleet$len==1)$number) # all zeros?
# # sum(subset(fleet,fleet$len==2)$number) # all zeros?
# # sum(subset(fleet,fleet$len==3)$number) # all zeros?
# # Bins: 
# NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=80, by=2),170) # Desired bins (SS) 67
# nam<-as.character(paste("l",NLbins, sep="")) # name for column
# nam<-nam[-58] # remove l170 group
# setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
# fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
# fleet<-fleet[complete.cases(fleet), ] # remove NAS
# fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"), timevar="bins")
# length(fleet$Yr) # 104 rows = 1 seasons * 12 years
# 
# #SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
# fleet$FltSvy<-rep(fltn,length(fleet$Yr)) 
# fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
# fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                    OJO !!!!!!
# ns<-subset(ns_trawlers, ns_trawlers$Year<1994)
# length(fleet$Yr)
# length(ns$Year)
# fleet$Nsamp<-ns$oMean
# fleet$method<-rep(1)
# fleet<-fleet[,c(64,2,3,1,61,62,63,4:60)] # reorder
# 
# # Sex settings:
# mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = length(nam))    # Create zero-matrix
# fleet<-cbind(fleet, mat_zero)
# colnames(fleet)<-c("Method","Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
# head(fleet,4)
# 
# 
# # # SP ---------------------------------------------------------------------------
# # 
# # 
# # vec=c(fleet$Nsamp[1], 1, 1, 1)# relative weights are 1 for all seasons. First number is the 
# # vec=round(vec,2)
# # 
# # # Correction, years with 4 equal values, (1994-2000) sum and structure in Super-Periods
# # aux=rbind(fleet,fleet,fleet,fleet)
# # year_aux=1982:1993
# # index=seq(1,length(year_aux)*4,by=4)
# # for(i in index){
# #   ind=which(index==i)
# #   
# #   # seas 1
# #   aux[i,]=fleet[ind,]
# #   aux[i,]$Seas=-as.numeric(aux[i,]$Seas)
# #   aux[i,]$Nsamp=vec[1]
# #   aux[i,]$Yr=year_aux[ind]
# #   
# #   # seas 2
# #   aux[i+1,]$FltSvy=-aux[i+1,]$FltSvy
# #   aux[i+1,]$Nsamp=vec[2]
# #   aux[i+1,]$Seas=5.5
# #   aux[i+1,]$Yr=year_aux[ind]
# #   
# #   di=dim(aux)[2]
# #   aux[i+1,8:di]=fleet[ind,-(1:7)]
# # 
# #   # seas 3
# #   aux[i+2,]$FltSvy=-aux[i+2,]$FltSvy
# #   aux[i+2,]$Nsamp=vec[3]
# #   aux[i+2,]$Seas=8.5
# #   aux[i+2,]$Yr=year_aux[ind]
# # 
# #   di=dim(aux)[2]
# #   aux[i+2,8:di]=fleet[ind,-(1:7)]
# # 
# #   # seas 4
# #   
# #   aux[i+3,]$FltSvy=-aux[i+3,]$FltSvy
# #   aux[i+3,]$Nsamp=vec[4]
# #   aux[i+3,]$Seas=-11.5
# #   aux[i+3,]$Yr=year_aux[ind]
# #   
# #   di=dim(aux)[2]
# #   aux[i+3,8:di]=fleet[ind,-(1:7)]
# # 
# # }
# # 
# # head(aux)
# # freqbin[[fltn]]=aux # include in final
# # 
# # # volpal   ------------------------------------------------------------------
# # 
# # f1<-l_o_vol
# # colnames(f1)<-c("year","step","area","length", "age","number","fleet")
# # f2<-l_o_pal
# # colnames(f2)<-c("year","step","area","length", "age","number","fleet")
# # 
# # flt<-"volpal"# name for combined fleet
# # fltn<-2
# # 
# # # Reshape:
# # f1$number<-as.numeric(f1$number)
# # f2$number<-as.numeric(f2$number)
# # fleet<-f1 
# # sum <- fleet %>% mutate(sumrow= number + f2$number ) # same length both data frames
# # fleet$number<-sum$sumrow # sum number of both fleets
# # fleet$fleet<-rep(flt, times=length(fleet$fleet)) # substitute name of new fleet
# # fleet$len_cm<-sub('len', '', fleet$age) # remove "len1" to have only "1"...
# # fleet$len_cm<-as.numeric(fleet$len_cm) # create len_cm column (1...129)
# # fleet<-mutate(fleet, Seas = mapvalues(step, from=c("1","2","3","4"),
# #                                       to=c("2.5","5.5","8.5","11.5")))
# # fleet<-fleet[,c(1,9,7,8,6)]# final data structure: Yr  Seas FltSvy len number
# # colnames(fleet)<-c("Yr","Seas","FltSvy","len","number")
# # fleet$number<-fleet$number/1000# change units
# # #fleet<-subset(fleet, fleet$len<81)
# # tail(fleet,4)
# # # sum(subset(fleet,fleet$len==1)$number) # all zeros?
# # # sum(subset(fleet,fleet$len==2)$number) # all zeros?
# # # sum(subset(fleet,fleet$len==3)$number) # all zeros?
# # # Bins: 
# # NLbins<-c(seq(from=4, to=40, by=1)[1:36], seq(from=40, to=80, by=2),170) # Desired bins (SS) 67
# # nam<-as.character(paste("l",NLbins, sep="")) # name for column
# # nam<-nam[-58] # remove l170 group
# # setDT(fleet)[ , bins := cut(len, breaks = NLbins, right = FALSE, labels = nam)]# Set dframe bins
# # fleet<-ddply(fleet, .(FltSvy,Yr,Seas,bins), summarize,  num=sum(number)) # mean at each bin level
# # fleet<-fleet[complete.cases(fleet), ] # remove NAS
# # fleet<-reshape(fleet, direction="wide", idvar=c("FltSvy","Yr", "Seas"), timevar="bins")
# # length(fleet$Yr) # 104 rows = 1 seasons * 12 years
# # 
# # #SS structure: Yr  Seas FltSvy Gender Part   Nsamp l4 l5 l6 l7 l8 l9 ... l100
# # fleet$FltSvy<-rep(fltn,length(fleet$Yr)) 
# # fleet$Gender<-rep(0,length(fleet$Yr)) # sex 0=combined, 1=female, 2=male
# # fleet$Part<-rep(0,length(fleet$Yr)) # CHANGE 1=discards, 2=retained, 0=mixed                    OJO !!!!!!
# # ns<-subset(ns_volpal, ns_volpal$Year<1994)
# # length(fleet$Yr)
# # length(ns$Year)
# # fleet$Nsamp<-ns$oMean
# # fleet$method<-rep(1)
# # fleet<-fleet[,c(64,2,3,1,61,62,63,4:60)] # reorder
# # 
# # # Sex settings:
# # mat_zero <- matrix(0, nrow = length(fleet[,1]), ncol = length(nam))    # Create zero-matrix
# # fleet<-cbind(fleet, mat_zero)
# # colnames(fleet)<-c("Method","Yr","Seas","FltSvy","Gender","Part","Nsamp",nam, nam)
# # tail(fleet,4)
# # 
# # 
# # # SP ---------------------------------------------------------------------------
# # 
# # ## From dat 03.R: proportion for NSample in Super-periods
# # #Sdat[,2]/(max(Sdat[,2])/2)
# # vec=c(fleet$Nsamp[1], 1, 1, 1)# relative weights are 1 for all seasons. First number is the NSample.
# # vec=round(vec,2)
# # 
# # # Correction, years with 4 equal values, (1994-2000) sum and structure in Super-Periods
# # aux=rbind(fleet,fleet,fleet,fleet)
# # year_aux=1982:1993
# # index=seq(1,length(year_aux)*4,by=4)
# # for(i in index){
# #   ind=which(index==i)
# #   
# #   # seas 1
# #   aux[i,]=fleet[ind,]
# #   aux[i,]$Seas=-as.numeric(aux[i,]$Seas)
# #   aux[i,]$Nsamp=vec[1]
# #   aux[i,]$Yr=year_aux[ind]
# #   
# #   # seas 2
# #   aux[i+1,]$FltSvy=-aux[i+1,]$FltSvy
# #   aux[i+1,]$Nsamp=vec[2]
# #   aux[i+1,]$Seas=5.5
# #   aux[i+1,]$Yr=year_aux[ind]
# #   
# #   di=dim(aux)[2]
# #   aux[i+1,8:di]=fleet[ind,-(1:7)]
# # 
# #   # seas 3
# #   aux[i+2,]$FltSvy=-aux[i+2,]$FltSvy
# #   aux[i+2,]$Nsamp=vec[3]
# #   aux[i+2,]$Seas=8.5
# #   aux[i+2,]$Yr=year_aux[ind]
# #   
# #   di=dim(aux)[2]
# #   aux[i+2,8:di]=fleet[ind,-(1:7)]
# # 
# #   # seas 4
# #   
# #   aux[i+3,]$FltSvy=-aux[i+3,]$FltSvy
# #   aux[i+3,]$Nsamp=vec[4]
# #   aux[i+3,]$Seas=-11.5
# #   aux[i+3,]$Yr=year_aux[ind]
# #   
# #   di=dim(aux)[2]
# #   aux[i+3,8:di]=fleet[ind,-(1:7)]
# # 
# # }
# # 
# # head(aux)
# # freqbin[[fltn]]=aux # include in final
# # 
# # # save sizefreq ---------------------------------------------------------------------------
# # 
# # ## Input sizefreqbin 80 +
# # sizefreq<-rbind(freqbin[[1]],freqbin[[2]])
# # colnames(sizefreq)=colnames(cdata$sizefreq_data_list[[1]])
# # cdata$Nobs_per_method<-(dim(sizefreq)[1]) # number of rows for the 80+ data with Super Periods
# # sizefreq<-list(sizefreq)
# # cdata$sizefreq_data_list<-sizefreq

# 4) write shake_data.ss ------------------------------------------------------------

### Update SS datafile:
mkdir("data/ss files")
r4ss::SS_writedat(cdata, "./data/ss files/shake_data.ss", overwrite = TRUE) # Write data file

# 5) delete files --------------------------------------------------------------

# delete catchData25.csv
#file.remove(file.path(getwd(), "data", "catch", "catchData25.csv"))

# delete lenDistData25.csv
#file.remove(file.path(getwd(), "data", "LFDs", "lenDistData25.csv"))

