#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare SS shake forecast F scenarios #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 05/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido       #
# Francisco Izquierdo #
# Santiago Cervino    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#                                                                                                        
#   Authors:                                       #                                                                     
#   Francesco Masnadi (CNR-IRBIM & UNIBO, Ancona)  #                                                                   
#   Massimiliano Cardinale (SLU Aqua, Lysekil)     #
#   Christopher Griffiths (SLU Aqua, Lysekil)      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# 0) Edits/Decisions -----------------------------------------------------------

rm(list=ls())
library(writexl)

## Select run -----------------------------------------------------------------

run <- 'model/final' # model folder

## RP --------------------------------------------------------------------------

Blim=6011
Bpa=7556	
Flim=0.694	
Fpa=0.558	
Fmsy=0.221
FmsyLower=0.151	
FmsyUpper=0.311	
Bmsy=50878


## Fix intermediate year --------------------------------------------------------
# Modify!!!
year_inter=2026

## Values of F in the intermediate year ----------------------------------------

## There are the average of the values in the Naver+1 years, introduce Naver in the 
## next argument.

Naver=2 # number of average years-1

## Sequence of Fmult for year_inter+1 (see line 121)
# Number of models in the Fmult sequence between (s1) 0 and 
# FmsyLower, (s2) FmsyLower+0.01 and FmsyUpper, and (s3) FmsyUpper and Flim+0.05.
s1=15
s2=15
s3=15

## year_inter+2 use the multiplier for Fmsy not the previous sequence

# 1) Create scenarios -------------------------------------------------------------

library(r4ss)

mod_path <- paste0(getwd(), "/", run, sep="") 

dir.create(path= paste0(mod_path,"/Forecast"), showWarnings = T, recursive = T)
dir.create(path= paste0(mod_path,"/Forecast/Forecast files"), showWarnings = T, recursive = T)

dir.forecastTAC <-  paste0(mod_path,"/Forecast/Forecast files")

file.copy(paste(mod_path, "forecast.ss", sep="/"),
          paste(dir.forecastTAC, "forecast.ss", sep="/"))

## read forecast from forecast folder
fore <- r4ss::SS_readforecast(file = file.path(dir.forecastTAC, "forecast.ss"),
                              verbose = FALSE)

## Look for values of apical F for intermediate year 2026 (report:14)
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE) ## read

# int year ---------------------------------------------------------------------

## prepare intermediate year data 
dat=replist$exploitation
dat=dat[-c(3,4,5,6)];head(dat)

Nfor=fore$Nforecastyrs
startyear=max(dat$Yr)-Nfor-Naver
endyear=max(dat$Yr)-Nfor

## average of the last 3 years across seasons and fleets
data<-subset(dat,dat$Yr>=startyear & dat$Yr<=endyear)
data<-data[,-1] # remove year column
data_int<-aggregate(.~Seas,data=data,mean) 

## input intermediate year data
dimen=dim(data_int)
Year=rep(endyear+1,dimen[1]*(dimen[2]-1))
fore_dat_int=data.frame(Year)
fore_dat_int$Seas=rep(1:4)
fore_dat_int$Fleet=sort(rep(1:4,4))
fore_dat_int$F=as.vector(as.matrix(data_int[,-1]))

# define Fmult ---------------------------------------------------------------- 

# From reference points html

datmul=replist$exploitation
datmul=subset(datmul, datmul$Yr>=(year_inter-(Naver+1)) & datmul$Yr<=(year_inter-1))

# Mean of all Naver+1 years

fsq=mean(datmul$F_std[c(1,5,9)])
#fsq=mean(datmul$F_std[c(9)])

a=FmsyLower/fsq
b=FmsyUpper/fsq
c=Flim/fsq

fmult_msy=Fmsy/fsq
  
fmult=c(seq(0,a,length.out=s1),seq(a+0.01,b,length.out=s2),seq(b+0.01,c+0.05,length.out=s3),
        c(4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,8.9,9,9.5,10,10.38,10.39,10.5,11,11.5,12,12.5))

Fmult_names=paste0("Fmult",fmult)
save(Fmult_names,file=paste0(mod_path,"/Forecast","/Fmult_names.RData"))
l_fmult=length(fmult)
#fore_dat_int$F<-fore_dat_int$F*(fsq/fsq_mean_all_3)
aux=fore_dat_int

# Geometric mean for intermediate year
ass.sum <- SSsummarize(SSgetoutput(dirvec="model/final"))

hist.rec <- as.data.frame(ass.sum$recruits) %>% filter(Yr %in% 2016:(year_inter-1)) %>% .[,1]  #! since 1978 or 1990?

virg.rec <- as.data.frame(ass.sum$recruits) %>% filter(Label == "Recr_Virgin") %>% .[,1]

gmrec <- exp(mean(log(hist.rec)))

## create data for following forecast years using int year and Fmult
for (i in 1:l_fmult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$Year=endyear+j
    aux_fore$F=fmult[i]*aux$F
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$Year=endyear+j
  aux_fore$F=fmult_msy*aux$F
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  
  fore$InputBasis<-99 # 99 for F, 2 for Catch
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  
  # Mean of historical recruitments
  fore$fcast_rec_option <- 2 #= value*(virgin recruitment)
  fore$fcast_rec_val <- gmrec/virg.rec # geomean / virgin rec
  
  ## write all forecast files/scenarios
  r4ss::SS_writeforecast(fore, dir = dir.forecastTAC, file = paste0("forecast",Fmult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}

# 2) Do forecast ------------------------------------------------------------------

## FIRST generate forecast scenarios/files 

# load packages
library(r4ss)
library(ss3diags)
library(readr)
library(plyr)
library(reshape)
library(tidyverse)
library(parallel)
library(doParallel)

sessionInfo() # check for ss3diags_2.0.1, r4ss_1.43.0, kobe_2.2.0

## pre-register parallel function 
registerDoParallel(2)

## set seed for consistency
set.seed(1234)


dir <- paste0(getwd(), sep="") 
# create subfolder arrays for naming


tacs = paste0("Fmult",fmult) # TAC levels for forecast

# create forecast folder and subfolders (if the first time)
for(i in 1:length(run)){
  dir.runN <- paste0(dir,"/",run[i])
  dir.runN.new <- paste0(mod_path,"/Forecast")
  dir.create(path=dir.runN.new, showWarnings = T, recursive = T)
  for(j in 1:length(tacs)){
    dir.tacN <- paste0(dir.runN.new,"/",tacs[j])
    dir.create(path=dir.tacN, showWarnings = T, recursive = T)
    # copy the SS base files in every TAC subfolder 
    file.copy(paste(dir.runN, "starter.ss", sep="/"),
              paste(dir.tacN, "starter.ss", sep="/"))
    file.copy(paste(dir.runN, "control_fixed.ss", sep="/"),
              paste(dir.tacN, "control_fixed.ss", sep="/"))
    
    # ctlfile<-"control_fixed.ss"
    # ctl <- readLines(file.path(dir.tacN, ctlfile))
    # ind<-grep("end_yr_for_ramp", ctl)
    # aux<-ctl[ind] 
    # aux<-gsub("2025", "2024", aux) ### CHECK next year!
    # 
    # ctl[ind] <- aux
    # 
    # file.remove(file.path(dir.tacN, ctlfile))
    # writeLines(ctl, file.path(dir.tacN, ctlfile))
    
    file.copy(paste(dir.runN, "shake_data.ss", sep="/"),
              paste(dir.tacN, "shake_data.ss", sep="/"))	
    #file.copy(paste(dir.runN, "wtatage.ss", sep="/"),
    #          paste(dir.tacN, "wtatage.ss", sep="/"))
    file.copy(paste(dir.runN, "ss.par", sep="/"),
           paste(dir.tacN, "ss.par", sep="/"))
    # ss.par.file<-readLines( paste(dir.tacN, "ss.par", sep="/"))
    # linen <- NULL
    # linen <- grep("recdev1", ss.par.file)
    # vec<-ss.par.file[linen+1]
    # vec<-as.numeric(strsplit(vec, " ")[[1]]);vec<-vec[-1]
    # vec[length(vec)]<-0
    # 
    # ss.par.file[linen+1]<- paste(" ", paste(vec, collapse = " "), sep = "")
    # write(ss.par.file, paste(dir.tacN, "/ss.par", sep=""))
    
    
    
    file.copy(paste(dir.runN, "ss.exe", sep="/"),
              paste(dir.tacN, "ss.exe", sep="/"))
    
    # copy the right forecast file from the "forecast_TAC" folder
    file.copy(paste(dir.forecastTAC,  paste0("forecast",tacs[j], ".ss") , sep="/"),
              paste(dir.tacN, "forecast.ss", sep="/"))
    # Edit "starter.ss" 
    starter.file <- readLines(paste(dir.tacN, "/starter.ss", sep=""))
    linen <- NULL
    linen <- grep("#_init_values_src", starter.file)
    starter.file[linen] <- paste0("1 # 0=use init values in control file; 1=use ss.par") # tells it to use the estimate parameters
   
    linen <- grep("#_last_estimation_phase", starter.file)
    starter.file[linen] <- paste0("0 # Turn off estimation for parameters entering after this phase")
    write(starter.file, paste(dir.tacN, "/starter.ss", sep=""))
    
  }
}

# run forecasts for each model
mc.cores = 1 # set the number of cores as Nmodels x Nscenarios

#for(i in 1:length(run)){
    dir.runN.new <- paste0(mod_path,"/Forecast")
    mclapply(file.path(paste0(dir.runN.new,"/",tacs)), r4ss::run, extras = "-nohess", skipfinished = F,exe="ss")
  #   }

# 3) Output table --------------------------------------------------------------

# Table (forecast)

#rm(list=ls()) ## Clean environment
library(r4ss) 
library(icesAdvice)

# Years




# Catches, recruitment, F, SSB
mod_path=paste0(paste0(getwd(), "/", sep="") , run, sep="") # mod_path
fore_path <-  paste0(mod_path,"/Forecast")

## Retros for directories
tabledir_fore<- paste(fore_path, "/table", sep="")
dir.create(tabledir_fore)

retroModels <- SSgetoutput(dirvec=file.path(fore_path,
                                            Fmult_names))
info_fore<- paste(fore_path, "/info models", sep="")
dir.create(info_fore)

save(retroModels,file=paste(info_fore, "/forecast.RData", sep=""))

retroSummary <- SSsummarize(retroModels)

# SSB -------------------------------------------------------------------------

SSB <- as.data.frame(retroSummary["SpawnBio"])

Table_Inter=data.frame(matrix(0,ncol=7,nrow=1))
colnames(Table_Inter)=c(paste("SSB",year_inter+1,sep=""),paste("F",year_inter,sep=""),paste("Rec",year_inter,sep=""),
                        paste("Catches",year_inter,sep=""),
                        paste("Landings",year_inter,sep=""),
                        paste("Discards",year_inter,sep=""),paste("Rec",year_inter+1,sep=""))

ind=which((year_inter+1)==SSB$SpawnBio.Yr)
Table_Inter[,1]=SSB[ind,1]

lastyear=max(SSB$SpawnBio.Yr)

Table_fmult=data.frame(matrix(0,ncol=6,nrow=length(Fmult_names)))
rownames(Table_fmult)=round(fmult,3)
colnames(Table_fmult)=c(paste("SSB",lastyear,sep=""),
                        paste("F",year_inter+1,sep=""),
                        paste("Rec",year_inter+1,sep=""),
                        paste("Catches",year_inter+1,sep=""),
                        paste("Landings",year_inter+1,sep=""),
                        paste("Discards",year_inter+1,sep=""))
         

ind=which(lastyear==SSB$SpawnBio.Yr)
ncol<-dim(SSB)[2]
aux=SSB[ind,-c(ncol-1,ncol)]# remove year columns
colnames(aux)=NULL
Table_fmult[,1]=unlist(aux)


# F ----------------------------------------------------------------------------
# Note that F is from intermediate year

Fvalue <- as.data.frame(retroSummary["Fvalue"])

ind=which((year_inter)==Fvalue$Fvalue.Yr)
Table_Inter[,2]=Fvalue[ind,1]

ind=which((lastyear-1)==Fvalue$Fvalue.Yr)
aux=Fvalue[ind,-c(ncol-1,ncol)]
colnames(aux)=NULL
Table_fmult[,2]=unlist(aux)



# Rec -------------------------------------------------------------------------
# Note constant recruitment!

Recr <- as.data.frame(retroSummary["recruits"])

ind=which((year_inter)==Recr$recruits.Yr)
Table_Inter[,3]=Recr[ind,1]
ind=which((year_inter+1)==Recr$recruits.Yr)
Table_Inter[,7]=Recr[ind,1]

ind=which((lastyear-1)==Recr$recruits.Yr)
aux=Recr[ind,-c(ncol-1,ncol)]
colnames(aux)=NULL
Table_fmult[,3]=unlist(aux)

# Catches ----------------------------------------------------------------------

lcat=length(Fmult_names)

Fmsy_vector=Table_fmult[,4]

for (i in 1:lcat){
  
  output=retroModels[[i]]
  
  fltnms <- setNames(output$definitions$Fleet_name,1:9) 
  
  ## Catch
  
  catch <- as_tibble(output$timeseries) %>% filter(Era == "FORE" ) %>% 
    select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) 
  names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:4], sep = "_"), paste('LanEst', fltnms[1:4], sep = "_"),
                    paste('CatEst', fltnms[1:4], sep = "_"))
  aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
  names(aux1) <- paste('DisEst', fltnms[1:4], sep = "_")
  catch <- catch %>% bind_cols(aux1) 
  catch <- catch %>% pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
    mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
    select('year', 'season', 'fleet', 'indicator', 'value')  
  
  Landings=subset(catch, catch$indicator=="LanEst")
  Landings$year=as.factor(Landings$year)
  library(plyr)
  Landings=ddply(Landings, .(year), summarize,  number=sum(value))
  
  
  Discards=subset(catch, catch$indicator=="DisEst")
  Discards$year=as.factor(Discards$year)
  Discards=ddply(Discards, .(year), summarize,  number=sum(value))
  
  
  Cat=subset(catch, catch$indicator=="CatEst")
  Cat$year=as.factor(Cat$year)
  Cat=ddply(Cat, .(year), summarize,  number=sum(value))
  
  
  
  if(i==1){
    Table_Inter[,4]=Cat[1,2]
    Table_Inter[,5]=Landings[1,2]
    Table_Inter[,6]=Discards[1,2]
  }
  
  ll=dim(Cat)[1]-1
  Table_fmult[i,4]=Cat[ll,2]
  Table_fmult[i,5]=Landings[ll,2]
  Table_fmult[i,6]=Discards[ll,2]
  
  Fmsy_vector[i]=Cat[dim(Cat)[1],2]
}

# Save -------------------------------------------------------------------------

Table_Inter
Table_fmult=Table_fmult[,-3]
Fmsy_vector=as.data.frame(Fmsy_vector)
colnames(Fmsy_vector)<-paste("Catch",lastyear,sep="")
Table_fmult<-cbind(Table_fmult, Fmsy_vector)


write.csv(Table_Inter, paste(tabledir_fore, "/table intermediate year.csv", sep=""))
write.csv(Table_fmult, paste(tabledir_fore, "/table Fmult.csv", sep=""))


# 4) Interpolate Short Term Projection -----------------------------------------

## Run the next lines if you are running in a new R sesion only point 4)
run="model/final"
mod_path=paste0(paste0(getwd(), "/", sep="") , run, sep="")
fore_path <-  paste0(mod_path,"/Forecast")
tabledir_fore<- paste(fore_path, "/table", sep="")


stTab=read.csv( paste(tabledir_fore, "/table Fmult.csv", sep=""))
stTab_int=read.csv( paste(tabledir_fore, "/table intermediate year.csv", sep=""))[,-1]



## Function to interpolate -----------------------------------------------------

# Interpolate relevant figures for ST
#colName<-"Catch"
#varVal<-TAC*0.85
#rowName<-"Rec. Plan TAC constraint (-15%)"

interpolateStTab <- function (colName, varVal, rowName, stTab){
  # colName <- valid variable names ("Fmult", "F", "Yield", "Catch", "Bio", "SSB")
  # varVal <- amount to interpolate
  # rowName <- text with reference to identify (e.g. "Fmsy")
  # stTab is the tab produced with rscript with a grid of Fmult
  # It returns a line with the df strulture with all the variables interpolated
  varNames <- names(stTab)
  stopifnot(colName %in% varNames)
  
  # Identify upper and lower row indices regarding varValue
  vec <- stTab[,colName]
  stopifnot(varVal>min(vec) & varVal < max(vec))
  if (which(vec>varVal)[1] == 1) iLow <- max(which(vec>varVal)) 
  else iLow <- max(which(vec<varVal))
  iUp <- iLow + 1
  
  # variabble to estimate (all but varName)
  x1 <- stTab[iLow, colName]
  x2 <- varVal
  x3 <- stTab[iUp, colName]
  y2List <- list()
  for (i in varNames) {
    if (i == colName) y2List[i] <- varVal else {
      y1 <- stTab[iLow, i]
      y3 <- stTab[iUp, i]
      y2List[i] <- y1 + (x2-x1) * (y3-y1) / (x3 - x1)  # function to interpolate
    }
  }
  newLine <- as.data.frame(y2List)
  row.names(newLine) <- rowName
  return(newLine)  # data frame same structure than stTab with the line asked
}

## Text of Ices template ------------------------------------------------------
"MSY approach = FMSY"
"EU MAP: FMSY"
"F = MAP FMSY lower"
"F = MAP FMSY upper"
"F = 0"
"F = Fpa"
"SSB (2028) = Blim"
"SSB (2028) = Bpa"
"SSB (2028) = MSY Btrigger"
"SSB (2028) = SSB(2027)"
"F = F2026"


## Set years for the text in Cat. Opt. Tab. basis
intYr=year_inter
ssbYr <- intYr + 2
cotYr <- intYr + 1

### F msy ----------------------------------------------------------------------
basis <- "MSY approach = FMSY"
df1 <-interpolateStTab(colName=paste("F",year_inter+1,sep=""), 
                       varVal =Fmsy, rowName=basis, stTab)

### F = 0 ----------------------------------------------------------------------
xx <- stTab[1,]
#xx[, 1:4] <- 0
rownames(xx) <- "F = 0"
df1 <- rbind(df1, xx)

### PA -----------------------------------------------------------------------------

basis <- paste("SSB (", ssbYr, ") = Blim", sep="")
df1 <- rbind(df1, interpolateStTab(colName=paste("SSB",year_inter+2,sep=""), Blim, basis, stTab))
basis <- paste("SSB (", ssbYr, ") = Bpa = MSY Btrg", sep="")
df1 <- rbind(df1, interpolateStTab(colName=paste("SSB",year_inter+2,sep=""), Bpa, basis, stTab))


#df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), Flim, "F = Flim", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), Fpa, "F = Fpa", stTab))



### Equal things ---------------------------------------------------------------

SSBint=stTab_int[,1]
#TAC=
Fint=stTab_int[,2]
df1 <- rbind(df1, interpolateStTab(colName=paste("SSB",year_inter+2,sep=""), SSBint, "SSB (2028) = SSB(2027)", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), Fint, "F = F2026", stTab))

### Management plan -------------------------------------------------------------

catches_fmsy=df1[1,4]
TAC=17445

df1 <- rbind(df1,interpolateStTab(colName=paste("F",year_inter+1,sep=""), varVal =Fmsy, rowName="EU MAP: Fmsy", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), FmsyLower, "F = MAP FMSY lower", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), FmsyUpper, "F = MAP FMSY upper", stTab))

#df1 <- rbind(df1, interpolateStTab(colName= paste("Catches",year_inter+1,sep=""), TAC*1.2, "TAC +20%", stTab))

### End Management Plan #############################

### Alternative Management Plan ------------------------------------------------------
#ForeCatch_2023 10823.8 1264.48
# ForeCatch_2024=12566.8
# if(catches_fmsy>ForeCatch_2024*1.2){
#   df1 <- rbind(df1, interpolateStTab(colName= paste("Catches",year_inter+1,sep=""), ForeCatch_2024*1.2, paste("EU MAP: Fmsy (TAC*1.2 (15079))",year_inter,sep=""), stTab))
# } else {
  #df1 <- rbind(df1,interpolateStTab(colName=paste("F",year_inter+1,sep=""), varVal =Fmsy, rowName="EU MAP: Fmsy (TAC*1.2 (15079))", stTab))
#}
df1 <- rbind(df1, interpolateStTab(colName= paste("Catches",year_inter+1,sep=""), TAC, "equal TAC", stTab))


colnames(df1)=c("Fmult",colnames(df1)[2:ncol(df1)])

df2=df1[,c(1,3,4,5,6,2,7)]

write.csv (df2, paste(tabledir_fore, "/catch table report.csv", sep=""))


# Extra (with extra multipliers)
data_extra<- read_csv("model/final/Forecast/table/catch table report.csv")
data_extra<-as.data.frame(data_extra)
data_extra_ordenado <- data_extra[order(data_extra[,2]), ]

#data_extra_ordenado<-data_extra_ordenado[-5,]

ind=which(data_extra_ordenado=="F = MAP FMSY lower")
ind2=which(data_extra_ordenado=="F = MAP FMSY upper")

fmult_extra<-seq(round(as.numeric(data_extra_ordenado[ind,2]),1),
                 round(as.numeric(data_extra_ordenado[ind2,2]),1),
                 by=0.1)
extra<-rep(0,7)
colnames(extra)=NULL
for (i in 1:length(fmult_extra)){
  a=interpolateStTab(colName="X", 
                   varVal =fmult_extra[i], rowName="", stTab)
  extra=rbind(extra,a)
}
extra<-extra[,c(1,3,4,5,6,2,7)]
extra$nueva_columna <- NA
extra<-extra[-1,]
data_extra_ordenado <- data_extra_ordenado[, c(2:length(data_extra_ordenado), 1)]

colnames(extra)<-colnames(data_extra_ordenado)
final<-rbind(extra,data_extra_ordenado)
final <- final[order(final[,1]), ]
final[,2]<-icesRound(final[,2])
final[,3:7]<-round(final[,3:7])
write.csv(final, paste(tabledir_fore, "/Table10.7 b extended.csv", sep=""), row.names=FALSE)
write_xlsx(final, paste(tabledir_fore, "/Table10.7 b extended.xlsx", sep=""))

## Catch Option Table - COT (ICES format) -----------------------------------------
icesAdviced <- 14907 
icesAdviced_low <- 10526 
icesAdviced_upper <-20125 
cot <- as.data.frame(matrix(nrow=dim(df1)[1], ncol=11))
names(cot) <- c("basis", "tcat", "plan", "pdis", "ft", "fpl", "fpd", "ssb", "schang","tacchang" ,"adchang")   #, "tacchang")
cot$basis <- rownames(df1)
cot$tcat <- round(df1[,4]) 
cot$plan <- round(df1[,5]) 
cot$pdis <- round(cot$tcat - cot$plan)
cot$ft <- icesRound(df1[,3])
cot$fpl <- icesRound(as.numeric(cot$ft) * cot$plan / cot$tcat)
cot$fpl[as.numeric(cot$ft)<=0.0000001] <- 0
cot$fpd <- icesRound(as.numeric(cot$ft) * cot$pdis / cot$tcat)
cot$fpd[as.numeric(cot$ft)<=0.0000001] <- 0
cot$ssb <- round(df1[,2], 0) 
cot$adchang <- as.character(paste(round(100*(cot$tcat-icesAdviced)/icesAdviced, 0), " %", sep=""))
cot$tacchang <- as.character(paste(round(100*(cot$tcat-TAC)/TAC, 0), " %", sep=""))
cot$schang <- as.character(paste(round(100*(cot$ssb-SSBint)/SSBint, 0), " %", sep=""))
 cot <- cot[c(8, 9, 10, 2, 5, 3, 4, 6,7,11),]
 cot[2,11] <- as.character(paste(round(100*(cot$tcat[2]-icesAdviced_low)/icesAdviced_low, 0), " %", sep=""))
 cot[3,11] <- as.character(paste(round(100*(cot$tcat[3]-icesAdviced_upper)/icesAdviced_upper, 0), " %", sep=""))
 
write.csv (cot, paste(tabledir_fore, "/catOptionsTab.csv", sep=""), row.names=FALSE)
write_xlsx(cot, paste(tabledir_fore, "/catOptionsTab.xlsx", sep=""))
