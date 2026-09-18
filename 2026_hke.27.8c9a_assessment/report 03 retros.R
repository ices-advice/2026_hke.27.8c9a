#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract retrospectives for SS shake model info #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 26/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido       #
# Anxo Paz            #
# Francisco Izquierdo #
# Santiago Cervino    #
#~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

rm(list=ls()) ## Clean environment
library(r4ss) 
library(icesAdvice)

## Model  path
run<-"model/final"
mod_path <- paste0(getwd(), "/", run, sep="") 

yper=0:-5 ## years period for retros


retroModels <- SSgetoutput(dirvec=file.path(mod_path, "retros",
                                            paste("retro",yper,sep="")))

save(retroModels, file=paste0(mod_path, 
                              "/retros/retroModels.RData", sep=""))

retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1


endyrvec <- retroSummary$endyrs + yper


# Extract time series ------------------------------------------------------------------

## SSB -------------------------------------------------------------------------

len_re=length(yper)-1

## Delete forecast years
library(reshape2)
library(ggplot2)
library(stringr)
startyr<-unique(retroSummary$startyrs)
endyr<-unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears<-length(years)
nforecastyears<-3 # Set to 3 in starter file
yearsfore <- c(years, years[nyears]+(1:nforecastyears))
nyearsfore=length(yearsfore)-length(years)
SSB <- as.data.frame(retroSummary["SpawnBio"])
nrSSB=nrow(SSB)
seq_aux=((nrSSB-nyearsfore)+2):nrSSB
SSB <- SSB[-c(1,2,seq_aux),] 
SSB <- SSB[,-(length(yper)+1)]
names(SSB) <- c(paste0("Retro",yper), "Year")

## Correct last years for each retro
endyr=max(endyrvec)
ind2=which(SSB$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(SSB$Year==endyrvec[i]+1)
  SSB[(ind1+1):(ind2+1),i]=NA
}
SSBm=SSB
SSB <- melt(SSB, id="Year")
names(SSB) <- c("Year", "Retro", "SSB")


## F (SS) ----------------------------------------------------------------------

## Delete forecast years
Fvalue <- as.data.frame(retroSummary["Fvalue"])
nrF=nrow(Fvalue)
seq_aux=((nrF-nyearsfore)+1):nrF
Fvalue <- Fvalue[-c(seq_aux),] # Note that F starts directly in 1953
Fvalue <- Fvalue[,-(length(yper)+1)]
names(Fvalue) <- c(paste0("Retro",yper), "Year")

## Correct last years for each retro
endyr=max(endyrvec)
ind2=which(Fvalue$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(Fvalue$Year==endyrvec[i])
  Fvalue[(ind1+1):ind2,i]=NA
}

Fvaluem <- melt(Fvalue, id="Year")
names(Fvaluem) <- c("Year", "Retro", "F")

## Rec -------------------------------------------------------------------------

## Delete forecast years
Recr <- as.data.frame(retroSummary["recruits"])
nrRecr=nrow(Recr)
seq_aux=((nrRecr-nyearsfore)+1):nrRecr
Recr <- Recr[-c(1,2,seq_aux),] 
Recr <- Recr[,-(length(yper)+1)]
names(Recr) <- c(paste0("Retro",yper), "Year")
## Correct last years for each retro
endyr=max(endyrvec)
ind2=which(Recr$Year==endyr)
for (i in 2:length(yper)){
  ind1=which(Recr$Year==endyrvec[i])
  Recr[(ind1+1):ind2,i]=NA
}
Recrm=Recr
Recr <- melt(Recr, id="Year")
names(Recr) <- c("Year", "Retro", "Recruitment")


## Mohn's rho ICES-------------------------------------------------------------------

library(icesAdvice)
# Recruitment
mohn(Recrm, peels = len_re)


#Fvalue
mohn(Fvalue, peels = len_re)# from ss



#SSBm
mohn(SSBm, peels = len_re, plot=T)


# Summary plot -----------------------------------------------------------------

aux=Fvaluem[,2]

val=unique(aux)
new=rep(0,length(aux))
for(i in 1:length(aux)){
  ind=which(aux==val[i])
  new[ind]=(endyrvec[i])
}

tab_f_r=data.frame(Fvaluem[,1],Fvaluem[,3],Recr[,3],new)
tab_f_r <- na.omit(tab_f_r)
colnames(tab_f_r)=c("Y", "F", "R","retYr")

aux=SSB[,2]
val=unique(aux)
new1=rep(0,length(aux))
for(i in 1:length(aux)){
  ind=which(aux==val[i])
  new1[ind]=(endyrvec[i])
}
tab_ssb=data.frame(SSB[,1],SSB[,3],new1)
tab_ssb <- na.omit(tab_ssb)
colnames(tab_ssb)=c("Y", "SSB", "retYr")
yrs<-years
lastYr<-endyr
yrng <- c(1990,endyr)
fmax <- max(tab_f_r$F[tab_f_r$Y>=yrng[1]])
smax <- max(tab_ssb$SSB[tab_ssb$Y>=yrng[1]])
rmax <- max(tab_f_r$R[tab_f_r$Y>=yrng[1]])
plotdir_report<-paste0(getwd(), "/","report/plots", sep="") 
jpeg(paste0(plotdir_report,"/Figure 10.10.png"), width = 2500, height = 3000, res = 300)
#png(paste0(plotdir_report,"/Figure 10.10.png"), height = 700, width = 1000)

par(mfrow=c(3,2), mar=c(3,3,2,2), mgp=c(2,0.8,0), oma=c(2,1.5,1.5,1.5) )

## PLot Recruitment
# absolute
print(plot(x=NA, y=NA, xlim=yrng, ylim=c(0,rmax), xlab="year", ylab="Rec (age 0)"))
for (yr in yrs){
  xx <- tab_f_r[tab_f_r$retYr==yr,]
  print(lines(xx$Y, xx$R, lwd= 1.4, lty=1,col=yr))
}

ssbrecf=retroModels$replist1$derived_quants
a=ssbrecf[substr(ssbrecf$Label,1,3)=="Rec",]
a=a[-c(1,2,(dim(a)[1]-3):dim(a)[1]),]
sd=a$StdDev

xx <- tab_f_r[tab_f_r$retYr==endyr,]
# lower= xx$R-2*sd
# upper=xx$R+2*sd
R.ic<- xx$R
upper  <- exp(log(R.ic)+sqrt(log(1+(1.96*sd/R.ic)^2)))
lower <- exp(log(R.ic)-sqrt(log(1+(1.96*sd/R.ic)^2)))


print(lines(xx$Y, lower,lwd= 1.2, lty=2,col="grey"))
print(lines(xx$Y, upper,lwd= 1.2, lty=2,col="grey"))

# Relative
print(plot(x=NA, y=NA, xlim=yrng, ylim=c(-1,2), xlab="year", ylab="Rec (age 0)"))
for(yr in yrs[-length(yrs)]){
  xx <- tab_f_r[tab_f_r$retYr==yr,]
  yy <- tab_f_r[tab_f_r$retYr==lastYr & tab_f_r$Y < yr+1,]
  print(lines(xx$Y, (xx$R-yy$R)/yy$R, col = yr, lwd= 1.4, lty=1))
}
print(abline(h=c(-0.1, -0.2, -0.3, 0.1, 0.2, 0.3), lwd= 0.8, lty=2))
print(text(1995, 0.5, paste("rhoMohn = ", round(mohn(Recrm, peels = len_re), 3) )))

## PLot F
# absolute
print(plot(x=NA, y=NA, xlim=yrng, ylim=c(0,fmax), xlab="year", ylab="F"))
for (yr in yrs){
  xx <- tab_f_r[tab_f_r$retYr==yr,]
  print(lines(xx$Y, xx$F, col = yr, lwd= 1.4, lty=1))
}

ssbrecf=retroModels$replist1$derived_quants
a=ssbrecf[substr(ssbrecf$Label,1,2)=="F_",]
a=a[-c((dim(a)[1]-2):dim(a)[1]),]
sd=a$StdDev

xx <- tab_f_r[tab_f_r$retYr==endyr,]
# lower= xx$F-2*sd
# upper=xx$F+2*sd

F.IC<- xx$F
upper  <- exp(log(F.IC)+sqrt(log(1+(1.96*sd/F.IC)^2)))
lower <- exp(log(F.IC)-sqrt(log(1+(1.96*sd/F.IC)^2)))

print(lines(xx$Y, lower,lwd= 1.2, lty=2,col="grey"))
print(lines(xx$Y, upper,lwd= 1.2, lty=2,col="grey"))

# Relative
print(plot(x=NA, y=NA, xlim=yrng, ylim=c(-0.5,0.5), xlab="year", ylab="F"))
for(yr in yrs[-length(yrs)]){
  xx <- tab_f_r[tab_f_r$retYr==yr,]
  yy <- tab_f_r[tab_f_r$retYr==lastYr & tab_f_r$Y < yr+1,]
  print(lines(xx$Y, (xx$F-yy$F)/yy$F, col = yr, lwd= 1.4, lty=1))
}
print(abline(h=c(-0.1, -0.2, -0.3, 0.1, 0.2, 0.3), lwd= 0.8, lty=2))
print(text(1995, 0.5, paste("rhoMohn = ", round(mohn(Fvalue, peels = len_re), 3) )))

## PLot SSB
# absolute
print(plot(x=NA, y=NA, xlim=yrng, ylim=c(0,smax), xlab="year", ylab="SSB"))
for (yr in (yrs)){
  xx <- tab_ssb[tab_ssb$retYr==yr,]
  print(lines(xx$Y, xx$SSB, col = yr, lwd= 1.4, lty=1))
}
ssbrecf=retroModels$replist1$derived_quants
a=ssbrecf[substr(ssbrecf$Label,1,3)=="SSB",]
a=a[-c(1,2,(dim(a)[1]-5):dim(a)[1]),]
sd=a$StdDev

xx <- tab_ssb[tab_ssb$retYr==endyr,]
# lower= xx$SSB-2*sd
# upper=xx$SSB+2*sd

SSB.ic<- xx$SSB
upper  <- exp(log(SSB.ic)+sqrt(log(1+(1.96*sd/SSB.ic)^2)))
lower <- exp(log(SSB.ic)-sqrt(log(1+(1.96*sd/SSB.ic)^2)))

print(lines(xx$Y, lower,lwd= 1.2, lty=2,col="grey"))
print(lines(xx$Y, upper,lwd= 1.2, lty=2,col="grey"))

# Relative
print(plot(x=NA, y=NA, xlim=yrng, ylim=c(-0.5,0.7), xlab="year", ylab="SSB"))
for(yr in yrs[-length(yrs)]){
  xx <- tab_ssb[tab_ssb$retYr==yr,]
  yy <- tab_ssb[tab_ssb$retYr==lastYr & tab_ssb$Y < yr+2,]
  print(lines(xx$Y, (xx$SSB-yy$SSB)/yy$SSB, col = yr, lwd= 1.4, lty=1))
}

print(abline(h=c(-0.1, -0.2, -0.3, 0.1, 0.2, 0.3), lwd= 0.8, lty=2))
print(text(1995, 0.5, paste("rhoMohn = ", round(mohn(SSBm, peels = len_re), 3) ))  )

mtext("retros Pattern (absolute (left) and relative (right)). Red dashed lines IC of 95%",  outer=T)

dev.off()

