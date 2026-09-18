
### Script to read and check southern hake intercatch data ###

# Input: intercatch files for catch in weight and length distribution.
#
# Output: files for ICES stock assessment.
#
# Authors: Santiago Cerviño
# Reviewed by: Marta Cousido Rocha in 2026

# For saving results!
mkdir("data/LFDs")
mkdir("data/Intercatch")
mkdir("data/catch")

rm(list=ls())

# Functions --------------------------------------------------------------------

sopCtr <- function(xTab, a.=a, b.=b) {
  # input a length distribution tab and adds sop estimation and soprat
  lNames <- names(xTab)[grep("Lngt", names(xTab))]
  lenN <- as.numeric(substr(lNames, start=5, stop=10)) / 10
  wLenN <- a. * (lenN+0.5) ^ b.  # weight at len in kg
  sop <- NA
  for (i in 1:dim(xTab)[1]) sop[i] <- rowSums(wLenN * xTab[i,lNames])  
  xTab$sop <- sop
  xTab$soprat <- xTab$sop / xTab$Catch..kg
  return(xTab)
}


sopCorrection <- function(xTab, sopRange=c(0.95, 1.05)){
  # xTab is a ld data frame with sop and soprat fields 
  # Returns a data.frame with sop correction for records out of sopRange
  sopCorRowNames <- rownames(xTab[!(xTab$soprat > sopRange[1] & xTab$soprat < sopRange[2]),])
  lNames <- names(xTab)[grep("Lngt", names(xTab))]
  xTab[sopCorRowNames, lNames] <- xTab[sopCorRowNames, lNames] / xTab[sopCorRowNames, "soprat"]
  return(xTab)
}


allocUnalloc <- function(xTab, seasonCode=datYr) {
  # Allocating UNALLOCATED (records with season= 2014). Adding Catch/4 to each season
  # xTab is a data frame with some yearly records with with landings and without Ld
  # the function assign the amount to seasons in area and fleet and afterwards raises the ld to new amount
  
  xx <- xTab[xTab$Season==seasonCode, c("Area", "Fleets", "Catch..kg")]   # table of unallocated
  lNames <- names(xTab)[grep("Lngt", names(xTab))]
  
  for(i in 1:dim(xx)[1]){
    ar <- xx[i,"Area"]
    fl <- xx[i, "Fleets"]
    numSeas <- dim(xTab[xTab$Season%in%1:4 & xTab$Area==ar & xTab$Fleets==fl,])[1]
    unCat <- xx[i,"Catch..kg"] / numSeas  # unallocated catches to add to each season
    for(s in 1:4){
      cat1 <- xTab[xTab$Season==s & xTab$Area==ar & xTab$Fleets==fl, "Catch..kg"]
      rat1 <- (cat1 + unCat) / cat1
      xTab[xTab$Season==s & xTab$Area==ar & xTab$Fleets==fl, c("Catch..kg", lenNames)] <-
        xTab[xTab$Season==s & xTab$Area==ar & xTab$Fleets==fl, c("Catch..kg", lenNames)] * rat1
    }
  }
  xTab <- xTab[xTab$Season%in%1:4,]  # delete records that are now allocated
  return(xTab)
}

allocMisMis <- function (xTab, flTrg="MIS_MIS_0_0_0_HC", aggBy=c("Season", "Area"), ...) {
  # Allocating MIS_MIS_0_0_0_HC records (landings without metier assignement) in season 1, 2, 3, 4 and areas VIIIs and IXa
  # flTrg is the fleet target where allocate Lds
  # aggBy is the columns used to extract Lds to allocate in flTrg
  fl <- flTrg
  xx <- xTab[xTab$Fleets==fl, c(aggBy, "Catch..kg")]   # MIS_MIS table
  yy <- xTab[xTab$Fleets!=fl,]
  yy <- aggregate(yy[, c("Catch..kg", lenNames)], by=yy[,aggBy], sum)
  #stopifnot(dim(xx)[1]==dim(yy)[1])
  
  for(i in 1:dim(xx)[1]){
    ar <- xx[i,"Area"]
    s <- xx[i, "Season"]
    rat1 <- xx[i,"Catch..kg"] / yy[yy$Season==s & yy$Area==ar,"Catch..kg"]
    xTab[xTab$Season==s & xTab$Area==ar & xTab$Fleets==fl, lenNames] <-
      yy[yy$Season==s & yy$Area==ar, lenNames] * rat1
  }
  return(xTab)
}

allocMean <- function (xTab, lenNames, ...) {
  # Allocating ONE record without LD with the mean of all the others
  xx <- xTab[is.na(xTab$Caton),]   
  yy <- xTab[!is.na(xTab$Caton),]
  allocRat <- xx$Catch..kg / sum(yy$Catch..kg)
  xx[,lenNames] <- allocRat * apply(yy[,lenNames], 2, sum) 
  return(xx)
}

allocXY <- function (xTab, yTab, lenNames, ...) {
  # Allocating mean LD of xTab on each record of yTab
  xAlloc <- apply(xTab[,lenNames], 2, sum) / sum(xTab$Catch..kg)
  yTab[, lenNames] <- t(xAlloc %*% t(yTab[,"Catch..kg"]))
  return(yTab)
}


allocNonRep <- function(xTab, ...) {
  # Allocating non-reported spanish data in reported records raising catch and numbers and delete unreported ones
  
  xx <- xTab[xTab$Report.cat.=="N-Nonreported", c("Area", "Fleets", "Season", "Catch..kg")]   # table of non-reported
  lNames <- names(xTab)[grep("Lngt", names(xTab))]
  
  for(i in 1:dim(xx)[1]){
    ar <- xx[i,"Area"]
    fl <- xx[i, "Fleets"]
    ss <- xx[i, "Season"]
    cat <- xx[i,"Catch..kg"] 
    cat1 <- xTab[xTab$Season==ss & xTab$Area==ar & xTab$Fleets==fl & xTab$Report.cat.=="R-Reported", "Catch..kg"]
    stopifnot(cat1 > 0)
    rat1 <- (cat1 + cat) / cat1
    
    xTab[xTab$Season==ss & xTab$Area==ar & xTab$Fleets==fl & xTab$Report.cat.=="R-Reported", c("Catch..kg", lenNames)] <-
      xTab[xTab$Season==ss & xTab$Area==ar & xTab$Fleets==fl & xTab$Report.cat.=="R-Reported", c("Catch..kg", lenNames)] * rat1
  }
  xTab <- xTab[xTab$Report.cat.!="N-Nonreported",]  # delete records that are now allocated
  return(xTab)
}

raise2Effort <- function(xTab, yTab, ld=FALSE, ...){
  # Function to estimate catches for relevant fleets having effort but not caches (example: discards zero for pair trw in 2017)
  # xTab are the records with catch and effort to raise yTab
  # yTab are the records with effort but not catch
  # If ld=TRUE ld is also raised to the same fleet
  yTab$Catch..kg <-  yTab$Effort * sum(xTab$Catch..kg) / sum(xTab$Effort)
  if (ld){
    yTab <- allocXY(xTab, yTab)
  }
  return(yTab)
  
}

# Length-weight relationship ---------------------------------------------------
# For SOPs (updated in 2022 WKAngHake)
#a <- 0.00000377  
#b <- 3.16826   
# Previous to benchmark (used by SAP)
a <- 0.00000659  
b <- 3.01721

datYr <- 2025  #year of data

# Read data --------------------------------------------------------------------

## read data INTERCATCH weight data for all fleets
catTabFull <- read.table("boot/data/intercatch/StockOverview.txt", header = TRUE, sep = "\t",)

## read INTERCATCH length distribution data 
## (SKIP FIRST TWO LINES IN FILE) for fleets with length sampling
ldTabFull <- read.table("boot/data/intercatch/NumbersAtAgeLength.txt", header = TRUE, sep = "\t", skip=2)

## Check categories of catch ---------------------------------------------------

unique(catTabFull$Catch.Cat.) 
catTabFull[catTabFull$Catch.Cat.=="Logbook Registered Discard",]
sum(catTabFull[catTabFull$Catch.Cat.=="Logbook Registered Discard",]$Catch..kg)  
# IMP: zero Logbook Registered Discard in 2022, 2023, 2024, 2025!
sum(catTabFull[catTabFull$Country=="Spain" & catTabFull$Report.cat.=="N-Nonreported",]$Catch..kg) # Non reported Spanish catch
# Explanation: N-Nonreported are the catches computed by the SAP (are estimated
# using the total effort). Hence, under the name "landings" there are the official
# catches and under "N-Nonreported" the ones estimates by the SAP. If the differences
# are soft only the official ones are reported as in this case (as you can see N-Nonreported =0).


# Sample level ---------------------------------------------------------------
# By country and category
# Explanation: Caton is catch in Kg. We need to check if there are large values of
# caton with 0 or almost 0 NumLengthMeasurements or NumSamplesLength.


# 1º Look the following data:

#View(ldTabFull[,c(3:7, 9, 10, 13, 15:dim(ldTabFull)[2])])

# In 2024: -9 (no sense positive number of samples with -9 measurements)
# In 2025: -9 does not appear!
#ut<-ldTabFull[,c(3:7, 9, 10, 13, 15:dim(ldTabFull)[2])]
#View(subset(ut,ut$NumLengthMeasurements==-9))
#View(subset(ut,ut$NumSamplesLength==-9))

# No zeros!
#View(subset(ut,ut$NumLengthMeasurements==0))
#View(subset(ut,ut$NumSamplesLength==0))



# Review "parejas". Explanation: The "parejas" discard large amounts of fish.
# This art takes a lot of small
# fish (due to the type of mesh) that is discarded. 
#View(ldTabFull[ldTabFull$Fleets=="PTB_MPD_>=55_0_0" ,c(3:7, 9, 10, 13, 15)]) 
# There are a cases with 0 measurements but the corresponding caton's are small.
# Low sampling in the remaining discards?
# Note that when the observers are in the boat they try to go for Bacaladilla instead
# of hake.

# Review "arrastreros".
#View(ldTabFull[ldTabFull$Fleets=="OTB_MPD_>=55_0_0" ,c(3:7, 9, 10, 13, 15)])  
#View(ldTabFull[ldTabFull$Fleets=="OTB_DEF_>=55_0_0" ,c(3:7, 9, 10, 13, 15)]) 
#View(ldTabFull[ldTabFull$Fleets=="OTB" ,c(3:7, 9, 10, 13, 15)]) # Only landings, OTB is trawl in Portugal
#View(ldTabFull[ldTabFull$Fleets=="OTB_MCD_>=55_0_0" ,c(3:7, 9, 10, 13, 15)]) # Is Cádiz trawl


# Review "volanta".
# 2023 missing landing in 9a season 4, and missing several entries of discards 
# (the discard problem  can be ignored). You need to check that such information
# is available below in the catch file without lenght data.

# 2024 missing landing in 9a season 4, and low sampling and -9 problem in discards 
# (the discard problem  can be ignored). You need to check that such information
# is available below in the catch file without lenght data.

# 2025 missing landings and discards entries. You need to check that such information
# is available below in the catch file without length data.

#View(ldTabFull[ldTabFull$Fleets=="GNS_DEF_80-99_0_0" ,c(3:7, 9, 10, 13, 15)]) 

# Review "beta".
#View(ldTabFull[ldTabFull$Fleets=="GNS_DEF_60-79_0_0" ,c(3:7, 9, 10, 13, 15)]) # Only landings.

# Review "palangre".
# 2023 missing landing data in 9a seasons 3 and 4. The same, the information is below
# with no length data associated.
# 2025 missing landing data in 9a season 4. The same, the information is below
# with no length data associated.
#View(ldTabFull[ldTabFull$Fleets=="LLS_DEF_0_0_0" ,c(3:7, 9, 10, 13, 15)]) # Only landings.

# Review of other fleets less important!
#View(ldTabFull[ldTabFull$Fleets=="GNS_DEF_>=100_0_0" ,c(3:7, 9, 10, 13, 15)])
#View(ldTabFull[ldTabFull$Fleets=="GTR_DEF_60-79_0_0" ,c(3:7, 9, 10, 13, 15)])
#View(ldTabFull[ldTabFull$Fleets=="LHM_DEF_0_0_0" ,c(3:7, 9, 10, 13, 15)])
#View(ldTabFull[ldTabFull$Fleets=="MIS_MIS_0_0_0" ,c(3:7, 9, 10, 13, 15)])
#View(ldTabFull[ldTabFull$Fleets=="MIS_MIS_0_0_0_HC" ,c(3:7, 9, 10, 13, 15)])
#View(ldTabFull[ldTabFull$Fleets=="PS_SPF_0_0_0" ,c(3:7, 9, 10, 13, 15)])            


ldTabFull_sample_lev<-ldTabFull

sampLev <- ldTabFull_sample_lev[, c("Country", "Catch.Cat.", "NumLengthMeasurements", "NumSamplesLength")]
sampLev <- aggregate(ldTabFull_sample_lev[, c("NumLengthMeasurements", "NumSamplesLength")],
                     by=ldTabFull_sample_lev[, c("Country", "Catch.Cat.")], sum)
write.csv(sampLev, file="data/intercatch/sampleLevel.csv", row.names=FALSE, quote=FALSE)


# Length distribution table ---------------------------------------------------
# Select columns and change some names
names(ldTabFull)   # check useful columns
ldTab <- ldTabFull[, c(2:10, 16:((dim(ldTabFull)[2])-1))]   
# CHECK every year the number of length data columns above is correct
names(ldTab)[grep("Undetermined", names(ldTab))] <- substr(names(ldTab)[grep("Undetermined", names(ldTab))], start=13, 21) # PATCH to extract the part of the length field name
names(ldTab)
lenNames <- names(ldTab)[grep("Lngt", names(ldTab))] #names length fields
lenNames
nLd <- length(lenNames)
ldTab$SumLd <- apply(ldTab[,lenNames], 1, sum)

ldTab$Catch.Cat. <- as.character(ldTab$Catch.Cat.)  # to have common names in catch category before merge
ldTab$Catch.Cat.[ldTab$Catch.Cat. == "D"] <- "Discards"
ldTab$Catch.Cat.[ldTab$Catch.Cat. == "L"] <- "Landings"
ldTab$Catch.Cat. <- as.factor(ldTab$Catch.Cat.)

# Catch table -------------------------------------------------------------------
catTab1 <- catTabFull[, c("Year", "Country", "Season", "Area", "Catch.Cat.", "Fleets", "Effort", "Catch..kg", "Report.cat.")]


# Data for ICES Report Table 1 -------------------------------------------------
# ToDo: move after data analysis. It can be corrections (e.g. new estimated discards)

# France landings are presented this year. Metiers:
#"MIS_MIS_0_0_0"  "OTB_DEF_>=70_0_0"   "LLS_DEF"  "GTR_DEF_100-119_0_0_all"
#"GNS_DEF_100-119_0_0_all"

frLand <- sum(catTab1$Catch..kg[catTab1$Country=="France"])
tab1 <- catTab1[catTab1$Country!="France",]
# Now Pt and Sp land and discards are calculated by Fleet as defined in Table 1 Report
tab1$Fleets <- as.character(tab1$Fleets)
tabFlt <- data.frame(Fleets = sort(unique(tab1$Fleets)))
tabFlt
tabFlt$fltTab1 <- c("art", "art", "gil", "art", "art", "art", "art", "lon", "art",
                      "trw", "trw","trw-bk",  
                     "trw-cd", "trw-bk", "art", "trw-pa")
tabFlt  # Double check names!!!!!
write.csv(tabFlt, file="data/intercatch/fleetNames.csv", row.names=FALSE, quote=FALSE) # Explanation:
# the next year use this file to make the fleets name association. 

# fleets deleted and included (2025)
# "LHM_SPF_0_0_0" (appears again)
#  GTR_DEF_50-59_0_0 (new)

# fleets deleted and included (2026)
# "MIS_MIS_0_0_0_HC" (disappears)



tab1 <- merge(tab1, tabFlt)
tab1 <- tab1[,c("Country", "Catch.Cat.", "Report.cat.", "fltTab1", "Catch..kg")]
tab1.df <- data.frame(spArt=NA, spGil=NA, spLon=NA, cdTr=NA, spTr=NA, spPa=NA, spBk=NA, spDisc=NA, spBMS=NA,
                      ptArt=NA, ptTr=NA, ptDisc=NA, frLand=NA, unal=NA)
                      
xx <- aggregate(tab1$Catch..kg, by=tab1[, c("Country", "Catch.Cat.", "fltTab1", "Report.cat.")], sum)  
tab1.df$spArt <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Landings" & xx$Report.cat.=="R-Reported" & xx$fltTab1=="art"]) / 1000000
tab1.df$spGil <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Landings" & xx$Report.cat.=="R-Reported" & xx$fltTab1=="gil"]) / 1000000
tab1.df$spLon <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Landings" & xx$Report.cat.=="R-Reported" & xx$fltTab1=="lon"]) / 1000000
tab1.df$cdTr <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Landings" & xx$Report.cat.=="R-Reported" & xx$fltTab1=="trw-cd"]) / 1000000
tab1.df$spPa <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Landings" & xx$Report.cat.=="R-Reported" & xx$fltTab1=="trw-pa"]) / 1000000
tab1.df$spBk <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Landings" & xx$Report.cat.=="R-Reported" & xx$fltTab1=="trw-bk"]) / 1000000
tab1.df$spDisc <- sum(xx$x[xx$Country=="Spain" & xx$Catch.Cat=="Discards"]) / 1000000
tab1.df$ptArt <- sum(xx$x[xx$Country=="Portugal" & xx$Catch.Cat=="Landings" & xx$fltTab1=="art"]) / 1000000
tab1.df$ptTr <- sum(xx$x[xx$Country=="Portugal" & xx$Catch.Cat=="Landings" & xx$fltTab1=="trw"]) / 1000000
tab1.df$ptDisc <- sum(xx$x[xx$Country=="Portugal" & xx$Catch.Cat=="Discards"]) / 1000000
tab1.df$frLand <- frLand  / 1000000
tab1.df$unal <- sum(xx$x[xx$Country=="Spain" & xx$Report.cat.=="N-Nonreported"]) / 1000000
# CAUTION! In 2023, we needed to add the BMS landings! They were associated to OTB_MPD_>=55_0_0 and OTB_DEF_>=55_0_0 (trw-bk)
# tab1.df$spBMS <- (subset(xx,xx$Catch.Cat.=="BMS landing")$x/ 1000000)
# spBMS=(subset(xx,xx$Catch.Cat.=="BMS landing")$x)
# CAUTION! In 2024, no BMS landings available!
# CAUTION! In 2025, no BMS landings available!
unique(xx$Catch.Cat.)
subset(xx,xx$Catch.Cat.=="Logbook Registered Discard")

tab1CatnoFrance <- sum(xx$x[xx$Country != "France"])


sum(tab1.df[1,], na.rm=TRUE) # value checked with intercatch summary

write.csv(tab1.df, file="data/intercatch/tab1.csv", row.names=FALSE, quote=FALSE)
rm(list=c("tab1", "xx", "tab1.df"))

# Explanation: we have spTr=NA because now we report this value divided between
# SpPa and SpBk.


# Spanish discards analysis ----------------------------------------------------

discTab <- catTab1[catTab1$Country == "Spain" & catTab1$Catch.Cat.=="Discards",]  
# Select only Spanish Discards above

# Delete fleets not discarding
xx <- aggregate(discTab[,"Catch..kg"], by=list(discTab[, "Fleets"]), sum)
xx <- xx[xx[,2]>0,]
discFleets <- as.character(xx[,1])
rm(xx)
discTab <- discTab[discTab$Fleets%in%discFleets,]

# Estimate fleets without discards but having effort (2023 data)
#View(aggregate(discTab[,c("Catch..kg", "Effort")], by=discTab[, c("Area", "Season", "Fleets")], sum)  )

# Area Season            Fleets Catch..kg Effort
#Area Season            Fleets Catch..kg Effort
#1 27.8.c      1 GNS_DEF_80-99_0_0         0 176847
#2 27.9.a      1 GNS_DEF_80-99_0_0         0  21216
#5 27.8.c      3 GNS_DEF_80-99_0_0         0 138346
#6 27.9.a      3 GNS_DEF_80-99_0_0         0  13628

# Explanation: Discards are not important for  GNS_DEF_80-99_0_0 (gil).
# In 2024 data, no catch..kg =0 with effort==0
# In 2025 data:
# Area Season            Fleets Catch..kg Effort
# 3 27.8.c      2 GNS_DEF_80-99_0_0         0 197374
# 4 27.9.a      2 GNS_DEF_80-99_0_0         0  14016
# 7 27.8.c      4 GNS_DEF_80-99_0_0         0 206373
# 8 27.9.a      4 GNS_DEF_80-99_0_0         0   5183

# Then we can omit the section "raise discards from same métier".

# With BMS!
#spDisc <- sum(discTab$Catch..kg) +  spBMS#Value checked in the summary intercatch table
spDisc <- sum(discTab$Catch..kg) 
spDisc


# Allocation LD Spanish Discards
discTab <- merge(discTab, ldTab[ldTab$Country == "Spain" & ldTab$Catch.Cat.=="Discards",], all=TRUE)  # Select only Spanish Discards
sopCtr(xTab=discTab)[, c("Season", "Area", "Fleets", "Effort", "Catch..kg", "Caton", "sop", "soprat")]
# CHECK "Catch..kg" == "Caton" -> OK
discTab$Catch..kg - discTab$Caton
# CHECK "soprat" ~= 1 ? (min=0.7950, max = 1.0128)
sopCtr(xTab=discTab)[, "soprat"]
summary(sopCtr(xTab=discTab)[, "soprat"])

# In case there are any relevant fleet with positive effort and zero discards it must be estimated with "raise2Effort" function
# Problem: no fleet!
fltD0 <- unique(as.character(discTab$Fleets[discTab$Catch..kg==0]))
fltD0
# raise discards from same metier if it is needed
# for(flt in fltD0){
#   fltDPUE <- sum(discTab$Catch..kg[discTab$Fleets==flt]) / sum(discTab$Effort[discTab$Fleets==flt])
#   if (fltDPUE > 0 ) {
#     discTab$Catch..kg[discTab$Fleets==flt & discTab$Catch..kg==0] <- fltDPUE * 
#                   discTab$Effort[discTab$Fleets==flt & discTab$Catch..kg==0]
#   }
# }
#discTab[,1:10]  # CHECK catch..kg are all TRUE (estimated or true zero)
#sum(discTab$Catch..kg)

# Alloc discards LD
# fltD0 <- unique(as.character(discTab$Fleets[is.na(discTab$SumLd)]))
# for(flt in fltD0){
#   nLens <- sum(discTab$SumLd[discTab$Fleets==flt], na.rm = TRUE)
#   if (nLens > 0 & is.na(sum(discTab$SumLd[discTab$Fleets==flt]))) {
#     catSum <- sum(discTab$Catch..kg[discTab$Fleets==flt & !is.na(discTab$SumLd)])
#     lenVec <- apply(discTab[discTab$Fleets==flt & !is.na(discTab$SumLd), lenNames], 2, sum) / catSum
#     discTab[discTab$Fleets==flt & is.na(discTab$SumLd), lenNames] <- 
#                t(lenVec %*% t(discTab$Catch..kg[discTab$Fleets==flt & is.na(discTab$SumLd)]))
#   }
# }
# 
# sopCtr(xTab=discTab)[, c("Season", "Area", "Fleets", "Effort", "Catch..kg", "Caton", "sop", "soprat")]  # CHECK IT!!!
# 
# spDisc <- sum(discTab$Catch..kg)

# Correcting discards if the raise is carried out (2023: NO RELEVANT FLEETS WITH EFFORT BUT ZERO DISCARDS)
# tab1.df <- read.csv(file="data/intercatch/tab1.csv", header=TRUE)
# tab1.df$spDisc <- spDisc / 1000000
# write.csv(tab1.df, file="data/intercatch/tab1.csv", row.names=FALSE, quote=FALSE)

## Discard data exporting -------------------------------------------------------


# Total disc (in weight)
spDiscGad <- aggregate(discTab[,"Catch..kg"], by=discTab[,c("Year", "Season")], sum)
names(spDiscGad) <- c("year", "mon", "wei")


# To pre ss format
spDiscGad <- data.frame(year = spDiscGad$year, step = spDiscGad$mon, area = 1, 
                        fleet = "spDisc", amount = spDiscGad$wei)

sum(spDiscGad$amount) - spDisc  # must be zero. If not check procedure


## Discard Length distribution ------------------------------------------------
# LD with only trawls (without Cádiz and other fleets). 
# Cadiz LD in 20 and 21 seems to be more relevant because 
# unrealistic discards for other trawls. In 2021 is around 15%
# 2022 and 2023 the % of Cádiz increased, in 2023 is 35%
# In 2024 data, Cádiz is a 32%.
# In 2025 data, Cádiz is a 22%.

spLdDiscGad <- discTab[discTab$Fleets%in%c("OTB_DEF_>=55_0_0", "OTB_MPD_>=55_0_0", "PTB_MPD_>=55_0_0"),]

discRat <- sum(spLdDiscGad$Catch..kg)/spDisc   # ratio to raise discard LD to total discards. 

spLdDiscGad <- spLdDiscGad[spLdDiscGad$Catch..kg !=0,]  # Delete rows with non-relevant fleets with zero catch

sopCtr(xTab=spLdDiscGad)[, c("Season", "Area", "Fleets", "Effort", "Catch..kg", "Caton", "sop", "soprat")]  
spLdDiscGad <- aggregate(spLdDiscGad[, lenNames], by=spLdDiscGad[,c("Year", "Season")], sum)
spLdDiscGad <- data.frame(YEAR=rep(spLdDiscGad$Year, nLd), MES=rep(spLdDiscGad$Season, nLd), stack(spLdDiscGad[,lenNames]))
spLdDiscGad$ind <- as.numeric(substr(as.character(spLdDiscGad$ind), 5, 10)) /10
names(spLdDiscGad) <- c("year", "mon", "num", "len")

spLdDiscGad$num <- spLdDiscGad$num / discRat             # correction to raise to total discards
spLdDiscGad <- spLdDiscGad[spLdDiscGad$num>0,]
spLdDiscGad <- spLdDiscGad[order(spLdDiscGad$mon, spLdDiscGad$len),]

# To pre ss format
spLdDiscGad <- data.frame(year = spLdDiscGad$year, step = spLdDiscGad$mon , area = "area1", 
                          length = spLdDiscGad$len, number = spLdDiscGad$num, fleet = "spDisc")



# Spanish landings analysis ----------------------------------------------------

spLandTab1 <- catTab1[catTab1$Country == "Spain" & catTab1$Catch.Cat.=="Landings",]  # Select only Spanish Landings

ldTab1 <- ldTab[ldTab$Country == "Spain" & ldTab$Catch.Cat.=="Landings",]  # Select only Spanish Landings

catTab <- merge(spLandTab1, ldTab1, all=TRUE)  # just to check were catches == NA i.e. were ld == NA

catTab$Country <- as.factor(catTab$Country)

catTab[catTab$Catch..kg==0,]  # check all records have valid weight (Catch..kg > 0  or TRUE 0)
catTab <- catTab[catTab$Catch..kg>0, ]   # delete records with catch = 0
totSpLand <- sum(catTab$Catch..kg)  # save the total weight, checked with intercatch summary
catTab$Catch..kg - catTab$Caton  # MUST BE 0 or NA

catTabNA <- catTab[is.na(catTab$Caton),]  # select records without len distribution
catTabNA[, 1:10]
unique(catTabNA$Fleets)
catTab <- catTab[!is.na(catTab$Caton),]  # select records with len distribution


## SOP control
catTab <- sopCtr(catTab)
xCol <- c(1:9, dim(catTab)[2]-1, dim(catTab)[2])
sopWrongRecords <- rbind(catTab[catTab$soprat < 0.95, xCol], catTab[catTab$soprat > 1.05, xCol ])
sopWrongRecords

# In case wrong records -> SEND sopWrongRecords TO DATA PROVIDERS (SAP estimates catch with old a and b L-W params). 
# However everything is between 0.94 and 1.05


# If there are not time to check. JUST CORRECT IT HERE!!
catTab <- sopCorrection(catTab)
catTab <- sopCtr(catTab)

sopWrongRecordsXX <- rbind(catTab[catTab$soprat < 0.95, xCol],
                         catTab[catTab$soprat > 1.05, xCol])
sopWrongRecordsXX

# Join again records with ld (now corrected) and without ld 
catTabNA$sop <- NA
catTabNA$soprat <- NA

# Check LD allocation needed (Catch without LD). WARNING: THIS CAN BE DIFFERENT EACH YEAR!
 rec2Alloc <- catTabNA[catTabNA$Fleets!="MIS_MIS_0_0_0_HC" & catTabNA$Report.cat.=="R-Reported",]  # MIS and Non-reported excluded to allocate later. IMP: Why MIS_MIS_0_0_0_HC # is excluded?
 rec2Alloc[,1:10]
 dim(rec2Alloc[,1:9])

# 2023 data
# unique(catTabNA$Fleets)
# [1] "LHM_DEF_0_0_0"     "MIS_MIS_0_0_0_HC"  "PS_SPF_0_0_0"      "GNS_DEF_>=100_0_0" "LLS_DEF_0_0_0"     "GNS_DEF_80-99_0_0"

# 2024 data
# unique(catTabNA$Fleets)
# [1] "LHM_DEF_0_0_0"     "MIS_MIS_0_0_0_HC"  "GNS_DEF_>=100_0_0" "GTR_DEF_50-59_0_0" "PS_SPF_0_0_0"      "LHM_SPF_0_0_0"    
#   "GNS_DEF_80-99_0_0" 

 # New! Not with NA's in previous years!
# "GTR_DEF_50-59_0_0"
# "LHM_SPF_0_0_0" 
 
# 2025 data
 
# > unique(catTabNA$Fleets)
# [1] "LHM_DEF_0_0_0"     "LHM_SPF_0_0_0"     "GNS_DEF_>=100_0_0" "GNS_DEF_80-99_0_0" "GTR_DEF_50-59_0_0"
# [6] "PS_SPF_0_0_0"      "GTR_DEF_60-79_0_0" "LLS_DEF_0_0_0"    
# 
# New! Not with NA's in previous years!

# "GTR_DEF_60-79_0_0" 
# 1.
xAlloc <- catTab[catTab$Fleets=="GNS_DEF_>=100_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="GNS_DEF_>=100_0_0",]
xx <- allocXY(xAlloc, yAlloc, lenNames)

xx <- sopCtr(xx)
xx[, xCol]

# 2.
xAlloc <- catTab[catTab$Fleets=="GNS_DEF_80-99_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="GNS_DEF_80-99_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)

# 3. "LHM_SPF_0_0_0"  new! Can we use  LHM_DEF_0_0_0? No info!
# Other art:  GNS_DEF_60-79_0_0

xAlloc <- catTab[catTab$Fleets=="GNS_DEF_60-79_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="LHM_SPF_0_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)

# 4.
xAlloc <- catTab[catTab$Fleets=="GNS_DEF_60-79_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="LHM_DEF_0_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)

# 5. 
xAlloc <- catTab[catTab$Fleets=="GNS_DEF_60-79_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="PS_SPF_0_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)


# 6. New!
xAlloc <- catTab[catTab$Fleets=="GTR_DEF_60-79_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="GTR_DEF_50-59_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)

# 7. New!
xAlloc <- catTab[catTab$Fleets=="GTR_DEF_60-79_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="GTR_DEF_60-79_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)

# 8. New!
xAlloc <- catTab[catTab$Fleets=="LLS_DEF_0_0_0"  & catTab$Report.cat.=="R-Reported", ]
yAlloc <- rec2Alloc[rec2Alloc$Fleets=="LLS_DEF_0_0_0",]
aux <- allocXY(xAlloc, yAlloc, lenNames)

aux<- sopCtr(aux)
aux[, xCol]

xx=rbind(xx,aux)

dim(xx)
dim(catTabNA[catTabNA$Fleets!="MIS_MIS_0_0_0_HC" & catTabNA$Report.cat.=="R-Reported",])


catTabNA[catTabNA$Fleets!="MIS_MIS_0_0_0_HC" & catTabNA$Report.cat.=="R-Reported",] <- xx
sum(catTab$Catch..kg) + sum(catTabNA$Catch..kg) - totSpLand
catTab <- rbind(catTab, catTabNA)
xTab <- catTab


# Allocating Non-reported. NO NON-REPORTED IN 2023 and 2024 and 2025
catTab[, xCol]
sum(catTab$Catch..kg) - totSpLand
xx <- catTab

 # function called to do the job. Only if Nonreported are available
# xx <- allocNonRep(catTab) 
# xx[, xCol]
# sum(xx$Catch..kg) - totSpLand  # CHECK if zero is OK
# xx <- sopCtr(xx)
# rbind(xx[xx$soprat < 0.95, xCol], xx[xx$soprat > 1.05, xCol])   # NAs may hide results
# xx[, xCol]  # CHECK if all soprat are around 1 is OK

# Allocating MIS_MIS_0_0_0_HC records (landings without metier assignment) in season 1, 2, 3, 4 and VIIIc and IXa
xx <- allocMisMis(xx)  # function called to do the job
xx <- sopCtr(xx)
xx[, xCol]
sum(xx$Catch..kg) - totSpLand  # CHECK zero is OK

rbind(xx[xx$soprat < 0.95, xCol], xx[xx$soprat > 1.05, xCol])   # CHECK zero is OK
xx$Caton <- xx$Catch..kg
xx <- sopCtr(xx)  # check all around 1
xx[, xCol]

spLandTab <- xx

## Format of landings ----------------------------------------------------------

#Prepare ldTab for SS DB files
dbTab <- spLandTab
dbTab$Fleets <- as.character(dbTab$Fleets)
dbTab$Country <- as.character(dbTab$Country)
dbTab$year <- dbTab$Year

#check existing fleets 
sort(unique(dbTab$Fleets))
#  GNS_DEF_>=100_0_0" (rasco), "GTR_DEF_60-79_0_0" (trasmallo), LHM_DEF_0_0_0,  PS_SPF_0_0_0 y "MIS_MIS_0_0_0_HC"



arteDF <- data.frame(Fleets=sort(unique(dbTab$Fleets)),
                    fleet=c("Art", "Art", "volanta", "Art", "Art", "Art", "Art","palangre", 
                              "baka", "cdTrw",  "baka","Art","pairTrw"))
arteDF  # CHECK CONSISTENCY
dbTab <- merge(dbTab, arteDF)
dbTab$fleet <- as.character(dbTab$fleet)


# Origenes validos
#unique(dbTab$Area)
#head(dbTab)
#dbTab$ORIGEN <- NA
#dbTab$ORIGEN[dbTab$Area == "27.9.a"] <- "Division IXa"
#dbTab$ORIGEN[dbTab$Fleets == "OTB_MCD_>=55_0_0"] <- "Subdivision IXa-Sur"  #Unica flota del Sur
#dbTab$ORIGEN[dbTab$Area == "27.8.c"] <- "Division VIIIc"

# species
#dbTab$SPECIES <- "Merluccius merluccius"

dbTab$step <- dbTab$Season 
dbTab$area <- 1

# Spanish landings to SS
#spLandTab <- dbTab[dbTab$Country == "Spain" & dbTab$Catch.Cat. == "Landings",]
spLandGadget <- aggregate(dbTab[,"Caton"], dbTab[, c("year", "step", "area", "fleet")], sum) # aggregate otter trawls (baca and mixta)
stopifnot(sum(spLandGadget$x) - totSpLand == 0)
names(spLandGadget)[5] <- "amount"

# LFDs

spLdGadget <-  aggregate(dbTab[,lenNames], dbTab[, c("year", "step", "area", "fleet")], sum)
spLdGadget <- data.frame(year=rep(spLdGadget$year, nLd), step=rep(spLdGadget$step, nLd), area=rep(spLdGadget$area, nLd),
                         fleet= rep(spLdGadget$fleet, nLd), stack(spLdGadget[,lenNames]))

names(spLdGadget) <-  c("year", "step", "area", "fleet", "number", "length")
spLdGadget$length <- as.numeric(substr(spLdGadget$length, 5, 10)) / 10
spLdGadget <- spLdGadget[,c("year", "step", "area", "length", "number", "fleet")]
spLdGadget <- spLdGadget[spLdGadget$number != 0,]
spLdGadget$number <- round(spLdGadget$number, 2)
spLdGadget <- spLdGadget[order(spLdGadget[,"step"], spLdGadget[,"fleet"], spLdGadget[,"length"]),]

# TOTAL SOP control
sum(spLdGadget$number * a * (spLdGadget$length + 0.5) ^ b) / totSpLand 


# Pt Landings ------------------------------------------------------------------

# Select only Portuguese Landings
ptLandTab1 <- catTab1[catTab1$Country == "Portugal" & catTab1$Catch.Cat.=="Landings",]  
# Select only Pt Landings
ptldTab1 <- ldTab[ldTab$Country == "Portugal" & ldTab$Catch.Cat.=="Landings",]  

ptLandTab <- merge(ptLandTab1, ptldTab1, all=TRUE)  
# just to check were catches == NA i.e. were ld == NA

ptLandTab$Country <- as.factor(ptLandTab$Country)

ptLandTab[ptLandTab$Catch..Kg==0]  # check all records have weight
ptLandTab[is.na(ptLandTab$Catch..Kg)]
totPtLand <- sum(ptLandTab$Catch..kg)  # save the total weight
ptLandTab$Catch..kg - ptLandTab$Caton  # MUST BE 0 or NA
ptLandTab[, 1:10]

# SOP control
ptLandTab <- sopCtr(ptLandTab)
xCol <- c(1:9, dim(ptLandTab)[2]-1, dim(ptLandTab)[2])
sopWrongRecords <- rbind(ptLandTab[ptLandTab$soprat < 0.99, xCol], ptLandTab[ptLandTab$soprat > 1.01, xCol ])
sopWrongRecords

#ptLandTab <- sopCorrection(ptLandTab)
#ptLandTab <- sopCtr(ptLandTab)

#sopWrongRecordsXX <- rbind(ptLandTab[ptLandTab$soprat < 0.95, xCol],
#                           ptLandTab[ptLandTab$soprat > 1.05, xCol])
#sopWrongRecordsXX
#ptLandTab[, xCol]

# Format of landings ---------------------------------------------------------
dbTab <- ptLandTab
dbTab$Fleets <- as.character(dbTab$Fleets)
dbTab$Country <- as.character(dbTab$Country)
dbTab$year <- datYr

# Change fleet names
arteDF <- data.frame(Fleets=sort(unique(dbTab$Fleets)), fleet=c("ptArt", "ptTrw"))
arteDF  # CHECK IT!!!!
dbTab <- merge(dbTab, arteDF)
dbTab$fleet <- as.character(dbTab$fleet)

dbTab$area <- 1
dbTab$step <- dbTab$Season  

ptLandGadget <- aggregate(dbTab[,"Caton"], dbTab[, c("year", "step", "area", "fleet")], sum) 
names(ptLandGadget) <- c("year", "step", "area", "fleet", "amount")
stopifnot(sum(ptLandGadget$amount) - totPtLand == 0)


# LFDs ------------------------------------------------------------------------
ptLdGadget <-  aggregate(dbTab[,lenNames], dbTab[, c("year", "step", "area", "fleet")], sum)
ptLdGadget <- data.frame(year=rep(ptLdGadget$year, nLd), step=rep(ptLdGadget$step, nLd), area=rep(ptLdGadget$area, nLd),
                         fleet= rep(ptLdGadget$fleet, nLd), stack(ptLdGadget[,lenNames]))
names(ptLdGadget) <-  c("year", "step", "area", "fleet", "number", "length")
ptLdGadget$area <- "area1"
ptLdGadget$length <- as.numeric(substr(ptLdGadget$length, 5, 10)) / 10
ptLdGadget <- ptLdGadget[,c("year", "step", "area", "length", "number", "fleet")]
ptLdGadget <- ptLdGadget[ptLdGadget$number != 0,]
ptLdGadget$number <- round(ptLdGadget$number, 2)
ptLdGadget <- ptLdGadget[order(ptLdGadget[,"step"], ptLdGadget[,"fleet"], ptLdGadget[,"length"]),]

# Total SOP control
sum(ptLdGadget$number * a * (ptLdGadget$length + 0.5) ^ b) / totPtLand


# Portuguese discards analysis--------------------------------------------------

ptDiscTab <- catTab1[catTab1$Country == "Portugal" & catTab1$Catch.Cat.=="Discards",]  # Select only Pt Discards

# Delete fleets without discards
xx <- aggregate(ptDiscTab[,"Catch..kg"], by=list(ptDiscTab[, "Fleets"]), sum)
xx <- xx[xx[,2]>0,]
discFleets <- as.character(xx[,1])
rm(xx)
ptDiscTab <- ptDiscTab[ptDiscTab$Fleets%in%discFleets,]

# Estimate fleets without data but having discards
aggregate(ptDiscTab[,c("Catch..kg", "Effort")], by=ptDiscTab[, c("Area", "Season", "Fleets")], sum)  #Check: 
# OK WITH 2022 DATA
# Ok with 2023 data

ptDisc <- sum(ptDiscTab$Catch..kg)

## Allocation LD Pt Discards
ptDiscTab <- merge(ptDiscTab, ldTab[ldTab$Country == "Portugal" & ldTab$Catch.Cat.=="Discards",], all=TRUE)  # Select only Pt disc
ptDiscTab <- sopCtr(xTab=ptDiscTab)
# CHECK "Catch..kg" == "Caton" -> OK
# CHECK "soprat" ~= 1 -> NO

sopWrongRecords <- rbind(ptDiscTab[ptDiscTab$soprat < 0.99, xCol], ptDiscTab[ptDiscTab$soprat > 1.01, xCol ])
sopWrongRecords

ptDiscTab <- sopCorrection(ptDiscTab)
ptDiscTab <- sopCtr(ptDiscTab)

sopWrongRecordsXX <- rbind(ptDiscTab[ptDiscTab$soprat < 0.95, xCol],
                           ptDiscTab[ptDiscTab$soprat > 1.05, xCol])
sopWrongRecordsXX
ptDiscTab[, xCol]

ptDiscGad <- aggregate(ptDiscTab[,"Catch..kg"], by=ptDiscTab[,c("Year", "Season")], sum)
ptDiscGad$area <- 1
ptDiscGad$fleet <- "ptDisc"
names(ptDiscGad) <- c("year", "step", "amount", "area", "fleet")
ptDiscGad <- ptDiscGad[, c("year", "step", "area", "fleet", "amount")]


#length distribution
ptLdDiscGad <- ptDiscTab
ptLdDiscGad <- aggregate(ptLdDiscGad[, lenNames], by=ptLdDiscGad[,c("Year", "Season")], sum)
ptLdDiscGad <- data.frame(year=rep(ptLdDiscGad$Year, nLd), step=rep(ptLdDiscGad$Season, nLd), stack(ptLdDiscGad[,lenNames]))
ptLdDiscGad$fleet <- "ptDisc"
ptLdDiscGad$area <- "area1"
ptLdDiscGad$ind <- as.numeric(substr(as.character(ptLdDiscGad$ind), 5, 10)) /10
names(ptLdDiscGad) <- c("year", "step", "number", "length", "fleet", "area")
ptLdDiscGad <- ptLdDiscGad[, c("year", "step", "area", "length", "number", "fleet")]
ptLdDiscGad <- ptLdDiscGad[ptLdDiscGad$num>0,]
ptLdDiscGad <- ptLdDiscGad[order(ptLdDiscGad$step, ptLdDiscGad$length),]





# Final combination ---------------------------------------------

# Catch file
frLand <- catTab1[catTab1$Country == "France" & catTab1$Catch.Cat. == "Landings", 
                  c("Year", "Season", "Fleets", "Catch..kg")]
frLand <- aggregate(frLand[,"Catch..kg"], by=frLand[,c("Year", "Season")], sum)
frLand <- data.frame(year = frLand$Year, step = frLand$Season, area = 1, fleet = "frArt", amount = frLand$x)
frLand

# BMS landing

bms<-catTab1[catTab1$Catch.Cat. == "BMS landing", 
                     c("Year", "Season", "Fleets", "Catch..kg")];bms
#bms<-aggregate(bms[,"Catch..kg"], by=bms[,c("Year", "Season")], sum)

# Add frLand to ART in spLandGadget

ind=which(spLandGadget$fleet=="Art")
spLandGadget[ind,]$amount=spLandGadget[ind,]$amount+frLand$amount

# Add BMS landing

#spDiscGad$amount=spDiscGad$amount+c(bms$x,0)

catchFile <- rbind(spLandGadget,
                ptLandGadget,
                frLand,
                spDiscGad
                ,
                ptDiscGad
                )

sum(catchFile$amount) # Same as Table1 total in 2024, care that frland is twice, in is own slot and in artisanal


ldFile <- rbind(spLdGadget,
                ptLdGadget,
                spLdDiscGad
                ,
                ptLdDiscGad
                )

write.csv(catchFile, file="data/catch/catchData25.csv", row.names=FALSE, quote=FALSE)
write.csv(ldFile, file="data/LFDs/lenDistData25.csv", row.names=FALSE, quote=FALSE)

