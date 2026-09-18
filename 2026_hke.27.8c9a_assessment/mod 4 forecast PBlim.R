#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run SS shake model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 08/06/2025 #
#~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido Rocha #
# Anxo Paz            #
#~~~~~~~~~~~~~~~~~~~~~~

# Provisional code for this year; next year it will be integrated with Mod 3 script
# to create a more streamlined version

# Packages ---------------------------------------------------------------------
library(readxl)
library(writexl)
library(readr)
library(r4ss)
library(dplyr)
library(icesAdvice)

# Set variables (review next year)----------------------------------------------
year_inter<-2026
Blim=6011
Bpa=7556	
Flim=0.694	
Fpa=0.558	
Fmsy=0.221
FmsyLower=0.151	
FmsyUpper=0.311	
Bmsy=50878

# Set directories --------------------------------------------------------------
run="model/final"
mod_path=paste0(paste0(getwd(), "/", sep="") , run, sep="")
fore_path <-  paste0(mod_path,"/Forecast")
tabledir_fore<- paste(fore_path, "/table", sep="")

# Read forecast info from mod 3 script -----------------------------------------
Table10_7_b_extended <- read_excel(paste0(tabledir_fore,"/Table10.7 b extended.xlsx"))
load(paste0(fore_path,"/Fmult_names.RData"))
table_Fmult <- read_csv(paste0(tabledir_fore,"/table Fmult.csv"))

# Identify which models need to run again with hessian -------------------------
ind<-which(is.na(Table10_7_b_extended$...8)==FALSE)

aux<-Table10_7_b_extended[ind,]
Models<-aux$Fmult
Fmult<-substr(Fmult_names, 6, 30)

x<-Models
y_sorted<-as.numeric(Fmult)

# Function to find interval indices
find_interval <- function(val, ref) {
  pos <- which(ref <= val)
  if (length(pos) == 0 || max(pos) == length(ref)) {
    return(c(NA, NA))  # outside the range
  } else {
    lower <- max(pos)
    upper <- lower + 1
    return(c(lower, upper))
  }
}

# Apply the function to each value in x
intervals <- t(sapply(x, find_interval, ref = y_sorted))
colnames(intervals) <- c("lower_index", "upper_index")
rownames(intervals) <- paste("x =", x)

Models_to_run<-unique(c(intervals[,1], intervals[,2]))

tacs<-Fmult_names[sort(Models_to_run)]

# Run selected models ---------------------------------------------------------
# This section is commented out because it runs on the CESGA supercomputer.
# If you prefer to run it locally, uncomment this section and comment out the next one.
# for (i in 1:length(tacs)){
# dir.tacN<- file.path(paste0(fore_path,"/",tacs[i]))
#   
# starter.file <- readLines(paste(dir.tacN, "/starter.ss", sep=""))
# linen <- NULL
# linen <- grep("Turn off estimation for parameters entering after this phase", starter.file)
# starter.file[linen] <- paste0("10 # Turn off estimation for parameters entering after this phase") # tells it to use the estimate parameters
# 
# write(starter.file, paste(dir.tacN, "/starter.ss", sep=""))
# command <- "ss -maxfn 0 -phase 99"
# 
# setwd(dir.tacN)
# print(format(Sys.time(), "%H:%M:%S"))
# system(paste('ss.exe', command), intern = TRUE,invisible = FALSE)
# 
# 
# setwd(mod_path)
# } 
  
# Prepare folder to run in supercomputer-CESGA ---------------------------------
 for (i in 1:length(tacs)){
 dir.tacN<- file.path(paste0(fore_path,"/",tacs[i]))

 dir.cesga<-file.path(paste0(fore_path,"/","CESGA_Hessian/",i))
 dir.create(dir.cesga,showWarnings = T, recursive = T)

 file.copy(paste(dir.tacN, "starter.ss", sep="/"),
  paste(dir.cesga, "starter.ss", sep="/"))
  starter.file <- readLines(paste(dir.cesga, "/starter.ss", sep=""))
  linen <- NULL
  linen <- grep("Turn off estimation for parameters entering after this phase", starter.file)
  starter.file[linen] <- paste0("10 # Turn off estimation for parameters entering after this phase") # tells it to use the estimate parameters
  write(starter.file, paste(dir.cesga, "/starter.ss", sep=""))
  
 file.copy(paste(dir.tacN, "control_fixed.ss", sep="/"),
           paste(dir.cesga, "control_fixed.ss", sep="/"))


 file.copy(paste(dir.tacN, "shake_data.ss", sep="/"),
           paste(dir.cesga, "shake_data.ss", sep="/"))

 file.copy(paste(dir.tacN, "ss.par", sep="/"),
           paste(dir.cesga, "ss.par", sep="/"))

 file.copy(paste(dir.tacN, "ss.exe", sep="/"),
           paste(dir.cesga, "ss.exe", sep="/"))

 file.copy(paste(dir.tacN, "forecast.ss", sep="/"),
           paste(dir.cesga, "forecast.ss", sep="/"))

 }
# The model was run on the supercomputer, and the results were pasted into the corresponding CESGA folder.
# If you are running it locally, you can also delete the following section.
# Load the model outputs and move CESGA results to corresponding local folder!--


 for (i in 1:length(tacs)){
   dir.tacN<- file.path(paste0(fore_path,"/",tacs[i]))

  dir.cesga<-file.path(paste0(fore_path,"/","CESGA_Hessian/",i))


   file.copy(paste(dir.cesga, list.files(dir.cesga), sep="/"),
             dir.tacN,overwrite = TRUE)

 }
 unlink(file.path(paste0(fore_path,"/","CESGA_Hessian")), recursive = TRUE)

retroModels <- SSgetoutput(dirvec=file.path(fore_path,tacs))

forecastSummary <- SSsummarize(retroModels)

# Compute prob and carried out interpolation!-----------------------------------

corresp<-data.frame(scenario=Table10_7_b_extended[ind,]$...8, 
                    lower=Fmult_names[intervals[,1]],
                    upper=Fmult_names[intervals[,2]])

table_Fmult<-as.data.frame(table_Fmult)

table_Fmult$Pblim<-NA

vec_or<-sort(Models_to_run)


# Compute PBlim 

pblim<-function(SSB, SSB_sigma,Blim){
  
  logSSBly.mu    <- log(SSB^2 / sqrt(SSB^2 + SSB_sigma^2))
  logSSBly.sigma <- sqrt(log(1 + SSB_sigma^2/SSB^2))
  pBlim <- plnorm(Blim, mean = logSSBly.mu, sd = logSSBly.sigma)
  
}


for (i in 1:length(tacs)){

  SSB_sd   <- (as.data.frame(forecastSummary[["SpawnBioSD"]]))
  ind<-dim(SSB_sd)[2]
  SSB_sd<-SSB_sd[,c(i,ind)]
  
  SSBly.sd <-( SSB_sd %>% filter(Yr == year_inter+2) %>% select( -Yr) %>% unlist())
  SSB_sd <- as.numeric(SSBly.sd[1])
  
  SSB<-table_Fmult[vec_or[i],2]
  
  p_blim<-pblim(SSB, SSB_sd,Blim); table_Fmult[vec_or[i],8]<-p_blim
}


# Interpolation ! ---------------------------------------------------------------

# Repeat mod 3 code with new column Pblim 

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

stTab<-table_Fmult;stTab_int=read.csv( paste(tabledir_fore, "/table intermediate year.csv", sep=""))[,-1]
ssbYr <- year_inter + 2
cotYr <- year_inter + 1

basis <- "MSY approach = FMSY"
df1 <-interpolateStTab(colName=paste("F",year_inter+1,sep=""), 
                       varVal =Fmsy, rowName=basis, stTab)

xx <- stTab[1,]
rownames(xx) <- "F = 0"
df1 <- rbind(df1, xx)
basis <- paste("SSB (", ssbYr, ") = Blim", sep="")
df1 <- rbind(df1, interpolateStTab(colName=paste("SSB",year_inter+2,sep=""), Blim, basis, stTab))
basis <- paste("SSB (", ssbYr, ") = Bpa = MSY Btrg", sep="")
df1 <- rbind(df1, interpolateStTab(colName=paste("SSB",year_inter+2,sep=""), Bpa, basis, stTab))

#df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), Flim, "F = Flim", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), Fpa, "F = Fpa", stTab))

SSBint=stTab_int[,1]

Fint=stTab_int[,2]
df1 <- rbind(df1, interpolateStTab(colName=paste("SSB",year_inter+2,sep=""), SSBint, "SSB (2028) = SSB(2027)", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), Fint, "F = F2026", stTab))

catches_fmsy=df1[1,4]
TAC=17445

df1 <- rbind(df1,interpolateStTab(colName=paste("F",year_inter+1,sep=""), varVal =Fmsy, rowName="EU MAP: Fmsy", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), FmsyLower, "F = MAP FMSY lower", stTab))
df1 <- rbind(df1, interpolateStTab(colName=paste("F",year_inter+1,sep=""), FmsyUpper, "F = MAP FMSY upper", stTab))
df1 <- rbind(df1, interpolateStTab(colName= paste("Catches",year_inter+1,sep=""), TAC, "equal TAC", stTab))
colnames(df1)=c("Fmult",colnames(df1)[2:ncol(df1)])
df2=df1[,c(1,3,4,5,6,2,7,8)]
df3<-df2[c(8, 9, 10, 2, 5, 3, 4, 6,7,11),]
df3$Pblim<-  icesRound(as.numeric(df3$Pblim))
View(df3)

write_xlsx(df3, paste(tabledir_fore, "/Table Pblim.xlsx", sep=""))
