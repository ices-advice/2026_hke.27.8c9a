#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run retrospective shake analysis #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 26/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido Rocha #
# Anxo Paz            #
# Santiago Cervino    #
#~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

rm(list=ls())
assessment_year<-2025
library(r4ss)
library(icesTAF)

# Create retrospective data files ---------------------------------------------
# Create model directory
original<-getwd()
run<-"model/final"
mod_path <- paste0(getwd(), "/", run, sep="") 

# Read the code to create the files
source(file.path(taf.boot.path("software"),"retro_create_files_function.R"), echo=TRUE)

# Create the folders and data files 
yper=0:-5 

retro_create_files(dir=mod_path, oldsubdir="", newsubdir = "retros", extras="-nohess",
       subdirstart = "retro",years = yper, overwrite = TRUE, exe = "ss")
  
# Change in control for retro!

dirs<-paste0(mod_path,"/retros","/retro-",1:5)

for (i in 1:5){
ctlfile<-"control_fixed.ss"
ctl <- readLines(file.path(dirs[i], ctlfile))
# RW Rec
ind<-grep("RecrDist_GP_1_area_1", ctl)
aux<-ctl[ind[2]] 
aux<-gsub(assessment_year, assessment_year-i, aux) ### CHECK next year!

ctl[ind[2]] <- aux


# recr devs ramp

ind<-grep("end_yr_for_ramp", ctl)
aux<-ctl[ind] 
aux<-gsub(assessment_year+1, assessment_year+1-i, aux) ### CHECK next year!

ctl[ind] <- aux

file.remove(file.path(dirs[i], ctlfile))
writeLines(ctl, file.path(dirs[i], ctlfile))
}

# Complete folder retro0 with all the model info-------------------------------

dir_list<- paste0(mod_path, "/", "retros", "/", "retro", yper, sep="") 

dir0<-dir_list[1]

dir_list<-dir_list[-1]

files <- list.files(mod_path)


for (file in files) {
  source <- file.path(mod_path, file)
  destination <- file.path(dir0, file)
  file.copy(source, destination, overwrite = TRUE)
}

# Move the ss.par of each retro to its folder ----------------------------------
# The ss.par comes from the jittering process for each retro carried out in a super computer.

dir_ss_par<-paste0(getwd(), "/", "boot/data/ss.par files retros", sep="") 

ss_par_files_names<-paste0("ss",yper[-1],".par")


for (i in 1:length(ss_par_files_names)) {
  origen <- paste0(dir_ss_par, "/",ss_par_files_names[i])
  destino <- paste0(dir_list[i], "/",ss_par_files_names[i])
  file.copy(origen, destino)
  file.rename(destino, file.path(dir_list[i], "ss.par"))
}



# Modify starter to run the model based on the ss.par from jitter in each retro------

for (i in 1:(length(yper)-1)){


starter <- SS_readstarter(file.path(dir_list[i], 'starter.ss'))

starter$jitter_fraction = 0
starter$init_values_src=1

SS_writestarter(starter, dir=dir_list[i], overwrite=TRUE)

# Run retro model -------------------------------------------------------------

command <- "ss -maxfn 0 -phase 99 -nohess"
setwd(dir_list[i])
system(paste('ss.exe', command), intern = TRUE)

}
setwd(original)