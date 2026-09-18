
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run SS shake model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 25/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
# Marta Cousido Rocha #
# Santiago Cervino    #
#~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline


rm(list=ls()) 
library(r4ss)
original<-getwd()

# Copy the files to model/final --------------------------------------------

# Copy the "control_fixed.ss", "starter.ss", "forecast.ss" files
# Also the ss.par that as explained in the data.bib comes from a jittering due to 
# convergence problems.
source_dir <- file.path(getwd(), "boot", "data", "SS files template")
files_to_copy <- c("control_fixed.ss", "starter.ss", "forecast.ss", "ss.par")

dest_dir <- file.path(getwd(), "model", "final")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

for (file in files_to_copy) {
  file.copy(file.path(source_dir, file), file.path(dest_dir, file))
}

# Copy the SS data file generated using data.R
source_file <- file.path(getwd(), "data", "ss files", "shake_data.ss")
dest_dir <- file.path(getwd(), "model", "final")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(source_file, dest_dir)

# Copy also the executable 

source_exe <- file.path(getwd(), "boot", "software", "ss.exe")
file.copy(source_exe, dest_dir)

# Modify control! (check every year)--------------------------------------------
ctlfile<-"control_fixed.ss"
ctl <- readLines(file.path(dest_dir, ctlfile))
# RW Rec
ind<-grep("RecrDist_GP_1_area_1", ctl)
aux<-ctl[ind[2]] 
aux<-gsub("2024", "2025", aux) ### CHECK next year!

ctl[ind[2]] <- aux


# main recr devs

ind<-grep("last year of main recr_devs", ctl)
aux<-ctl[ind] 
aux<-gsub("2024", "2025", aux) ### CHECK next year!

ctl[ind] <- aux

# recr devs ramp

ind<-grep("end_yr_for_ramp", ctl)
aux<-ctl[ind] 
aux<-gsub("2025", "2026", aux) ### CHECK next year!

ctl[ind] <- aux

file.remove(file.path(dest_dir, ctlfile))
writeLines(ctl, file.path(dest_dir, ctlfile))


# Run --------------------------------------------------------------------------

#r4ss::run_SS_models(dirvec = mod_path, model = "ss", exe_in_path = TRUE,
#                    verbose=TRUE,extras="-nohess") ## "-nox" or "-nohess" 


# Run the model conditioned on the ss.par that was obtained from the jittering process.
# The jittering process was performed externally on a supercomputer.

# Jitter frac to 0 to use exactly the ss.par values

starter <- SS_readstarter(file.path(dest_dir, 'starter.ss'))

starter$jitter_fraction = 0

SS_writestarter(starter, dir=dest_dir, overwrite=TRUE)

# Run model

command <- "ss -maxfn 0 -phase 99"

setwd(dest_dir)
system(paste('ss.exe', command), intern = TRUE,invisible = FALSE)

setwd(original)
