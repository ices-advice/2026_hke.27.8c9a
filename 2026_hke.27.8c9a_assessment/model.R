## Run model (including retrospective analysys and forecast)

## Before: SS files
## After: Model run, retrospective analysis run, forecast analysis run.

library(icesTAF)
library(r4ss)


library(r4ss)
library(ss3diags)
library(readr)
library(plyr)
library(reshape)
library(tidyverse)
library(parallel)
library(doParallel)
library(icesAdvice)
library(openxlsx)
library(writexl)


mkdir("model")



# Run model -------------------------------------------------------------------
# Note that the ss.par model was previously obtained by running the jitter process on a supercomputer due to the time consuming nature of the computation.
# Now we just need to run the hessian from this ss.par.

source('mod 1 run model.R')

# Run the retrospective analysis -----------------------------------------------

# Note that for each retro model the ss.par file was obtained in a jitter process
# carried out previously in a super computer. Now, from such ss.par and the basic
# model files we obtained the remaining outputs for each model retro.

source('mod 2 retro.R')


# Run the forecast procedure ----------------------------------------------------

source('mod 3 forecast.R')

# Run PBlim --------------------------------------------------------------------

source('mod 4 forecast PBlim.R')