## Extract results of interest, write TAF output tables

## Before: Model output files
## After: To understand the model outputs, additional plots to the report plots generated in the report.R file.
##        r4ss plots
##        Landings and discards estimates plots (obs vs exp),
##        biomass indices plot (residuals and obs vs exp),
##        plot of SSB with fish <90cm,
##        LFDs plots (including residuals bubble plots)
##       Also ss3 diags plots.



library(icesTAF)
library(ggridges)
library(r4ss)
library(tidyverse)
library(icesAdvice)
library(dplyr)
library(plyr)
library(ss3diags)
library(conflicted)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(ss3diags::sspar)
mkdir("output")

# Additional model plots------------------------------------------------------

# In this script we generate a series of extra plots to understand the behaviour of the model.
# Also the r4ss plots in html.

source('output 01 model plots.R')


# SS3 diags plots ---------------------------------------------------------------
#Index residuals
#Mean length residuals
#Join residuals (mean length and index)
#Kobe plot 
#Retro analysis index
#Retro analysis mean length

source('output 02 ss3 diags.R')
