## Prepare plots and tables for report

## Before: Model output files, and retros and forecast output
## After: Tables and plots in the report
library(xml2)
library(icesTAF)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(conflicted)
library(dplyr)
library(plyr)
library(r4ss) 
library(icesAdvice)
library(gridExtra)
library(grid)
library(openxlsx)
library(writexl)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(reshape::melt)
mkdir("report")

# Model plots ------------------------------------------------------------------
# Total landings and discards (observed and estimated) plot, Survey pearson residuals plot
# and LFDs plots of commercial and surveys fleets.

source('report 01 model plots.R')


# Moving plots ------------------------------------------------------------------
#  Moving figures in data and output folders to the report/plots folder 

source('report 02 move plots.R')

# Retrospective analysis plots -------------------------------------------------

source('report 03 retros.R')

# Forecast plots ---------------------------------------------------------------
# Report Figure 10.11
source('report 04 forecast.R')

# Report tables ----------------------------------------------------------------

# Tab 10.2. Length distributions 
# Tab10.5 CPUE
# Tab10.6 Assessment summary

source('report 05 tables.R')


# Report summary ICES standard graphs -----------------------------------------
# This year we have not been able to use this code because new information has
# been added in SAG, such as retrospective pattern data. If R functions are created
# to allow this information to be incorporated, we will update this code for use. 
# For the time being, however, the information has been uploaded manually, so please
# do not run this script.
source('report 06 StandardGraphs.R')

# Jitter likelihood plot -----------------------------------------------------

source('report 07 jitter plot.R')
