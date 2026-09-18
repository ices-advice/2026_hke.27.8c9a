## Preprocess data and creation of ss data file

## Before: Catch, length frequency and biomass indices information in boot/data
## After: The shake_data.ss file required for running the SS model

library(icesTAF)
library(readODS)
library(gridExtra)
library(ggplot2)
library(grid)

library(plyr)
library(patchwork)
library(dplyr)
library(readr)
library(openxlsx)

library(readxl)
library(reshape2)
library(r4ss)
library(tidyverse) 
library(data.table)

library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(reshape2::melt)
conflicts_prefer(dplyr::arrange)

mkdir("data")

# Combine CPUEs --------------------------------------------------------------
# Combine the CPUEs of the three different Spanish trawl métiers, targeting 
# medium size fish, in one CPUE index termed SpTrawl.
# Also combines the gillnetters and longliners CPUEs in one CPUE index termed SpVolpal.

source('dat 01 combine CPUEs.R')


# Intercatch data review and formating -----------------------------------------

# This file checks the catch and length data from Intercatch and starts the formatting process.

source('dat 02 intercatch.R')

# Nsample generation -----------------------------------------------------------

# In this script the NSample, value used in SS in order to give weights to the LFDs by fleet and Year,
# is computed.


source('dat 03 nsample.R')

# Reshape data ----------------------------------------------------------------

#This script reshapes the data to achieve the required format for later inclusion 
#in the ss data file in the next script.

source('dat 04 arrange all data.R')

# Create the SS data file ------------------------------------------------------
source('dat 05 input datafile.R')

# Tables and Plots -----------------------------------------------------------------------

# Table and plot of the biomass indices of the different surveys.
source('dat 06 add plots.R')
