################################################################################
#  WGBIE southern hake - Standard Graphs table                                 #
#------------------------------------------------------------------------------#
#   created by: Sonia Sanchez (AZTI-Tecnalia) for nothern hake                 #
#             09/05/2023                                                       #
#   modified: Marta Cousido for southern hake                                  #
################################################################################

# filling Standard Graphs table and saving it in ICES repository


# Copyright: AZTI, 2023
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

# Based on code from Hans Gerritsen

# IMPORTANT:
# This year we have not been able to use this code because new information has
# been added in SAG, such as retrospective pattern data. If R functions are created
# to allow this information to be incorporated, we will update this code for use. 
# For the time being, however, the information has been uploaded manually, so please
# do not run this script.

rm(list = ls())

dtyr <- 2025

#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

library(icesSAG)
library(openxlsx)

#==============================================================================
# TOKEN                                                                    ----
#==============================================================================

# You can generate a token like this:
# first log in on  https://standardgraphs.ices.dk/manage
# then go to sg.ices.dk/manage/CreateToken.aspx
# paste the token below after SG_PAT=

cat("# Standard Graphs personal access token",
    "SG_PAT=f115dedb-76aa-4dd8-b6bb-d69c45e78ad8", # replace with your own token
    sep = "\n",
    file = "~/.Renviron_SG")
options(icesSAG.use_token = TRUE)



#==============================================================================
# DATA                                                                     ----
#==============================================================================

stockcode <- 'hke.27.8c9a'

sumTab <- read.xlsx(file.path(getwd(),"report","table","Tab10.6.xlsx"))


# STOCK INFORMATION
Blim=6011
Bpa=7556	
Flim=0.694	
Fpa=0.558	
Fmsy=0.221
FmsyLower=0.151	
FmsyUpper=0.311	
Bmsy=50878

# icesSAG:::validNames("stockInfo")


info <- stockInfo( StockCode = 'hke.27.8c9a',           # grep("hke", icesVocab::getCodeList("ICES_StockCode")$Key, value = TRUE)
                   AssessmentYear = dtyr+1, 
                   ContactPerson = 'marta.cousido@ieo.csic.es', 
                   StockCategory = 1.0, 
                   ModelType = 'AL',                         # icesVocab::getCodeList("AssessmentModelType") 
                   ModelName = 'SS3',  
                   BMSY= 50878.5,# icesVocab::getCodeList("AssessmentModelName") 
                   FMGT_lower = FmsyLower, FMGT = Fmsy, FMGT_upper = FmsyUpper, 
                   MSYBtrigger = Bpa, FMSY =Fmsy, 
                   Blim = Blim, Bpa = Bpa, 
                   Flim = NA, Fpa = Fpa, 
                   Fage='1-7', RecruitmentAge = 0, 
                   CatchesLandingsUnits = 't', 
                   ConfidenceIntervalDefinition = '90%', 
                   RecruitmentUnits = 'NE3', 
                   StockSizeUnits = 't', 
                   StockSizeDescription = 'Female-only SSB', # icesVocab::getCodeList("StockSizeIndicator") 
                   Purpose = 'Advice'
                  )

info$StockCategory <- 1 

# icesSAG:::validNames("stockFishdata")
sumTab<-subset(sumTab,sumTab$years>=1982)

# Delete the 2020 discard value because it is an estimate
ind<-which(sumTab$discards==684)
dis<-sumTab$discards
dis[ind]<-NA

fishdata <- stockFishdata( Year = sumTab$years, 
                           Recruitment = sumTab$rec_value, Low_Recruitment = sumTab$rec_low, High_Recruitment = sumTab$rec_upp, 
                           TBiomass = sumTab$Bio, Low_TBiomass = NA, High_TBiomass = NA, 
                           StockSize = sumTab$ssb_val, Low_StockSize = sumTab$ssb_low, High_StockSize = sumTab$ssb_upp,
                           # Catches = sumTab$CatObs, Landings = sumTab$LanObs, Discards = sumTab$DisObs, # observed
                           Landings = sumTab$landings, Discards = dis, # observed landings and estimated discards
                           # Catches = sumTab$CatEst, Landings = sumTab$LanEst, Discards = sumTab$DisEst, # estimated by the model
                           FishingPressure = sumTab$F_val, Low_FishingPressure = sumTab$F_low, High_FishingPressure = sumTab$F_upp
                          )


#==============================================================================
# UPLOAD                                                                   ----
#==============================================================================

# Some problems this year with the uploadStock function then we use createSAGxml
# and uploaded it manually
#key <- icesSAG::uploadStock(info, fishdata)


output<-createSAGxml(info, fishdata)
writeLines(output, "report/plots/hke8c9a_data.xml")
