#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots for SS shake model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 25/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Massimiliano Cardinale #
# Francisco Izquierdo    #
# Marta Cousido          #
# Santiago Cervino       #
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline


## The code provides: Landings and discards estimates plots (obs vs exp),
##        biomass indices plot (residuals and obs vs exp),
##        plot of SSB with fish <90cm,
##        LFDs plots (including residuals bubble plots)



# Load packages!

rm(list=ls()) ## Clean environment
library(icesTAF)
library(ggridges)
library(r4ss)
library(tidyverse)
library(icesAdvice)
library(dplyr)
library(plyr)
library(conflicted)
conflict_prefer("summarize", "plyr")

out_dir<-file.path(getwd(), "output")
# r4ss model plots --------------------------------------------------------------------

dest_dir <- file.path(getwd(), "model", "final")
## Read output
replist <- SS_output(dir = dest_dir, verbose=TRUE, printstats=TRUE) ## read

## Plot
SS_plots(replist, pdf=F, png=T, html=T, printfolder = "r4ss plots",dir=out_dir) ## html output
#SS_plots(replist, pdf=T, png=F, html=F, printfolder = "r4ss plots",dir=out_dir) ## pdf output

## Summary plot
png(file=paste(out_dir,"/a_summaryplot.png",sep=""), width = 1200, height = 800,pointsize=18)
par(mfrow=c(3,3))
SSplotCatch(replist, subplots=10); title("Landings")
SSplotSummaryF(replist); title("F")
SSplotBiology(replist,subplots = 1)
SSplotTimeseries(replist, subplot = 14, minyr = 1982); title("Recruits")
SSplotRecdevs(replist, subplots=1);title("recdevs")
SSplotBiology(replist,subplots = 6); title("Mat")
SSplotSelex(replist, subplot = 1)
SSplotTimeseries(replist, subplot = 7); title("Biomass")
SSplotTimeseries(replist, subplot = 7, minyr = 1982,maxyr = 2026); title("Biomass recent")
dev.off()

## Typical summary tables
replist$likelihoods_used 
replist$RunTime 
replist$likelihoods_by_fleet
write.csv(replist$likelihoods_by_fleet, file=paste0(out_dir, "/likelihood-fleets.csv", sep=""))




# Generate additional plots from SS shake model ------------------------

## Model path
run <- 'model/final' ## *CHANGE name
mod_path <- paste0(getwd(), "/",run, sep="") 

## plots folder
plotdir<-paste0(getwd(), "/","output/additional plots", sep="") 
mkdir(plotdir)
## Read output
output <- replist

output$nforecastyears <- ifelse(is.na(output$nforecastyears), 0, 
                                output$nforecastyears)

forecast <- T
nyearsaveragesel     <- 3
dtyr <- output$endyr
years     <- output$startyr:dtyr
yearsfore <- output$startyr:(dtyr+output$N_forecast_yrs) 
nyears    <- length(years)

fltnms <- setNames(output$definitions$Fleet_name,1:9)

## Model datafile and starter
start <- r4ss::SS_readstarter(file = file.path(mod_path, "starter.ss"), 
                              verbose = FALSE)
ss3Dat <- r4ss::SS_readdat(file = file.path(mod_path, start$datfile),
                           verbose = FALSE)

## Catch ------------------------------------------------------------------------

## Prepare objects and plot observed and Fitted landings & discards by fleet 

## Catch
catch <- as_tibble(output$timeseries) %>% filter(Era == 'TIME') %>% 
  select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) 
names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:4], sep = "_"), paste('LanEst', fltnms[1:4], sep = "_"),
                  paste('CatEst', fltnms[1:4], sep = "_"))
aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
names(aux1) <- paste('DisEst', fltnms[1:4], sep = "_")
catch <- catch %>% bind_cols(aux1) 
catch <- catch %>% pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
  mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
  select('year', 'season', 'fleet', 'indicator', 'value')  

## Discards
discdat <- as_tibble(ss3Dat$discard_data) 
discdat <- discdat %>% mutate(fleet =  fltnms[Flt], season = recode(Seas, `2.5` = 1, `5.5` = 2, `8.5` = 3, `11.5` = 4), 
                              indicator = 'DisObs') %>% 
  select('Yr', 'season', 'fleet', 'indicator', 'Discard')
names(discdat) <- c('year', 'season', 'fleet', 'indicator', 'value')

## Bind
catch <- catch %>% bind_rows(discdat) %>%
  mutate(category = ifelse(substr(indicator,1,3) == 'Lan', 'Landings', ifelse(substr(indicator,1,3) == 'Dis', 'Discards', 'Catch')),
         type = ifelse(substr(indicator,4,6) == 'Est', 'Estimated', 'Observed'),
         time = year + 1.125 - 1/season)  %>% 
  select('year', 'season', 'time', 'fleet', 'category', 'type', 'indicator', 'value')  

discard_2020 = catch %>% group_by(year, category, type, indicator) %>% filter(indicator == 'DisEst', year==2020) %>% summarise(value = sum(value))

save(catch,file = file.path(plotdir, "catch.RData"))
pdf(paste(plotdir, "/EstObs_Land&Disc.pdf", sep=""), height = 6.5, width = 11)

## By Fleet: Landings and Discards: Fitted vs Observed.
estLand_fl <- ggplot(catch %>% filter(indicator != 'CatEst' & category == 'Landings')) +
  geom_line(aes(time, value, group = indicator, color = type), size = 0.5) + 
  geom_point(aes(time, value, group = indicator, color = type), size = 1) +
  facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = c("dodgerblue3","black")) +
  ggtitle('Landings by fleet')+ theme_light() +theme(plot.title = element_text(size=11))
print(estLand_fl)

## Discards by fleet
estDisc_fl <- ggplot(catch %>% filter(indicator != 'CatEst' & category == 'Discards' & fleet != 'LONGLINE' & year>0 & fleet == "trawlers" )) +
  geom_line(aes(time, value, group = indicator, color = type), size = 0.5) + 
  geom_point(aes(time, value, group = indicator, color = type), size = 1) +
  facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = c("dodgerblue3","black")) +
  ggtitle('Discards by fleet')+ theme_light() +theme(plot.title = element_text(size=11))

print(estDisc_fl)

dev.off()


## landings & Discards: only Observed

## Total landings & Discards: Fitted vs Observed
catch_df <- catch %>% group_by(year, category, type, indicator) %>% filter(indicator != 'CatEst', year>0) %>% summarise(value = sum(value))
obsLandDisc <- ggplot(catch %>% group_by(year, category, type, indicator) %>% filter(indicator != 'CatEst', type=='Observed',year>0) %>% summarise(value = sum(value))) +
  geom_line(aes(year, value, group = indicator, color = type), size = 1) +
  geom_point(aes(year, value, group = indicator, color = type), size = 2) +
  facet_grid(category~., scales = 'free') + scale_color_manual(values = c("black"))+
  theme_light() + ggtitle("Total landings & discards") + labs(x="Year",y="") +
  theme(plot.title = element_text(size=11), legend.title=element_blank())
print(obsLandDisc)


load('boot/data/TACs/TACS.RData') 

TACS$category <- 'Landings'
TACS <- TACS[-c(nrow(TACS)),]

pdf(paste(plotdir, "/Obs_Land&Disc.pdf", sep=""), height = 6.5, width = 11)

ggplot(catch %>% group_by(year, category, type, indicator) %>% filter(indicator != 'CatEst', type=='Observed',year>0) %>% summarise(value = sum(value))) +
  geom_line(aes(year, value, group = indicator, color = type), size = 1) +
  geom_point(aes(year, value, group = indicator, color = type), size = 2) +
  facet_grid(category~., scales = 'free') + 
  geom_path(data = filter(TACS, category == "Landings"), aes(x=years, y=agreeded_TAC, color = "TACS")) +
  geom_point(data = filter(TACS, category == "Landings"), aes(x=years, y=agreeded_TAC, color = "TACS")) +
  scale_color_manual(values = c("black", "red"), labels = c("Observed", "TACS")) +
  theme_light() + ggtitle("Total landings & discards") + labs(x="Year",y="") +
  theme(plot.title = element_text(size=11), legend.title=element_blank())

dev.off()



pdf(paste(plotdir, "/Obs_Land&Disc_fleet.pdf", sep=""), height = 6.5, width = 11)

## By Fleet: Landings and Discards: Fitted vs Observed.
estLand_fl <- ggplot(catch %>% group_by(year, category, fleet, type, indicator) %>% filter(type=='Observed', indicator != 'CatEst' & category == 'Landings') %>% summarise(value = sum(value)))   +
  geom_line(aes(year, value, group = indicator, color = type), size = 0.5) +
  geom_point(aes(year, value, group = indicator, color = type), size = 1) +
  facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = c("black")) +
  ggtitle('Landings by fleet')+ theme_light() +theme(plot.title = element_text(size=11))
print(estLand_fl)

## Discards by fleet
estDisc_fl <- ggplot(catch %>% group_by(year, category, fleet, type, indicator) %>% filter(type=='Observed', indicator != 'CatEst' & category == 'Discards' & fleet != 'LONGLINE' & year>0 & fleet == "trawlers" ) %>% summarise(value = sum(value))) +
  geom_line(aes(year, value, group = indicator, color = type), size = 0.5) +
  geom_point(aes(year, value, group = indicator, color = type), size = 1) +
  facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = c("black")) +
  ggtitle('Discards by fleet')+ theme_light() +theme(plot.title = element_text(size=11))

print(estDisc_fl)

dev.off()


## LFD's ------------------------------------------------------------------------

## Length Frequencies: Distribution & bubble plots
## Note that CPUE indices have mirrored LFDs

LFD    <- as_tibble(output$lendbase[, c('Yr', 'Seas', 'Time', 'YrSeasName',  
                                        'Fleet', 'Part', 'Bin', 'Obs', 'Exp', 
                                        'Pearson','Sex')])
fltNms <- setNames(output$definitions[,'Fleet_name'], output$definitions[,1]) 
CaComp <- setNames(c('cat','dis','lan'), 0:2) 

LFD <- LFD %>% mutate(FleetNm = fltNms[Fleet], 
                      CatchComponent = CaComp[as.character(Part)]) %>% 
  pivot_longer(cols = c('Obs', 'Exp', 'Pearson'), names_to = 'variable', 
               values_to = 'value')

LFD_plots <- setNames(vector('list', 7), c(fltNms[1:7]))

## Commercial fleets -----------------------------------------------------------

## Trawlers & Discards

save(LFD, file = file.path(plotdir, "LFD.RData"))

pdf(paste(plotdir, "/LDbin_trawlers&discards.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet == 1, variable == "Pearson", CatchComponent == 'dis')) + 
  geom_point(aes(Time, Bin, size=abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) +
  ggtitle("Commercial fleets - Trawlers discards")+ 
  theme_light() + theme(plot.title = element_text(size=11))
print(a)
a=ggplot(LFD %>% filter(Fleet == 1, variable == "Pearson", CatchComponent == 'lan')) + 
  geom_point(aes(Time, Bin, size=abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) +
  ggtitle("Commercial fleets - Trawlers landings") + 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

## Volpal


pdf(paste(plotdir, "/LDbin_volpal.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 2, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) + 
  facet_wrap(~Seas, ncol = 4) + 
  scale_color_manual(values = c('blue','red')) +
  ggtitle("Commercial fleets - Volpal")+ 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

# Artisanal



pdf(paste(plotdir, "/LDbin_artisanal.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 3, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~Seas, ncol = 4) + 
  ggtitle("Commercial fleets - Artisanal") + 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin")
print(a)
ggplot(LFD %>% filter(Fleet %in% 3, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) + 
  facet_wrap(~Seas, ncol = 4) + 
  scale_color_manual(values = c('blue','red')) +
  ggtitle("Commercial fleets - Artisanal")+ 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()





pdf(paste(plotdir, "/LDbin_cdTrw.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 4, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~Seas, ncol = 4) + 
  ggtitle("Commercial fleets - cdTrw")+ 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin")
print(a)
a=ggplot(LFD %>% filter(Fleet %in% 4, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  facet_wrap(~Seas, ncol = 4) + 
  scale_color_manual(values = c('blue','red')) +
  ggtitle("Commercial fleets - cdTrw")+ 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

## All 

pdf(paste(plotdir, "/LDbin_all_commercial_fleets.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 1:4, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~FleetNm, ncol = 4) + 
  ggtitle("Commercial fleets")+ 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin") 
print(a)
a=ggplot(LFD %>% filter(Fleet %in% 1:4, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) + 
  facet_wrap(~FleetNm, ncol = 4) +
  ggtitle("Commercial fleets")+ 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

## Surveys ---------------------------------------------------------------------

## Spsurv (5)

pdf(paste(plotdir, "/LDbin_SpSurv.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 5, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~Sex, ncol = 4) + 
  ggtitle("Surveys - SpSurv")+ 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin")
print(a)
a=ggplot(LFD %>% filter(Fleet %in% 5, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) + 
  facet_grid(Sex~.) +
  ggtitle("Surveys - SpSurv") + 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

## Indices have no LFD, they are mirrored from other fleets

## PtSurv(6)

pdf(paste(plotdir, "/LDbin_PtSurv.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 6, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~Sex, ncol = 4) + 
  ggtitle("Surveys - PtSurv")+ 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin")
print(a)
a=ggplot(LFD %>% filter(Fleet %in% 6, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) + 
  facet_grid(Sex~.) +
  ggtitle("Surveys - PtSurv") + 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

## cdSurv

pdf(paste(plotdir, "/LDbin_cdSurv.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 7, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~Sex, ncol = 4) + 
  ggtitle("Surveys - CdSurv")+ 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin")
print(a)
a=ggplot(LFD %>% filter(Fleet %in% 7, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) + facet_grid(Sex~.) +
  ggtitle("Surveys - CdSurv") + 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()

## Surveys all

pdf(paste(plotdir, "/LDbin_surveys_all.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(LFD %>% filter(Fleet %in% 5:7, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  scale_color_manual(values = c('blue','red')) + 
  facet_grid(FleetNm~.) +
  ggtitle("Surveys - SpSurv") + 
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
dev.off()


# Observed LFD by fleet

last_3y <- max(LFD$Yr)-2

pdf(paste(plotdir, "/LDbin_Obs_fleet.pdf", sep=""), height = 6.5, width = 11)

aux <- LFD %>% filter(Fleet == 1, CatchComponent == "dis", variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, variable) %>% summarise(value = sum(value))


a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  scale_fill_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  scale_color_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  ggtitle("Commercial fleets - Trawlers discards") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)

aux <- LFD %>% filter(Fleet == 1, CatchComponent == "lan", variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, variable) %>% summarise(value = sum(value))


a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  scale_fill_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  scale_color_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  ggtitle("Commercial fleets - Trawlers landings") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)

aux <- LFD %>% filter(Fleet == 2,  variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, variable) %>% summarise(value = sum(value))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  scale_fill_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  scale_color_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  ggtitle("Commercial fleets - VolPal") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())

print(a)

aux <- LFD %>% filter(Fleet == 3,  variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, variable) %>% summarise(value = sum(value))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  scale_fill_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  scale_color_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  ggtitle("Commercial fleets - Artisanal") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())

print(a)
aux <- LFD %>% filter(Fleet == 4,  variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, variable) %>% summarise(value = sum(value))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  scale_fill_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  scale_color_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  ggtitle("Commercial fleets - cdTrw") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)

aux <- LFD %>% filter(Fleet == 5,  variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, Sex, variable) %>% summarise(value = sum(value))

aux$variable=ifelse(aux$Bin>20,"Female Obs","Undetermined Obs")
aux2 = aux %>% filter(Bin==21)
aux2 = aux2 %>% group_by(Yr) %>% dplyr::summarize( value=mean(value))
aux2$Bin = 20.5
aux2 <- rbind(aux2,aux2,aux2)
lyrs <- length(unique(aux2$Yr))
aux2$variable=c(rep("Undetermined Obs",lyrs),rep("Female Obs",lyrs),rep("Male Obs",lyrs))
aux$variable[aux$Sex==2]="Male Obs"
aux=rbind(aux,aux2)

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  ggtitle("Surveys - SpSurv") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())

print(a)

aux <- LFD %>% filter(Fleet == 6,  variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, Sex, variable) %>% summarise(value = sum(value))

aux$variable=ifelse(aux$Bin>20,"Female Obs","Undetermined Obs")
aux2 = aux %>% filter(Bin==21)
aux2 = aux2 %>% group_by(Yr) %>% dplyr::summarize( value=mean(value))
aux2$Bin = 20.5
aux2 <- rbind(aux2,aux2,aux2)
lyrs <- length(unique(aux2$Yr))
aux2$variable=c(rep("Undetermined Obs",lyrs),rep("Female Obs",lyrs),rep("Male Obs",lyrs))
aux$variable[aux$Sex==2]="Male Obs"
aux=rbind(aux,aux2)

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  ggtitle("Surveys - PtSurv") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())

print(a)

aux <- LFD %>% filter(Fleet == 7,  variable %in% c('Obs'), Yr>=last_3y)  %>%  group_by(Yr, Bin, variable) %>% summarise(value = sum(value))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=0.8, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  scale_fill_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  scale_color_cyclical(
    values = "#009E73", guide = "legend",
    labels = "Obs") +
  ggtitle("Surveys - cdSurv") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)
dev.off()




# Indices-----------------------------------------------------------------------

# Surveys and CPUEs observed vs expected  

surveys <- as_tibble(output$cpue) %>% select('Fleet', 'Fleet_name', 'Yr', 'Month',
                                             'Time', 'Obs', 'Exp', 'SE') %>% 
  mutate(Obs = log(Obs), Exp = log(Exp), residuals = Obs-Exp, upp = Obs + 2*SE,
         low = Obs - 2*SE) 

pdf(paste(plotdir, "/EstObs_Indices&Surveys.pdf", sep=""), height = 6.5, width = 11)

a=ggplot(surveys) + geom_line(aes(Yr, Obs), col = 'red') + 
  geom_ribbon(aes(x = Yr, ymin = low, ymax = upp), fill = 'red', alpha = 0.3) +
  geom_point(aes(Yr, Exp), col = 'blue') +
  facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y') +
  ggtitle("Survey exp. (blue) and obs. (red) - log scale ") +
  theme_light() +theme(plot.title = element_text(size=11))
print(a)
a=ggplot(surveys) + geom_line(aes(Yr, exp(Obs)), col = 'red') + 
  geom_point(aes(Yr, exp(Exp)), col = 'blue') +
  facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y') +
  ggtitle("Survey exp. (blue) and obs. (red)")+
  theme_light() +theme(plot.title = element_text(size=11))


print(a)
dev.off()


save(surveys, file = file.path(plotdir, "surveys.RData"))
## Extra plots ------------------------------------------------------------------

### Catch prop  -----------------------------------------------------------------

DoR4SSplots <- FALSE
DoLikelihoodProfiles <- FALSE
PrepareLFDbyQuarter <- FALSE

if(DoR4SSplots){ SS_plots(output, uncertainty=T, pdf=F, png=T, datplot=T, forecastplot=F) }

## Years
output$startyr <- 1948   
output$endyr    
Yr <- unique( output$catch$Yr[output$catch$Yr >= output$startyr] ) ;  Yr

## Catch
CatchSeas <- matrix( output$catch$Obs[(output$catch$Fleet==1)&(output$catch$Yr >= output$startyr)], nrow = length(unique(output$catch$Seas)) )
CatchSeas <- array( dim=c(dim(CatchSeas), length(unique(output$catch$Fleet)), 3) )  
dim(CatchSeas) # 4 73  4  3, i.e. season, yr (1948 to 2020), fleet, Obs or Exp, ExpDiscard

## Discards
DiscardSeas <- matrix( output$discard$Obs[(output$discard$Fleet==1)&(output$discard$Yr >= output$startyr)], nrow = length(unique(output$discard$Seas)) )
DiscardSeas <- array( dim=c(dim(DiscardSeas), 2) )  
dim(DiscardSeas)  #4 26 2 (seas, yr, Obs or Exp)
DiscardSeas[,,1] <- matrix( output$discard$Obs[(output$discard$Fleet==1)&(output$discard$Yr >= output$startyr)], nrow = length(unique(output$discard$Seas)) )
DiscardSeas[,,2] <- matrix( output$discard$Exp[(output$discard$Fleet==1)&(output$discard$Yr >= output$startyr)], nrow = length(unique(output$discard$Seas)) )
DiscardYr <- matrix( apply(DiscardSeas, -1, sum), ncol=2)
DiscardYr <- cbind( c(min(output$discard$Yr):max(output$discard$Yr)), DiscardYr) 

for( fl in 1:length(unique(output$catch$Fleet)) ){    
  CatchSeas[,,fl,1] <- matrix( output$catch$Obs[(output$catch$Fleet==fl)&(output$catch$Yr >= output$startyr)], nrow = length(unique(output$catch$Seas)) ) 
  CatchSeas[,,fl,2] <- matrix( output$catch$Exp[(output$catch$Fleet==fl)&(output$catch$Yr >= output$startyr)], nrow = length(unique(output$catch$Seas)) ) 
  CatchSeas[,,fl,3] <- matrix( output$catch$kill_bio[(output$catch$Fleet==fl)&(output$catch$Yr >= output$startyr)]-output$catch$ret_bio[(output$catch$Fleet==fl)&(output$catch$Yr >= output$startyr)], nrow = length(unique(output$catch$Seas)) ) 
}

## Annual Catch
CatchYr <- array( apply(CatchSeas, -1, sum), dim=dim(CatchSeas)[-1] )  
dim(CatchYr) #73  4  2, i.e. yr, fleet, obs or exp

## Catch Proportion by Season
CatchPropSeas <- aperm( aperm( CatchSeas, perm=c(2,3,4,1) ) / as.vector(CatchYr), perm=c(4,1,2,3) )
dim( CatchPropSeas )   # 4 73 4 3, i.e. seas, yr, fl, Obs or Exp, ExpDisc

pdf(paste(plotdir, "/Extra_Catch_by_Fleet.pdf", sep=""), height = 6.5, width = 11)

titleplot1 <- "Annual Catch by Fl: Trawl,VolPal,Artis,CDtrawl"
titleplot2 <- "Annual Ratio VolPal/Trawl"
aux <- c("Obs", "Exp")

for(type in c(1,2)){
  par(mfrow=c(1,1))
  for( fl in 1:length(unique(output$catch$Fleet)) ){
    maxplot <- max(CatchYr[!is.na(CatchYr)])
    minplot <- 0
    auxi <- CatchYr[,fl,type]
    auxiYr <- Yr[!is.na(auxi)]
    auxiCatch <- auxi[!is.na(auxi)]
    if(fl==1){ 
     print( plot(auxiYr, auxiCatch, type="l", xlab="",ylab="", main="", ylim=c(minplot,maxplot)) )
      print(title( paste(aux[type], titleplot1) ) )
    }
    
   print( lines( auxiYr, auxiCatch, col=fl, lwd=1.5))
    print(points(auxiYr, auxiCatch, col=fl, pch=20))
    
    if(type==2){
      auxi <- CatchYr[,fl,3]
      auxiYr <- Yr[!is.na(auxi)]
      auxiCatch <- auxi[!is.na(auxi)]
      
      if( max(auxiCatch)>0.1 ){
       print( lines( auxiYr, auxiCatch, col=fl, lwd=1.5))
       print( points(auxiYr, auxiCatch, col=fl, pch=20))
      }
      
      print(abline(h = 1, col=5, lty=2 ))
    }
    
  }
}

dev.off()

pdf(paste(plotdir, "/Extra_ObsVsExpbyfleet.pdf", sep=""), height = 6.5, width = 11)

### Total
exp=CatchYr[,1,2]
obs=CatchYr[,1,1]

t_exp=rep(0,length(exp))
t_obs=rep(0,length(obs))
for( fl in 1:length(unique(output$catch$Fleet)) ){
  exp=CatchYr[,fl,2]
  t_exp=t_exp+exp
  obs=CatchYr[,fl,1]
  t_obs=t_obs+obs
}
par(mfrow=c(1,1))
print(plot(auxiYr, t_exp, type="l", xlab="",ylab="",lty=2, main="Catches - total",col="dodgerblue3", lwd=1.8) )
print(lines( auxiYr, t_obs, col="gray60",lty=1))

print(legend(x = "topleft",  cex = 0.7,        # Position
       legend = c("Exp.", "Obs."),  # Legend texts
       lty = c(2, 1),           # Line types
       col = c("dodgerblue3", "gray60"),           # Line colors
       lwd = 2))


dim(CatchYr) # 74  4  2

if(type==1){
  auxi <- CatchYr[,2,type]/CatchYr[,1,type]
  print(plot( cbind(Yr[!is.na(auxi)], round(auxi[!is.na(auxi)],2) ), type="p", pch=20, main="" , xlab="", ylab="", ylim=c(0,max(auxi[!is.na(auxi)])), col=2))
  print(title( paste(aux[type], titleplot2) ) )
  print(abline(h = 1, col=5, lty=2 ))
}

### Plot of obs and expected catches by fleet
vec=c("trawlers","volpal","artisanal","cdTrw")
par(mfcol=c(2,2))
if(type==2){
  for( fl in 1:length(unique(output$catch$Fleet)) ){
    exp=CatchYr[,fl,2]
    obs=CatchYr[,fl,1]
    
    auxiYr <- Yr[!is.na(obs)]
    exp <- exp[!is.na(exp)]
    obs <- obs[!is.na(obs)]
    
    main=paste("Catches - ",vec[fl],sep="")
   print( plot(auxiYr, exp, type="l", xlab="",ylab="",lty=2, main=main,col="dodgerblue3", lwd=1.8) )
    print(lines( auxiYr, obs, col="gray60",lty=1))
    
    print(legend(x = "topleft",  cex = 0.7, # Position
           legend = c("Exp.", "Obs."), # Legend texts
           lty = c(2, 1), # Line types
           col = c("dodgerblue3", "gray60"), # Line colors
           lwd = 2) )    
    
  }
  
}

dev.off()

dim(CatchSeas)  # 4 73 4 2 seas, yr, fl, obs or exp
dim(CatchYr)    #   73 4 2       yr, fl, 1(obs) or 2(exp/obs)
dim( CatchPropSeas )   # 4 73 4 2, i.e. seas, yr, fl, Obs or Exp
titleplot1 <- "Catch Prop by Q:"

pdf(paste(plotdir, "/Extra_catch_prop_by_fleet.pdf", sep=""), height = 6.5, width = 11)

par(mfcol=c(2,2))
for( fl in 1:length(unique(output$catch$Fleet)) ){
  for(type in c(1,2)){
    
    for(seas in 1:4){
      maxplot <- max(CatchPropSeas[!is.na(CatchPropSeas)])
      minplot <- 0
      auxi <- CatchPropSeas[seas,,fl,type]
      auxiYr <- Yr[!is.na(auxi)]
      auxiCatch <- auxi[!is.na(auxi)]
      if(seas==1){
        print(plot(auxiYr, auxiCatch, type="l", xlab="",ylab="", main="", ylim=c(minplot,maxplot)) )
        print(title( paste(aux[type], titleplot1, output$FleetNames[fl]) ))
        print(abline(h=0.25, col=5, lty=2))
      } 
      
      print(lines( auxiYr, auxiCatch, col=seas, lwd=1.5))
      print(points(auxiYr, auxiCatch, col=seas, pch=20))
      if(type==2) abline(h = 1, col=5, lty=2 )
    }
  }
}

dev.off()

### SSB cut -------------------------------------------------------------------

## Maturity at length
## Length increments in population length bins (distance between bins)
increments <- output$lbinspop[-1]-output$lbinspop[-output$nlbinspop] 
increments <- c(increments, increments[length(increments)])

## Length at mid-point of population length bins
len <- output$lbinspop + increments/2
matslope <- as.numeric(output$MGparmAdj$"Mat_slope_Fem")[1]
matl50 <- as.numeric(output$MGparmAdj$"Mat50%_Fem")[1]  
matlen <- 1/( 1 + exp(matslope*(len - matl50 )) ) # Ogiva: % matures by length

## Weight at length 
wlena <- as.numeric(output$MGparmAdj$Wtlen_1_Fem)[1] 
wlenb <- as.numeric(output$MGparmAdj$Wtlen_2_Fem)[1] 
wlen <- wlena*(len^wlenb) # Weight at length for each one of the lengths 


natlen<-output$natlen
names(natlen)
natlen<-natlen %>% filter(Sex==1, `Beg/Mid`=="B", Seas==1)

vec=colnames(natlen)
ind=which(vec=="4"); lvec=length(vec)
vec=vec[ind:lvec]

N=unique(natlen$Yr)
for (i in 1:length(vec)){
  
  aux=aggregate(natlen[,ind+i-1], by=list(Category=natlen$Yr), FUN=sum)
  
  N=cbind(N,aux[,2])
}
colnames(N)=c("Yr",vec)

## Total
laux=length(unique(natlen$Yr))
year=unique(natlen$Yr)[1:(laux-3)] # 3 forecast years
l_l=length(unique(wlen))
l_y=length((year))
SSB=matrix(0,ncol=l_l,nrow=l_y)
B=matrix(0,ncol=l_l,nrow=l_y)
for (i in 1:l_y){
  for (j in 1:l_l){
    SSB[i,j]=N[i,j+1]*wlen[j]*matlen[j]
    B[i,j]=N[i,j+1]*wlen[j]
  }
}

ssb_t=apply(SSB, 1,sum)
b_t=apply(B, 1,sum)

## Define cut! -----------------------------------------------------------------

vec ## select a value from here
cut_len=90

ind=which(vec==cut_len)
l_l=length(vec[1:ind])

SSB=matrix(0,ncol=l_l,nrow=l_y)
B=matrix(0,ncol=l_l,nrow=l_y)
for (i in 1:l_y){
  for (j in 1:l_l){
    SSB[i,j]=N[i,j+1]*wlen[j]*matlen[j]
    B[i,j]=N[i,j+1]*wlen[j]
  }
}

ssb_t_cut=apply(SSB, 1,sum)
b_t_cut=apply(B, 1,sum)

pdf(paste(plotdir, "/Extra_SSB_cut.pdf", sep=""), height = 6.5, width = 11)

par(mfcol=c(2,2))

print(plot(year, b_t, type="l", ylab="B",xlab="Year",lty=2, main="Bio",col="black", lwd=1.8) )
print(lines( year, b_t_cut, col="orange",lty=1))

print(legend(x = "topright",  cex = 0.7,        # Position
       legend = c("B", paste("B<", cut_len, sep="")),  # Legend texts
       lty = c(2, 1),           # Line types
       col = c("black", "orange"),           # Line colors
       lwd = 2))

ylab=paste(paste("1-(B<", cut_len, sep=""),"/B)")
main=paste(paste("B proportion above ",cut_len,sep=""),"cm")

print(plot(year, 1-b_t_cut/b_t, type="l",ylim=c(0,1), 
     ylab=ylab,xlab="Year",lty=2, 
     main=main,col="black", lwd=1.8) )

max_b=max(b_t)
min_b=min(b_t_cut)
print(plot(year, ssb_t, type="l", ylim=c(min_b,max_b),
     ylab="SSB",xlab="Year",lty=2, 
     main="SSB",col="black", lwd=1.8) )
print(lines( year, ssb_t_cut, col="orange",lty=1))

print(legend(x = "topright",  cex = 0.7,        # Position
       legend = c("SSB", paste("SSB<", cut_len, sep="")),  # Legend texts
       lty = c(2, 1),           # Line types
       col = c("black", "orange"),           # Line colors
       lwd = 2))

ylab=paste(paste("1-(SSB<", cut_len, sep=""),"/SSB)")
main=paste(paste("SSB proportion above ",cut_len,sep=""),"cm")
print(plot(year, 1-ssb_t_cut/ssb_t, type="l",ylim=c(0,1), 
     ylab=ylab,xlab="Year",lty=2, main=main,col="black", lwd=1.8) )
dev.off()


