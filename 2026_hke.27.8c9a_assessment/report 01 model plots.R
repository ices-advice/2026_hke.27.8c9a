
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Report plots for SS shake model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 25/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo    #
# Marta Cousido          #
# Anxo Paz               #
# Santiago Cervino       #
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline


## The code provides: Landings and discards observed LFDs
##        Total landings and discards
##        Survey pearson residuals indices
##        LFDs plots (estimated vs obs)



# Load packages!


library(tidyverse)
library(ggplot2)
library(ggridges)
library(icesTAF)
library(conflicted)
library(dplyr)
library(plyr)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarize)

plotdir_report<-paste0(getwd(), "/","report/plots") 
mkdir(plotdir_report)

# Length distributions obs -------------------------------------------
run <- 'model/final' ## *CHANGE name
mod_path <- paste0(getwd(), "/",run, sep="") 
start <- r4ss::SS_readstarter(file = file.path(mod_path, "starter.ss"), 
                              verbose = FALSE)
ss3Dat <- r4ss::SS_readdat(file = file.path(mod_path, start$datfile),
                           verbose = FALSE)


ldfs=ss3Dat$lencomp

ldfs=subset(ldfs,ldfs$FltSvy==1 |ldfs$FltSvy==2 |ldfs$FltSvy==3|ldfs$FltSvy==4)

## Landings --------------------------------------------------------------------

landings=subset(ldfs,ldfs$Part==2 | ldfs$Part==0)
m=landings[,c(1,7:dim(landings)[2])]
land=t(sapply(split(as.data.frame(m), m$Yr), colSums))[,-1]


land=land[,1:(dim(land)[2]/2)]
colnames(land)
len=c(4:40,seq(42,100,by=2))

land=split(land, seq(nrow(land)))
lst <- data.frame(unlist(land))
lst$series<-sort(rep(unique(landings$Yr),length(len)))
lst$len_cm<-rep(len,length(unique(landings$Yr)))
colnames(lst)=c("dens","series","len_cm")

lst$data=rep("landings",dim(lst)[1])

## Discards --------------------------------------------------------------------

discards=subset(ldfs,ldfs$Part==1)
m=discards[,c(1,7:dim(discards)[2])]
disc=t(sapply(split(as.data.frame(m), m$Yr), colSums))[,-1]


disc=disc[,1:(dim(disc)[2]/2)]
colnames(disc)
len=c(4:40,seq(42,100,by=2))

disc=split(disc, seq(nrow(disc)))
aux <- data.frame(unlist(disc))
aux$series<-sort(rep(unique(discards$Yr),length(len)))
aux$len_cm<-rep(len,length(unique(discards$Yr)))
colnames(aux)=c("dens","series","len_cm")



years=unique(aux$series)
catches=lst
catches$data=rep("catches",dim(catches)[1])
for (i in 1:length(years)){
  ind=which(aux$series==years[i])
  ind2=which(lst$series==years[i])
  catches[ind2,]$dens=lst$dens[ind2]+aux$dens[ind]
}

final=rbind(catches,lst)
final$data=as.factor(final$data)


a=ggplot(final, aes(x=len_cm, y =dens, colour=data, fill=data)) +
  geom_density(stat="identity",alpha = 0.5)+
  theme_bw() + facet_wrap(~series,dir="v") +
  theme_light(base_size = 10) + xlim(0,100) +
  theme(strip.text = element_text( size=7, color="black"),
        strip.background = element_rect(fill="grey93", 
                                        colour="black",linewidth=0.5)) +
  theme(strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm"))) +
  labs(title="", x="Length (cm)", y = "Density") +
  theme(legend.title=element_blank())
print(a)
ggsave(paste(plotdir_report, "/Figure 10.1.png", sep=""),width = 9,height = 7)



# Total landings & Discards: Fitted vs Observed --------------------------------
plotdir<-paste0(getwd(), "/","output/additional plots", sep="") 
load(file = file.path(plotdir, "catch.RData"))

png(paste(plotdir_report, "/Figure 10.7a.png", sep=""), height = 700, width = 1000)

a=catch %>% group_by(year, category, type, indicator) %>% filter(indicator != 'CatEst', year>0) %>% summarise(value = sum(value))

estLandDisc <- ggplot(catch %>% group_by(year, category, type, indicator) %>% filter(indicator != 'CatEst', year>0) %>% summarise(value = sum(value))) +
  geom_line(aes(year, value, group = indicator, color = type), size = 1) + 
  geom_point(aes(year, value, group = indicator, color = type), size = 2) +
  facet_grid(category~., scales = 'free') + scale_color_manual(values = c("dodgerblue3","black"))+ 
  theme_light() + ggtitle("Total landings & discards") + labs(x="Year",y="") +
  theme(plot.title = element_text(size=11), legend.title=element_blank())

print(estLandDisc)
dev.off()

# Survey pearson residuals------------------------------------------------------

load(file = file.path(plotdir, "surveys.RData"))
png(paste(plotdir_report, "/Figure 10.7b.png", sep=""), height = 700, width = 1000)


a=ggplot(surveys) + geom_line(aes(Yr, residuals)) + 
  geom_point(aes(Yr, residuals), col = 'blue') +
  facet_wrap(~Fleet_name, ncol = 2, scales = 'free_y')+
  geom_hline(yintercept = 0) + ggtitle("Survey pearson residuals") +
  theme_light() + labs(x="Years",y="Residuals") +
  theme(plot.title = element_text(size=11)) + theme(plot.title = element_text(size=11))
print(a)
dev.off()

# LFDs plots --------------------------------------------------------------------

load(file = file.path(plotdir, "LFD.RData"))
png(paste(plotdir_report, "/Figure 10.7e.png", sep=""), height = 1000, width = 1000)

aux <- LFD %>% filter(Fleet == 1, CatchComponent == "dis", 
                      variable %in% c('Obs', 'Exp'))


a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), 
                     group = interaction(Yr, variable), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=1.2, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  facet_wrap(~Seas, ncol = 4) +
  scale_fill_cyclical(
    values = c("transparent","#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  scale_color_cyclical(
    values = c("black", "#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  ggtitle("Commercial fleets - Trawlers discards") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)
dev.off()

png(paste(plotdir_report, "/Figure 10.7d.png", sep=""), height = 1000, width = 1000)

aux <- LFD %>% filter(Fleet == 1, CatchComponent == "lan", 
                      variable %in% c('Obs', 'Exp'))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), 
                     group = interaction(Yr, variable), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=1.2, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  facet_wrap(~Seas, ncol = 4) +
  scale_fill_cyclical(
    values = c("transparent","#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  scale_color_cyclical(
    values = c("black", "#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  ggtitle("Commercial fleets - Trawlers landings") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)
dev.off()

png(paste(plotdir_report, "/Figure 10.7f.png", sep=""), height = 1000, width = 1000)

aux <- LFD %>% filter(Fleet %in% 2, variable %in% c('Obs', 'Exp'))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), 
                     group = interaction(Yr, variable), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=1.2, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  facet_wrap(~Seas, ncol = 4) +
  scale_fill_cyclical(
    values = c("transparent","#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  scale_color_cyclical(
    values = c("black", "#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  ggtitle("Commercial fleets - Volpal") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)
dev.off()

png(paste(plotdir_report, "/Figure 10.7g.png", sep=""), height = 1000, width = 1000)

aux <- LFD %>% filter(Fleet %in% 3, variable %in% c('Obs', 'Exp'))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), 
                     group = interaction(Yr, variable), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=1.2, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  facet_wrap(~Seas, ncol = 4) +
  scale_fill_cyclical(
    values = c("transparent","#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  scale_color_cyclical(
    values = c("black", "#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  ggtitle("Commercial fleets - Artisanal") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)
dev.off()
png(paste(plotdir_report, "/Figure 10.7h.png", sep=""), height = 1000, width = 1000)

aux <- LFD %>% filter(Fleet %in% 4, variable %in% c('Obs', 'Exp'))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), 
                     group = interaction(Yr, variable), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=1.2, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  facet_wrap(~Seas, ncol = 4) +
  scale_fill_cyclical(
    values = c("transparent","#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  scale_color_cyclical(
    values = c("black", "#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  ggtitle("Commercial fleets - CdTrw") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)
dev.off()


aux <- LFD %>% filter(Fleet %in% 5:7, variable %in% c('Obs', 'Exp'))

a=ggplot(data=aux, aes(x=Bin, height=value, y=factor(Yr), 
                     group = interaction(Yr, variable), color = variable)) +
  geom_density_ridges(aes(fill=variable), stat="identity",  scale=1.2, alpha=0.3, size = 0.2, show.legend = FALSE) + 
  geom_line() +
  facet_wrap(~FleetNm, ncol = 4) +
  scale_fill_cyclical(
    values = c("transparent","#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  scale_color_cyclical(
    values = c("black", "#009E73"), guide = "legend",
    labels = c("Exp" = "Exp", "Obs" = "Obs")) +
  ggtitle("Surveys") +
  theme_light() + labs(x="LD by bin",y="Year") +
  theme(plot.title = element_text(size=11), legend.title = element_blank())
print(a)


png(paste(plotdir_report, "/Figure 10.7i.png", sep=""), height = 1000, width = 1000)

spaux <- subset( aux, Fleet == 5) 

spaux$Sex[which(spaux$Bin<21)] <- 'Undetermined'
spaux$Sex[which(spaux$Sex==2)] <- 'Males'
spaux$Sex[which(spaux$Sex==1)] <- 'Females'

spaux$value[which(spaux$Sex=='Males')] <- -1 * spaux$value[which(spaux$Sex=='Males')]

spaux$inter <- paste0(spaux$Sex,' ',spaux$variable)


ggplot( spaux, aes(x = Bin, y = value, group = inter, fill = inter)) +
  geom_path( aes(col = inter)) +
  geom_ribbon( aes(ymin = 0, ymax = value), alpha = 0.3) +
  labs(x = "Bin (cm)", y = "LD") + ggtitle ('SpSurv') +
  facet_wrap(~ Yr, scales = "free", dir = 'v') +
  scale_color_manual( name='',
                      values=c('Females Obs' = 2, 'Males Obs' = 3, 
                               'Undetermined Obs' = 4, 'Females Exp' = 'black', 
                               'Males Exp' = 'black', 'Undetermined Exp' = 'black')) +
  scale_fill_manual( name='',
                     values=c('Females Obs' = 2, 'Males Obs' = 3, 
                              'Undetermined Obs' = 4, 'Females Exp' = "transparent", 
                              'Males Exp' = "transparent", 'Undetermined Exp' = "transparent")) +
  theme_minimal() 

dev.off()

png(paste(plotdir_report, "/Figure 10.7j.png", sep=""), height = 1000, width = 1000)

ptaux <- subset( aux, Fleet == 6) 

ptaux$Sex[which(ptaux$Bin<21)] <- 'Undetermined'
ptaux$Sex[which(ptaux$Sex==2)] <- 'Males'
ptaux$Sex[which(ptaux$Sex==1)] <- 'Females'

ptaux$value[which(ptaux$Sex=='Males')] <- -1 * ptaux$value[which(ptaux$Sex=='Males')]

ptaux$inter <- paste0(ptaux$Sex,' ',ptaux$variable)

ggplot( ptaux, aes(x = Bin, y = value, group = inter, fill = inter)) +
  geom_path( aes(col = inter)) +
  geom_ribbon( aes(ymin = 0, ymax = value), alpha = 0.3) +
  labs(x = "Bin (cm)", y = "LD") + ggtitle ('PtSurv') +
  facet_wrap(~ Yr, scales = "free", dir = 'v') +
  scale_color_manual( name='',
                      values=c('Females Obs' = 2, 'Males Obs' = 3, 
                               'Undetermined Obs' = 4, 'Females Exp' = 'black', 
                               'Males Exp' = 'black', 'Undetermined Exp' = 'black')) +
  scale_fill_manual( name='',
                     values=c('Females Obs' = 2, 'Males Obs' = 3, 
                              'Undetermined Obs' = 4, 'Females Exp' = "transparent", 
                              'Males Exp' = "transparent", 'Undetermined Exp' = "transparent")) +
  theme_minimal() 

dev.off()


png(paste(plotdir_report, "/Figure 10.7k.png", sep=""), height = 1000, width = 1000)

cdaux <- subset( aux, Fleet == 7) 

ggplot( cdaux, aes(x = Bin, y = value, fill = variable)) +
  geom_path( aes(col = variable)) +
  geom_ribbon( aes(ymin = 0, ymax = value), alpha = 0.3) +
  labs(x = "Bin (cm)", y = "LD") + ggtitle ('CdSurv') +
  facet_wrap(~ Yr, scales = "free", dir = 'v') +
  scale_color_manual(name='', values=c('Exp'="#000000", 'Obs'="#009E73")) +
  scale_fill_manual( name='', values=c('Exp'="transparent", 'Obs'="#009E73")) +
  theme_minimal() 

dev.off()



aux=plyr::ddply(LFD, .(Fleet,FleetNm,Sex,Bin,variable,
                       CatchComponent), summarize,  
                value=sum(value))


aux_trlan=aux %>% filter(Fleet %in% 1, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'lan')
aux_trlan_o=aux %>% filter(Fleet %in% 1, variable %in% c('Obs'),CatchComponent %in%  'lan')
aux_trlan_e=aux %>% filter(Fleet %in% 1, variable %in% c('Exp'),CatchComponent %in%  'lan')
aux_trlan$value2=aux_trlan$value

index=which(aux_trlan$variable=="Obs")
aux_trlan$value2[index]=aux_trlan$value2[index]/sum(aux_trlan_o$value)
aux_trlan$value2[-index]=aux_trlan$value2[-index]/sum(aux_trlan_e$value)

plot1 = ggplot()+
  geom_density(data=aux_trlan %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73", show.legend = FALSE) +
  geom_line(data=aux_trlan,aes(x=Bin, y=value2, colour = variable), size=1) +
  scale_color_manual(name='',
                     breaks=c('Exp', 'Obs'),
                     values=c('Exp'="#000000", 'Obs'="#009E73")) +
  geom_point(data=aux_trlan %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) + ggtitle("Trawlers (retained)")+
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="")



aux_trdis=aux %>% filter(Fleet %in% 1, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'dis')
aux_trdis_o=aux %>% filter(Fleet %in% 1, variable %in% c('Obs'),CatchComponent %in%  'dis')
aux_trdis_e=aux %>% filter(Fleet %in% 1, variable %in% c('Exp'),CatchComponent %in%  'dis')
aux_trdis$value2=aux_trdis$value

index=which(aux_trdis$variable=="Obs")
aux_trdis$value2[index]=aux_trdis$value2[index]/sum(aux_trdis_o$value)
aux_trdis$value2[-index]=aux_trdis$value2[-index]/sum(aux_trdis_e$value)

plot2 = ggplot()+
  geom_density(data=aux_trdis %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73", show.legend = FALSE) +
  geom_line(data=aux_trdis, aes(x=Bin, y=value2, colour = variable), size=1) +
  scale_color_manual(name='',
                     breaks=c('Exp', 'Obs'),
                     values=c('Exp'="#000000", 'Obs'="#009E73")) +
  geom_point(data=aux_trdis %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("Trawlers (discards)")+
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="")


aux_volpal=aux %>% filter(Fleet %in% 2, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat')
aux_volpal_o=aux %>% filter(Fleet %in% 2, variable %in% c('Obs'),CatchComponent %in%  'cat')
aux_volpal_e=aux %>% filter(Fleet %in% 2, variable %in% c('Exp'),CatchComponent %in%  'cat')
aux_volpal$value2=aux_volpal$value

index=which(aux_volpal$variable=="Obs")
aux_volpal$value2[index]=aux_volpal$value2[index]/sum(aux_volpal_o$value)
aux_volpal$value2[-index]=aux_volpal$value2[-index]/sum(aux_volpal_e$value)

plot3 = ggplot()+
  geom_density(data=aux_volpal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73", show.legend = FALSE) +
  geom_line(data=aux_volpal,aes(x=Bin, y=value2, colour = variable), size=1) +
  scale_color_manual(name='',
                     breaks=c('Exp', 'Obs'),
                     values=c('Exp'="#000000", 'Obs'="#009E73")) +
  geom_point(data=aux_volpal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("VolPal")+
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="")




aux_artisanal=aux %>% filter(Fleet %in% 3, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat')
aux_artisanal_o=aux %>% filter(Fleet %in% 3, variable %in% c('Obs'),CatchComponent %in%  'cat')
aux_artisanal_e=aux %>% filter(Fleet %in% 3, variable %in% c('Exp'),CatchComponent %in%  'cat')
aux_artisanal$value2=aux_artisanal$value

index=which(aux_artisanal$variable=="Obs")
aux_artisanal$value2[index]=aux_artisanal$value2[index]/sum(aux_artisanal_o$value)
aux_artisanal$value2[-index]=aux_artisanal$value2[-index]/sum(aux_artisanal_e$value)

plot4 = ggplot()+
  geom_density(data=aux_artisanal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73", show.legend = FALSE) +
  geom_line(data=aux_artisanal,aes(x=Bin, y=value2, colour = variable), size=1) +
  scale_color_manual(name='',
                     breaks=c('Exp', 'Obs'),
                     values=c('Exp'="#000000", 'Obs'="#009E73")) +
  geom_point(data=aux_artisanal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("Artisanal")+
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="Proportion")



aux_cdtrw=aux %>% filter(Fleet %in% 4, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat')
aux_cdtrw_o=aux %>% filter(Fleet %in% 4, variable %in% c('Obs'),CatchComponent %in%  'cat')
aux_cdtrw_e=aux %>% filter(Fleet %in% 4, variable %in% c('Exp'),CatchComponent %in%  'cat')
aux_cdtrw$value2=aux_cdtrw$value

index=which(aux_cdtrw$variable=="Obs")
aux_cdtrw$value2[index]=aux_cdtrw$value2[index]/sum(aux_cdtrw_o$value)
aux_cdtrw$value2[-index]=aux_cdtrw$value2[-index]/sum(aux_cdtrw_e$value)

plot5 = ggplot()+
  geom_density(data=aux_cdtrw %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73", show.legend = FALSE) +
  geom_line(data=aux_cdtrw,aes(x=Bin, y=value2, colour = variable), size=1) +
  scale_color_manual(name='',
                     breaks=c('Exp', 'Obs'),
                     values=c('Exp'="#000000", 'Obs'="#009E73")) +
  geom_point(data=aux_cdtrw %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("CdTrw")+
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="")


observed5 <- aux %>% filter(Fleet %in% 5, variable %in% c('Obs'),CatchComponent %in%  'cat')
expected5 <- aux %>% filter(Fleet %in% 5, variable %in% c('Exp'),CatchComponent %in%  'cat')

aux$sex2=aux$Sex
ind=which(aux$Sex==1)
aux$Sex2[ind]="Female"
aux$Sex2[-ind]="Male"

aux_mal=aux %>% filter(Fleet %in% 5, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat', Sex2 %in% 'Male')
aux_mal_o=aux %>% filter(Fleet %in% 5, variable %in% c('Obs'),CatchComponent %in%  'cat', Sex2 %in% 'Male')
aux_mal_e=aux %>% filter(Fleet %in% 5, variable %in% c('Exp'),CatchComponent %in%  'cat',Sex2 %in% 'Male')
aux_mal$value2=-aux_mal$value

index=which(aux_mal$variable=="Obs")
aux_mal$value2[index]=aux_mal$value2[index]/sum(observed5$value)
aux_mal$value2[-index]=aux_mal$value2[-index]/sum(expected5$value)


aux_fem=aux %>% filter(Fleet %in% 5, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat', Sex2 %in% 'Female')
aux_fem_o=aux %>% filter(Fleet %in% 5, variable %in% c('Obs'),CatchComponent %in%  'cat', Sex2 %in% 'Female')
aux_fem_e=aux %>% filter(Fleet %in% 5, variable %in% c('Exp'),CatchComponent %in%  'cat',Sex2 %in% 'Female')
aux_fem$value2=aux_fem$value

index=which(aux_fem$variable=="Obs")
aux_fem$value2[index]=aux_fem$value2[index]/sum(aux_fem_o$value)
aux_fem$value2[-index]=aux_fem$value2[-index]/sum(aux_fem_e$value)

plot6 = ggplot()+
  geom_density(data=aux_fem %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73") +
  geom_density(data=aux_mal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), fill="#009E73", stat="identity", alpha=0.2, size = 0.2) + 
  geom_line(data=aux_fem %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2, color='Obs'), size=1) +
  geom_line(data=aux_mal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2, color = 'Obs'), size=1) +
  geom_line(data=aux_fem %>% filter(variable %in% c('Exp')), aes(x=Bin, y=value2, color = 'Female'), size=1) +
  geom_line(data=aux_mal %>% filter(variable %in% c('Exp')), aes(x=Bin, y=value2, color = 'Male'), size=1) +
  geom_point(data=aux_fem %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  geom_point(data=aux_mal %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("SpSurv")+
  scale_color_manual(name='',
                     breaks=c('Female', 'Male', 'Obs'),
                     values=c('Female'="#D55E00", 'Male'="#56B4E9", 'Obs'="#009E73")) +
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="")


observed6 <- aux %>% filter(Fleet %in% 6, variable %in% c('Obs'),CatchComponent %in%  'cat')
expected6 <- aux %>% filter(Fleet %in% 6, variable %in% c('Exp'),CatchComponent %in%  'cat')

aux_mal2=aux %>% filter(Fleet %in% 6, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat', Sex2 %in% 'Male')
aux_mal2_o=aux %>% filter(Fleet %in% 6, variable %in% c('Obs'),CatchComponent %in%  'cat', Sex2 %in% 'Male')
aux_mal2_e=aux %>% filter(Fleet %in% 6, variable %in% c('Exp'),CatchComponent %in%  'cat',Sex2 %in% 'Male')
aux_mal2$value2=-aux_mal2$value

index=which(aux_mal2$variable=="Obs")
aux_mal2$value2[index]=aux_mal2$value2[index]/sum(observed6$value)
aux_mal2$value2[-index]=aux_mal2$value2[-index]/sum(expected6$value)


aux_fem2=aux %>% filter(Fleet %in% 6, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat', Sex2 %in% 'Female')
aux_fem2_o=aux %>% filter(Fleet %in% 6, variable %in% c('Obs'),CatchComponent %in%  'cat', Sex2 %in% 'Female')
aux_fem2_e=aux %>% filter(Fleet %in% 6, variable %in% c('Exp'),CatchComponent %in%  'cat',Sex2 %in% 'Female')
aux_fem2$value2=aux_fem2$value

index=which(aux_fem2$variable=="Obs")
aux_fem2$value2[index]=aux_fem2$value2[index]/sum(aux_fem2_o$value)
aux_fem2$value2[-index]=aux_fem2$value2[-index]/sum(aux_fem2_e$value)

plot7 = ggplot()+
  geom_density(data=aux_fem2 %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73") +
  geom_density(data=aux_mal2 %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), fill="#009E73", stat="identity", alpha=0.2, size = 0.2) + 
  geom_line(data=aux_fem2 %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2, color='Obs'), size=1) +
  geom_line(data=aux_mal2 %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2, color = 'Obs'), size=1) +
  geom_line(data=aux_fem2 %>% filter(variable %in% c('Exp')), aes(x=Bin, y=value2, color = 'Female'), size=1) +
  geom_line(data=aux_mal2 %>% filter(variable %in% c('Exp')), aes(x=Bin, y=value2, color = 'Male'), size=1) +
  geom_point(data=aux_fem2 %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  geom_point(data=aux_mal2 %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("PtSurv")+
  scale_color_manual(name='',
                     breaks=c('Female', 'Male', 'Obs'),
                     values=c('Female'="#D55E00", 'Male'="#56B4E9", 'Obs'="#009E73")) +
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="",y="")




aux_cdsurv=aux %>% filter(Fleet %in% 7, variable %in% c('Obs', 'Exp'),CatchComponent %in%  'cat')
aux_cdsurv_o=aux %>% filter(Fleet %in% 7, variable %in% c('Obs'),CatchComponent %in%  'cat')
aux_cdsurv_e=aux %>% filter(Fleet %in% 7, variable %in% c('Exp'),CatchComponent %in%  'cat')
aux_cdsurv$value2=aux_cdsurv$value

index=which(aux_cdsurv$variable=="Obs")
aux_cdsurv$value2[index]=aux_cdsurv$value2[index]/sum(aux_cdsurv_o$value)
aux_cdsurv$value2[-index]=aux_cdsurv$value2[-index]/sum(aux_cdsurv_e$value)

# ggplot()+
#   geom_density(data=aux_cdsurv %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill=3, show.legend = FALSE) +
#   geom_line(data=aux_cdsurv,aes(x=Bin, y=value2, colour = variable), size=1) +
#   geom_point(data=aux_cdsurv %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
#   ylim(0,0.2) +
#   ggtitle("CdSurv")+ 
#   theme_light() +theme(plot.title = element_text(size=11),
#                        axis.title.y=element_blank(),
#                        legend.title=element_blank())+ 
#   xlab("LD by bin") 


plot8 = ggplot()+
  geom_density(data=aux_cdsurv %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2), stat="identity", alpha=0.2, size = 0.2, fill="#009E73", show.legend = FALSE) +
  geom_line(data=aux_cdsurv,aes(x=Bin, y=value2, colour = variable), size=1) +
  scale_color_manual(name='',
                     breaks=c('Exp', 'Obs'),
                     values=c('Exp'="#000000", 'Obs'="#009E73")) +
  geom_point(data=aux_cdsurv %>% filter(variable %in% c('Obs')), aes(x=Bin, y=value2),colour="turquoise4", size=1.2) +
  ylim(-0.2,0.2) + xlim(0,80) +
  ggtitle("CdSurv")+
  theme_light() + theme(plot.title = element_text(size=11),
                        legend.title=element_blank(),
                        legend.background = element_rect(fill="transparent"),
                        legend.position = c(0.8,0.2)) + 
  labs(x="Length(cm)",y="")


png(paste(plotdir_report, "/Figure 10.7c.png", sep=""), height = 1000, width = 1000)

print(ggpubr::ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol=3,nrow=3))

dev.off()

