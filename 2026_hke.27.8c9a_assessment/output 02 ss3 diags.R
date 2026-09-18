#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Apply ss3diags to SS shake model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 25/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Massimiliano Cardinale #
# Francisco Izquierdo    #
# Marta Cousido          #
# Santiago Cervino       #
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

rm(list=ls()) 
library(r4ss)
library(ss3diags)

# Read retro information ------------------------------------------------------
run <- 'model/final' ## *CHANGE name
mod_path <- paste0(getwd(), "/", run, sep="") 
plotdir<-paste(getwd(), '/output/ss3 diags', sep="")

dir.create(plotdir)

yper=0:-5 
retroModels <- SSgetoutput(dirvec=file.path(mod_path, "retros",
                                            paste("retro",yper,sep="")))
retroSummary <- SSsummarize(retroModels) 



## Base model
ss3rep = retroModels[[1]]

# ss3diags ---------------------------------------------------------------------

## Index residuals -------------------------------------------------------------
par(mfrow=c(2,3))
SSplotRunstest(ss3rep,subplots="cpue",add=T)
dev.print(jpeg,paste0(plotdir,"/RunsTestResiduals_index.jpg"), 
          width = 8, height = 7, res = 300, units = "in")
dev.off()

## Mean length residuals--------------------------------------------------------
par(mfrow=c(3,3))
SSplotRunstest(ss3rep,subplots="len",add=T)
dev.print(jpeg,paste0(plotdir,"/RunsTestResiduals_len.jpg"), 
          width = 8, height = 7, res = 300, units = "in")

## Join residuals (mean length and index) -------------------------------------
par(mfrow=c(1,2))
SSplotJABBAres(ss3rep,subplots="cpue",add=T)
SSplotJABBAres(ss3rep,subplots="len",add=T)
dev.print(jpeg,paste0(plotdir,"/JointResiduals_.jpg"), 
          width = 8, height = 3.5, res = 300, units = "in")
dev.off()


## Kobe plot -------------------------------------------------------------------
par(mfrow=c(1,1))
mvn = SSdeltaMVLN(ss3rep,plot = T,years=1972:2025)
mvn$labels # the out put is SB/SBtrg
dev.print(jpeg,paste0(plotdir,"/Kobe_.jpg"), width = 6.5, 
          height = 6.5, res = 300, units = "in")



## Summarize the list of retroModels
retroSummary <- r4ss::SSsummarize(retroModels)


## Retro analysis index --------------------------------------------------------
pdf(paste0(plotdir,"/HCxvalIndex.pdf"), height = 6.5, width = 11)
par(mfrow=c(3,2))
SSplotHCxval(retroSummary,xmin=1953,add=T)
dev.off()
write.csv(SSplotHCxval(retroSummary,xmin=1960,add=T), file=paste0(plotdir,"/tabMASE_Ind.csv"))


## Retro analysis mean length --------------------------------------------------
hccomps = SSretroComps(retroModels)
pdf(paste0(plotdir,"/HCxvalLen.pdf"), height = 6.5, width = 11)
par(mfrow=c(3,3))
SSplotHCxval(hccomps,add=T,subplots = "len",legendloc="topleft")

dev.off()

write.csv(SSplotHCxval(hccomps,add=T,subplots = "len",legendloc="topleft"), file=paste0(plotdir,"/tabMASE_Ld.csv"))
