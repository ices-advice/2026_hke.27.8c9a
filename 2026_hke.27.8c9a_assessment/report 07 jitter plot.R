#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot of jitter resuts                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 26/03/2024 #
#~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido       #
# Anxo Paz            #
# Francisco Izquierdo #
# Santiago Cervino    #
#~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

rm(list=ls()) ## Clean environment

## Explanation:

# To ensure that the SS model finds a global optimum. A jittering procedure is performed
# in a supercomputer (external). More specifically, the model is run 50 times with 
# slightly different initial values. The file jit_likes.RData contains the model results of those 
# jittered runs that can invert the Hessian (25 out of 50). With this information the global
# minimum is found and the ss.par of one of the models in the minimum is used to run our final model.

# In this script the aim is to plot the likelihood values of that jitter runs.


# Load data --------------------------------------------------------------------


load(paste0(getwd(), "/boot/data/Jitter models/jit_likes.RData", sep="") )

# Extract likelihood values ----------------------------------------------------
 #witch_j_summary <- SSsummarize( jit_likes)
 #numjitter <-witch_j_summary$n
 #likes<-witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:numjitter]
# Previous code already done due to change in RData to have only likes.
numjitter<-length(likes)
# Plot -------------------------------------------------------------------------
dir_plot<-paste0(getwd(), "/report/plots/Figure 10.6.png", sep="")

png(dir_plot, width = 480, height = 480)
par(mfrow=c(1,1), mai=c(.6,.6,.3,.2), mex=.5)
print(plot(seq(1:numjitter), likes,ylab="Total likelihood",xlab = "Jitter model runs",
     ylim=c(0.97*min(na.omit(likes)),max(na.omit(likes)))) )
print(abline(h=min(na.omit(likes)), col="blue"))

print(abline(h=2611.08, col="red")) # Value coming from the model used as base to run the jitter.

dev.off()
