#~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare the data files #
# Southern hake stock    #
# Sex separated settings #
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Marta Cousido       #
# Santiago Cerviño    #
# ~~~~~~~~~~~~~~~~~~~~~
# 21/04/2023  #
# ~~~~~~~~~~~~~
rm(list=ls())
## To see the outline press: Ctrl + shift + O

# Read data files --------------------------------------------------------------
start <- r4ss::SS_readstarter(file = "boot/data/SS files template/starter.ss",
                              verbose = FALSE)
old <- r4ss::SS_readdat(file = "boot/data/SS files template/shake_data.ss",
                          verbose = FALSE)

new <- r4ss::SS_readdat(file = "data/ss files/shake_data.ss",
                        verbose = FALSE)

# Assessment year --------------------------------------------------------------

ass_y <- 2025



# Catch data -------------------------------------------------------------------

catch_o=old$catch
catch_n=new$catch

## Fleet 1 

a=subset(catch_o,catch_o$fleet=="1")
b=subset(catch_n,catch_n$fleet=="1" & catch_n$year<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)

## Fleet 2 

a=subset(catch_o,catch_o$fleet=="2")
b=subset(catch_n,catch_n$fleet=="2" & catch_n$year<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)

## Fleet 3

a=subset(catch_o,catch_o$fleet=="3")
b=subset(catch_n,catch_n$fleet=="3" & catch_n$year<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)

## Fleet 4

a=subset(catch_o,catch_o$fleet=="4")
b=subset(catch_n,catch_n$fleet=="4" & catch_n$year<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)

## Check assessment year

c=subset(catch_n,catch_n$fleet=="1" & catch_n$year==ass_y)
sum(c$catch)

c=subset(catch_n,catch_n$fleet=="2" & catch_n$year==ass_y)
sum(c$catch)

# Care don't add frArt again, is already in Art

c=subset(catch_n,catch_n$fleet=="3" & catch_n$year==ass_y)
sum(c$catch)

c=subset(catch_n,catch_n$fleet=="4" & catch_n$year==ass_y)
sum(c$catch)

c=subset(catch_n,catch_n$year==ass_y)
sum(c$catch)


# CPUE data -------------------------------------------------------------------


cpue_o=old$CPUE
cpue_n=new$CPUE

## Index 5

a=subset(cpue_o,cpue_o$index=="5")
b=subset(cpue_n,cpue_n$index=="5" & cpue_n$year<ass_y)
dim(a)
dim(b)
dif=a-b
summary(dif)

## Index 6

a=subset(cpue_o,cpue_o$index=="6")
b=subset(cpue_n,cpue_n$index=="6" & cpue_n$year<ass_y)
dim(a)
dim(b)
dif=a-b
summary(dif)

## Index 7

a=subset(cpue_o,cpue_o$index=="7")
b=subset(cpue_n,cpue_n$index=="7" & cpue_n$year<ass_y)
dim(a)
dim(b)
dif=a-b
summary(dif)

## Index 8

a=subset(cpue_o,cpue_o$index=="8")
b=subset(cpue_n,cpue_n$index=="8" & cpue_n$year<ass_y)
dim(a)
dim(b)
dif=a-b
summary(dif)

## Index 9

a=subset(cpue_o,cpue_o$index=="9")
b=subset(cpue_n,cpue_n$index=="9" & cpue_n$year<ass_y)
dim(a)
dim(b)
dif=a-b
summary(dif)

## Check assessment year

c=subset(cpue_n,cpue_n$index=="5" & cpue_n$year==ass_y);c
c=subset(cpue_n,cpue_n$index=="6" & cpue_n$year==ass_y);c
c=subset(cpue_n,cpue_n$index=="7" & cpue_n$year==ass_y);c


c=subset(cpue_n,cpue_n$index=="8");c
c=subset(cpue_n,cpue_n$index=="9");c

# Discard data -----------------------------------------------------------------

dis_o=old$discard_data
dis_n=new$discard_data
a=dis_o
b=subset(dis_n ,dis_n$Yr<ass_y)
dim(a)
dim(b)
dif=a-b
summary(dif)

## Check assessment year

c=subset(dis_n ,dis_n$Yr==ass_y)
sum(c$Discard)

# Lemcomp data -----------------------------------------------------------------

len_o=old$lencomp
len_n=new$lencomp

## Fleet 1 

a=subset(len_o,len_o$FltSvy=="1" & len_o$Part==2)
b=subset(len_n,len_n$FltSvy=="1" & len_n$Yr<ass_y & len_n$Part==2)

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)

a=subset(len_o,len_o$FltSvy=="1" & len_o$Part==1)
b=subset(len_n,len_n$FltSvy=="1" & len_n$Yr<ass_y & len_n$Part==1)

dim(a)
#ind<-which(a$Yr=="2020");a<-a[-ind,]
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Fleet 2

a=subset(len_o,len_o$FltSvy=="2")
b=subset(len_n,len_n$FltSvy=="2" & len_n$Yr<ass_y)

dim(a)
#ind<-which(a$Yr=="2005");a<-a[-ind,]
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Fleet 3

a=subset(len_o,len_o$FltSvy=="3")
b=subset(len_n,len_n$FltSvy=="3" & len_n$Yr<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Fleet 4

a=subset(len_o,len_o$FltSvy=="4")
b=subset(len_n,len_n$FltSvy=="4" & len_n$Yr<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Fleet 5

a=subset(len_o,len_o$FltSvy=="5")
b=subset(len_n,len_n$FltSvy=="5" & len_n$Yr<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Fleet 6

a=subset(len_o,len_o$FltSvy=="6")
b=subset(len_n,len_n$FltSvy=="6" & len_n$Yr<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Fleet 7

a=subset(len_o,len_o$FltSvy=="7")
b=subset(len_n,len_n$FltSvy=="7" & len_n$Yr<ass_y)

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)

## Check assessment year
par(mfcol=c(2,2))

## Fleet 1

c=subset(len_n,len_n$FltSvy=="4" & len_n$Yr>=(ass_y-3) & len_n$Yr<=ass_y & len_n$Part==0)
vec=new$lbin_vector
# Season 1
c1=subset(c,c$Seas==2.5)
d1 <- as.vector(as.numeric(c1[1,7:(length(vec)+6)]))
d2 <- as.vector(as.numeric(c1[2,7:(length(vec)+6)]))
d3 <- as.vector(as.numeric(c1[3,7:(length(vec)+6)]))
d4 <- as.vector(as.numeric(c1[4,7:(length(vec)+6)]))
maxi=max(c(d1,d2,d3,d4))
plot(vec,d1, ylim=c(0,maxi), main="Fleet 1 Season 1",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)




# Season 2
c1=subset(c,c$Seas==5.5)
d1 <- as.vector(as.numeric(c1[1,7:(length(vec)+6)]))
d2 <- as.vector(as.numeric(c1[2,7:(length(vec)+6)]))
d3 <- as.vector(as.numeric(c1[3,7:(length(vec)+6)]))
d4 <- as.vector(as.numeric(c1[4,7:(length(vec)+6)]))
maxi=max(c(d1,d2,d3,d4))
plot(vec,d1, ylim=c(0,maxi), main="Fleet 1 Season 1",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)




# Season 3
c1=subset(c,c$Seas==8.5)
d1 <- as.vector(as.numeric(c1[1,7:(length(vec)+6)]))
d2 <- as.vector(as.numeric(c1[2,7:(length(vec)+6)]))
d3 <- as.vector(as.numeric(c1[3,7:(length(vec)+6)]))
d4 <- as.vector(as.numeric(c1[4,7:(length(vec)+6)]))
maxi=max(c(d1,d2,d3,d4))
plot(vec,d1, ylim=c(0,maxi), main="Fleet 1 Season 3",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)

# Season 4
c1=subset(c,c$Seas==11.5)
d1 <- as.vector(as.numeric(c1[1,7:(length(vec)+6)]))
d2 <- as.vector(as.numeric(c1[2,7:(length(vec)+6)]))
d3 <- as.vector(as.numeric(c1[3,7:(length(vec)+6)]))
d4 <- as.vector(as.numeric(c1[4,7:(length(vec)+6)]))
maxi=max(c(d1,d2,d3,d4),na.rm = TRUE)
plot(vec,d1, ylim=c(0,maxi), main="Fleet 1 Season 4",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)

legend('top', legend=c1$Yr,
       col=c("black", "skyblue","green","orange"),lty=1,  cex=0.5, horiz = TRUE,seg.len=1, bty = 'n')


## Global ----------------------------------------------------------------------
par(mfcol=c(1,1))
c1=subset(len_n, len_n$Yr>=(ass_y-5) & len_n$Yr<=ass_y & len_n$Part==2)
c2=subset(len_n, len_n$Yr>=(ass_y-5) & len_n$Yr<=ass_y & len_n$Part==0 & len_n$FltSvy<=4)
c=rbind(c1,c2)

ind1=which(c$Yr==(ass_y-5))
ind2=which(c$Yr==(ass_y-4))
ind3=which(c$Yr==(ass_y-3))
ind4=which(c$Yr==(ass_y-2))
ind5=which(c$Yr==(ass_y-1))
ind6=which(c$Yr==ass_y)

d1 <- as.vector(as.numeric(colSums(c[ind1,7:(length(vec)+6)])))
d2 <- as.vector(as.numeric(colSums(c[ind2,7:(length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3,7:(length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4,7:(length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5,7:(length(vec)+6)])))
d6 <- as.vector(as.numeric(colSums(c[ind6,7:(length(vec)+6)])))
maxi=max(c(d1,d2,d3,d4,d5,d6))
plot(vec,d1, ylim=c(0,maxi), main="Landings",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)
lines(vec,d6, ylim=c(0,maxi), col='brown',lwd=1.5)

legend('top', legend=unique(c$Yr),
       col=c("black", "skyblue","green","orange","pink","brown"),
       lty=1,lwd=2,  cex=0.4, horiz = TRUE, seg.len=1, bty = 'n')


## By fleet ----------------------------------------------------------------------

par(mfcol=c(1,1))
c=subset(len_n, len_n$Yr>=(ass_y-4) & len_n$Yr<=ass_y & len_n$Part==0&len_n$FltSvy==4)


ind1=which(c$Yr==(ass_y-4))
ind2=which(c$Yr==(ass_y-3))
ind3=which(c$Yr==(ass_y-2))
ind4=which(c$Yr==(ass_y-1))
ind5=which(c$Yr==(ass_y))


d1 <- as.vector(as.numeric(colSums(c[ind1[4],7:(length(vec)+6)])))
d2 <- as.vector(as.numeric(colSums(c[ind2[4],7:(length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3[4],7:(length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4[4],7:(length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5[4],7:(length(vec)+6)])))

maxi=max(c(d1,d2,d3,d4,d5))
plot(vec,d1, ylim=c(0,maxi), main="Discards Fleet 1",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)


legend('top', legend=unique(c$Yr),
       col=c("black", "skyblue","green","orange","pink"),
       lty=1,lwd=2,  cex=0.4, horiz = TRUE,seg.len=1, bty = 'n')

## Surveys -----------------------------------------------------------

### SpSurv

c=subset(len_n, len_n$Yr==ass_y &len_n$FltSvy==5);c

### PtSurv

c=subset(len_n, len_n$Yr==(ass_y-2) &len_n$FltSvy==6);c

### CdSurv

c=subset(len_n, len_n$Yr==ass_y &len_n$FltSvy==7);c


### Plot

### SpSurv fem
par(mfcol=c(1,1))
c=subset(len_n, len_n$Yr>=(ass_y-5) & len_n$Yr<=ass_y & len_n$FltSvy==5)


ind1=which(c$Yr==(ass_y-5))
ind2=which(c$Yr==(ass_y-4))
ind3=which(c$Yr==(ass_y-3))
ind4=which(c$Yr==(ass_y-2))
ind5=which(c$Yr==(ass_y-1))
ind6=which(c$Yr==ass_y)

d1 <- as.vector(as.numeric(colSums(c[ind1,7:(length(vec)+6)])))
d2 <- as.vector(as.numeric(colSums(c[ind2,7:(length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3,7:(length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4,7:(length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5,7:(length(vec)+6)])))
d6 <- as.vector(as.numeric(colSums(c[ind6,7:(length(vec)+6)])))
maxi=max(c(d1,d2,d3,d4,d5,d6))
plot(vec,d1, ylim=c(0,maxi), main="Female Spsurv",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)
lines(vec,d6, ylim=c(0,maxi), col='brown',lwd=1.5)

legend('top', legend=unique(c$Yr),
       col=c("black", "skyblue","green","orange","pink","brown"),lty=1,lwd=2,  cex=0.4, horiz = TRUE,seg.len=1, bty = 'n')

### PtSurv fem
par(mfcol=c(1,1))
c=subset(len_n, len_n$Yr>=(ass_y-6) & len_n$Yr<=(ass_y-1) & len_n$FltSvy==6)


#ind1=which(c$Yr==(ass_y-6))
#ind2=which(c$Yr==(ass_y-5))
ind3=which(c$Yr==(ass_y-4))
ind4=which(c$Yr==(ass_y-3))
ind5=which(c$Yr==(ass_y-2))
#ind6=which(c$Yr==(ass_y-1))

#d1 <- as.vector(as.numeric(colSums(c[ind1,7:(length(vec)+6)])))
#d2 <- as.vector(as.numeric(colSums(c[ind2,7:(length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3,7:(length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4,7:(length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5,7:(length(vec)+6)])))
#d6 <- as.vector(as.numeric(colSums(c[ind6,7:(length(vec)+6)])))
maxi=max(c(d3,d4,d5))
plot(vec,d3, ylim=c(0,maxi), main="Female Ptsurv",type="l",xlab="",ylab="",lwd=1.5)
#lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)
#lines(vec,d6, ylim=c(0,maxi), col='brown',lwd=1.5)

legend('top', legend=unique(c$Yr),
       col=c("green", "orange","pink"),
       lty=1,lwd=2,  cex=0.4, horiz = TRUE,seg.len=1, bty = 'n')

### CdSurv
par(mfcol=c(1,1))
c=subset(len_n, len_n$Yr>=(ass_y-5) & len_n$Yr<=ass_y & len_n$FltSvy==7)


ind1=which(c$Yr==(ass_y-5))
#ind2=which(c$Yr==(ass_y-4))
ind3=which(c$Yr==(ass_y-3))
ind4=which(c$Yr==(ass_y-2))
ind5=which(c$Yr==(ass_y-1))
ind6=which(c$Yr==ass_y)


d1 <- as.vector(as.numeric(colSums(c[ind1,7:(length(vec)+6)])))
#d2 <- as.vector(as.numeric(colSums(c[ind2,7:(length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3,7:(length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4,7:(length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5,7:(length(vec)+6)])))
d6 <- as.vector(as.numeric(colSums(c[ind6,7:(length(vec)+6)])))

maxi=max(c(d1,d3,d4,d5,d6))
plot(vec,d1, ylim=c(0,maxi), main="Cdsurv",type="l",xlab="",ylab="",lwd=1.5)
#lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)
lines(vec,d6, ylim=c(0,maxi), col='brown',lwd=1.5)


legend('top', legend=unique(c$Yr),
       col=c("black","green","orange",'pink',"brown"),
       lty=1,lwd=2,  cex=0.4, horiz = TRUE,seg.len=1, bty = 'n')

### SpSurv mal
par(mfcol=c(1,1))
c=subset(len_n, len_n$Yr>=(ass_y-5) & len_n$Yr<=ass_y & len_n$FltSvy==5)


ind1=which(c$Yr==(ass_y-5))
ind2=which(c$Yr==(ass_y-4))
ind3=which(c$Yr==(ass_y-3))
ind4=which(c$Yr==(ass_y-2))
ind5=which(c$Yr==(ass_y-1))
ind6=which(c$Yr==ass_y)

d1 <- as.vector(as.numeric(colSums(c[ind1,(length(vec)+7):(2*length(vec)+6)])))
d2 <- as.vector(as.numeric(colSums(c[ind2,(length(vec)+7):(2*length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3,(length(vec)+7):(2*length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4,(length(vec)+7):(2*length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5,(length(vec)+7):(2*length(vec)+6)])))
d6 <- as.vector(as.numeric(colSums(c[ind6,(length(vec)+7):(2*length(vec)+6)])))
maxi=max(c(d1,d2,d3,d4,d5,d6))
plot(vec,d1, ylim=c(0,maxi), main="male Spsurv",type="l",xlab="",ylab="",lwd=1.5)
lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)
lines(vec,d6, ylim=c(0,maxi), col='brown',lwd=1.5)

legend('top', legend=unique(c$Yr),
       col=c("black", "skyblue","green","orange","pink","brown"),
       lty=1,lwd=2,  cex=0.4, horiz = TRUE,seg.len=1, bty = 'n')

### PtSurv mal
par(mfcol=c(1,1))
c=subset(len_n, len_n$Yr>=(ass_y-6) & len_n$Yr<=(ass_y-1) & len_n$FltSvy==6)


#ind1=which(c$Yr==(ass_y-6))
#ind2=which(c$Yr==(ass_y-5))
ind3=which(c$Yr==(ass_y-4))
ind4=which(c$Yr==(ass_y-3))
ind5=which(c$Yr==(ass_y-2))
#ind6=which(c$Yr==(ass_y-1))

#d1 <- as.vector(as.numeric(colSums(c[ind1,(length(vec)+7):(2*length(vec)+6)])))
#d2 <- as.vector(as.numeric(colSums(c[ind2,(length(vec)+7):(2*length(vec)+6)])))
d3 <- as.vector(as.numeric(colSums(c[ind3,7:(length(vec)+6)])))
d4 <- as.vector(as.numeric(colSums(c[ind4,7:(length(vec)+6)])))
d5 <- as.vector(as.numeric(colSums(c[ind5,(length(vec)+7):(2*length(vec)+6)])))
#d6 <- as.vector(as.numeric(colSums(c[ind6,(length(vec)+7):(2*length(vec)+6)])))
maxi=max(c(d3,d4,d5))
plot(vec,d3, ylim=c(0,maxi), main="male Ptsurv",type="l",xlab="",ylab="",lwd=1.5)
#lines(vec,d2, ylim=c(0,maxi), col='skyblue',lwd=1.5)
lines(vec,d3, ylim=c(0,maxi), col='green',lwd=1.5)
lines(vec,d4, ylim=c(0,maxi), col='orange',lwd=1.5)
lines(vec,d5, ylim=c(0,maxi), col='pink',lwd=1.5)
#lines(vec,d6, ylim=c(0,maxi), col='brown',lwd=1.5)

legend('top', legend=unique(c$Yr),
       col=c("green", "orange","pink"),
       lty=1,lwd=2,  cex=0.4, horiz = TRUE,seg.len=1, bty = 'n')



# Extra lencomp data - SOP -----------------------------------------------------

a <- 0.00000659  
b <- 3.01721
NLbins<-c(seq(from=4, to=40, by=1)[1:36]+0.5, seq(from=40, to=100, by=2)+1)

# Fleet 1

aux2=subset(len_n,len_n$FltSvy=="1" & len_n$Yr==ass_y & len_n$Part==2)
aux2


aux<-colSums(aux2[,7:(length(NLbins)+6)])

sum(aux * a * (NLbins+ 0.5) ^ b) 

c=subset(catch_n,catch_n$fleet=="1" & catch_n$year==ass_y)
sum(c$catch)

sum(aux * a * (NLbins+ 0.5) ^ b) /sum(c$catch)
# Fleet 2

aux2=subset(len_n,len_n$FltSvy=="2" & len_n$Yr==ass_y & len_n$Part==0)
aux2


aux<-colSums(aux2[,7:(length(NLbins)+6)])

sum(aux * a * (NLbins+ 0.5) ^ b) 

c=subset(catch_n,catch_n$fleet=="2" & catch_n$year==ass_y)
sum(c$catch)
sum(aux * a * (NLbins+ 0.5) ^ b) /sum(c$catch)
# Fleet 3

aux2=subset(len_n,len_n$FltSvy=="3" & len_n$Yr==(ass_y) & len_n$Part==0)
aux2


aux<-colSums(aux2[,7:(length(NLbins)+6)])

sum(aux * a * (NLbins+ 0.5) ^ b) 

c=subset(catch_n,catch_n$fleet=="3" & catch_n$year==(ass_y-1))
sum(c$catch)
sum(c$catch)/sum(aux * a * (NLbins+ 0.5) ^ b) 
# Fleet 4
aux2=subset(len_n,len_n$FltSvy=="4" & len_n$Yr==ass_y & len_n$Part==0)
aux2


aux<-colSums(aux2[,7:(length(NLbins)+6)])

sum(aux * a * (NLbins+ 0.5) ^ b) 
c=subset(catch_n,catch_n$fleet=="4" & catch_n$year==ass_y)
sum(c$catch)
sum(c$catch)/sum(aux * a * (NLbins+ 0.5) ^ b) 
# Discards
aux2=subset(len_n,len_n$FltSvy=="1" & len_n$Yr==ass_y & len_n$Part==1)
aux2


aux<-colSums(aux2[,7:(length(NLbins)+6)])

sum(aux * a * (NLbins+ 0.5) ^ b) 
c=subset(dis_n ,dis_n$Yr==ass_y)
sum(c$Discard)
sum(c$Discard)/sum(aux * a * (NLbins+ 0.5) ^ b) 

# Sizefreq data -----------------------------------------------------------------

len_o=old$sizefreq_data_list[[1]]
len_n=new$sizefreq_data_list[[1]]



a=len_o
b=len_n

dim(a)
dim(b)
dif=a-b
summary(dif)
sum(dif)


# Additional check due to changes in the historical Ptsurv------------------

len_o=old$lencomp
len_n=new$lencomp

c <- subset(len_n,len_n$FltSvy == 6)

c_o <- subset(len_o,len_o$FltSvy == 6)
years <- sort(unique(c$Yr))

pdf("data/LFDs/Ptsurv_LFDs_females_ind.pdf", width = 10, height = 8)
plots_por_pagina <- 6  

for (i in seq(1, length(years), by = plots_por_pagina)) {
  
  years_subset <- years[i:min(i + plots_por_pagina - 1, length(years))]
  
  par(mfrow = c(ceiling(length(years_subset)/2), 2),
      mar = c(3, 3, 2, 1))
  
  for (y in years_subset) {
    
    c_year <- subset(c, Yr == y)
    o_year <- subset(c_o, Yr == y)
    
    d_n <- as.vector(as.numeric(colSums(c_year[, 7:(length(vec)+6)])))
    
    if (nrow(o_year) > 0) {
      d_o <- as.vector(as.numeric(colSums(o_year[, 7:(length(vec)+6)])))
    } else {
      d_o <- rep(NA, length(vec))
    }
    
    maxi <- max(c(d_n, d_o), na.rm = TRUE)
    
    plot(vec, d_n,
         type = "l",
         ylim = c(0, maxi),
         main = paste("Year", y),
         col = "blue",
         lwd = 1.5)
    
    lines(vec, d_o, col = "red", lwd = 1.5)
    
    legend("topright",
           legend = c("new", "old"),
           col = c("blue", "red"),
           lty = 1,
           lwd = 2,
           bty = "n",
           cex = 0.7)
  }
  

  readline("Press Enter...")
}
dev.off()


pdf("data/LFDs/Ptsurv_LFDs_males_ind.pdf", width = 10, height = 8)
plots_por_pagina <- 6  

for (i in seq(1, length(years), by = plots_por_pagina)) {
  
  years_subset <- years[i:min(i + plots_por_pagina - 1, length(years))]
  
  par(mfrow = c(ceiling(length(years_subset)/2), 2),
      mar = c(3, 3, 2, 1))
  
  for (y in years_subset) {
    
    c_year <- subset(c, Yr == y)
    o_year <- subset(c_o, Yr == y)
    
    d_n <- as.vector(as.numeric(colSums(c_year[, (length(vec)+7):(2*length(vec)+6)])))
    
    if (nrow(o_year) > 0) {
      d_o <- as.vector(as.numeric(colSums(o_year[, (length(vec)+7):(2*length(vec)+6)])))
    } else {
      d_o <- rep(NA, length(vec))
    }
    
    maxi <- max(c(d_n, d_o), na.rm = TRUE)
    
    plot(vec, d_n,
         type = "l",
         ylim = c(0, maxi),
         main = paste("Year", y),
         col = "blue",
         lwd = 1.5)
    
    lines(vec, d_o, col = "red", lwd = 1.5)
    
    legend("topright",
           legend = c("new", "old"),
           col = c("blue", "red"),
           lty = 1,
           lwd = 2,
           bty = "n",
           cex = 0.7)
  }
  
  
  readline("Press Enter...")
}
dev.off()

