opt <- 'f' #f = fullpop, cf = fisherysf
# setwd('m:/f26/')
#setwd("/home/jtang/.gvfs/home0 on net/jtang/R/SAMS_Plot/sams/data")
setwd("SETWD_REPLACEMENT")
# sf1 <- read.csv(paste(opt,'newcl3.csv',sep=''),header=T)
#sf1 <- read.csv(paste(opt,'basev3.csv',sep=''),header=T)
#sf1 <- read.csv('basev3.dat',header=T)
#sf1 <- read.csv('bootbasev3.dat',header=T)
sf1 <- read.csv('f_samsout.dat',header=T)
maareas <- c(1255.6, 130.5, 670.2, 486.5, 741.7, 1408.4, 3867.5, 1735)
gbareas <- c(331.5, 341.8, 383.5, 993.8, 581.27, 346.4, 582.8, 104.9, 1320, 361.2, 1192.3)  
areas <- cbind(c(maareas,rep(NA,3)),gbareas)
gbopsubs = c(7:11)
maopsubs = c(2,6,7,8)
colors = c('darkblue','darkred','dark green')
linetypes = c(1,1,2)
linewidths = c(3,2,2)
#sf1 <- read.csv(paste('i:/f24/',opt,'alt21.csv',sep=''),header=T)
#modelyears <- c(2012,2013,2014)
modelyears <- c(2014,2015,2016)
sf1$Reg <- gsub(" ","",sf1$Reg)  #remove spaces
sf1$Sreg <- gsub(" ","",sf1$Sreg)
nreg <- length(levels(as.factor(sf1$Reg)))-1
nsubreg <- length((levels(as.factor(sf1$Sreg))))-3
nyears <- max(sf1$Year) - min(sf1$Year) + 1
nsh <- (165-40)/5 + 1
Reg_Combo <- unique(sf1[,c("Reg","Sreg")])
#Reg_Combo <- sapply(Reg_Combo,as.numeric)
Reg_Combo <- Reg_Combo[Reg_Combo$Sreg %in% as.character(1:20),]
meansf <-  matrix(rep(0,dim(Reg_Combo)[1]*nyears*(nsh+3)),ncol=nsh+3)
rowcount = 1
reg <- Reg_Combo$Reg
subreg <- Reg_Combo$Sreg
  for (i in 1:dim(Reg_Combo)[1]){
    for (year in min(sf1$Year):max(sf1$Year))
      {
        temp <- as.matrix(subset(sf1,(Reg == as.character(reg[i])) & (Sreg == as.character(subreg[i])) & (Year == year)))
        tempn <- matrix(as.numeric(temp[,c(-31,-4:-1)]),nrow=dim(temp)[1])
        thissf <- apply(tempn,2,mean,na.rm=T)
        meansf[rowcount,1:3] <- c(reg[i],subreg[i],year)
        meansf[rowcount,-3:-1] <- thissf
        rowcount <- rowcount + 1
      }
  }


# meansf <- matrix(rep(0,nreg*nsubreg*nyears*(nsh+3)),ncol=nsh+3)
# rowcount = 1
# for (reg in 1:nreg){
#   for (subreg in 1:nsubreg){
#     for (year in min(sf1$Year):max(sf1$Year))
#       if(!(reg == 1 & subreg > 7)){
#       temp <- as.matrix(subset(sf1,(Reg == as.character(reg)) & (Subreg == as.character(subreg)) & (Year == year)))
#       tempn <- matrix(as.numeric(temp[,c(-31,-4:-1)]),nrow=dim(temp)[1])
#       thissf <- apply(tempn,2,mean,na.rm=T)
#       meansf[rowcount,1:3] <- c(reg,subreg,year)
#       meansf[rowcount,-3:-1] <- thissf
#       rowcount <- rowcount + 1
#       }
#   }
# }
sh <- seq(40,165,5)
colnames(meansf) <- c('Reg','Sub','Year',sh)
meandf <- data.frame(meansf, stringsAsFactors=F)
sh <- sh + 2.5
year <- 2014
#if (.Platform$OS.type=="windows") windows(height=8,width=11,record=T) else X11(height=8,width=11)
pdf(file='test.pdf', height=8, width=11)
reg <- 1
masub <- c('Hudson Canyon S','Virginia Beach','Elephant Trunk Open','Elephant Trunk Closed','Delmarva','New York Bight','Long Island','Inshore NYB')

for (subarea in 1:length(masub)){
   year <- modelyears[1]
   sff <- as.numeric(subset(meandf,Year == year & Reg == reg & Sub == subarea))
   sf <- sff[-3:-1]
   plot(sf~sh,type='l',xlab='Shell height (mm)',ylab='#/tow',lwd=3,cex.lab=1.4,main=masub[subarea],ylim=c(0,1.2*max(sf)))
   year <- modelyears[2]
   sff <- as.numeric(subset(meandf,Year == year & Reg == reg & Sub == subarea))
   sf <- sff[-3:-1]
   lines(sf~sh,lwd=2,col='darkred')
   year <- modelyears[3]
   sff <- as.numeric(subset(meandf,Year == year & Reg == reg & Sub == subarea))
   sf <- sff[-3:-1]
   lines(sf~sh,col='blue3',lwd=2,lty=2)
   legend('topright',legend=modelyears,col=c('black','darkred','dark green'),lwd=c(3,2,2),lty=c(1,1,2),cex=1.8,bty='n')
   fname = paste('MA26newcl',masub[subarea],sep='-')
   if (opt == 'c')
     fname = paste('MA26c',masub[subarea],sep='-')
    #savePlot(type=ifelse(.Platform$OS.type=="windows","pdf","png"),filename = fname)
}
gbsub <- c('CL-1 NA','CL-1 Acc','CL-2 N','CL-2 S','NLS NA','NLS Acc','NLSext','CL-2 Ext','S Channel','Northern Edge','Southeast Part')
reg <- 2
for (sub in 1:length(gbsub)){
  year <- modelyears[1]
  sff <- as.numeric(subset(meandf,Year == year & Reg == reg & Sub == sub))
  sf <- sff[-3:-1]
  plot(sf~sh,type='l',xlab='Shell height (mm)',ylab='#/tow',lwd=3,cex.lab=1.4,main=gbsub[sub],col='black',ylim=c(0,max(1.2*max(sf),15)))
  year <- modelyears[2]
  sff <- as.numeric(subset(meandf,Year == year & Reg == reg & Sub == sub))
  sf <- sff[-3:-1]
  lines(sf~sh,lwd=2,col='darkred')
  year <- modelyears[3]
  sff <- as.numeric(subset(meandf,Year == year & Reg == reg & Sub == sub))
  sf <- sff[-3:-1]
  lines(sf~sh,col='blue3',lwd=2,lty=2)
  legend('topright',legend=modelyears,col=c('black','darkred','blue3'),lwd=c(3,2,2),lty=c(1,1,2),cex=1.8,bty='n')
  fname = filename = paste('GB26',gbsub[sub],sep='-')
  if (opt == 'c')
    fname = paste('GB26c',gbsub[sub],sep='-')
  #savePlot(type=ifelse(.Platform$OS.type=="windows","pdf","png"),filename=fname)
}
gbop = subset(meandf,Reg==2 & Sub %in% gbopsubs)
#gbopareas = as.numeric(areas[2,gbopsubs])
gbopareas = as.numeric(areas[gbopsubs,2])
totoparea = sum(gbopareas)
len = length(gbop[1,]) - 3 # num sh bins
numyears = length(modelyears)
gbopsf = matrix(rep(0,len*numyears),ncol=len)
rownames(gbopsf) = modelyears
colnames(gbopsf) = names(gbop)[-1:-3]
for (y in modelyears){
  sff = subset(gbop,Year==y)
  for (shh in 1:len)
    # gbopsf[y-modelyears[1]+1,shh] = as.vector(gbopareas) %*% as.vector(sff[,shh+3])/totoparea
    gbopsf[y-modelyears[1]+1,shh] = as.vector(gbopareas) %*% as.numeric(sff[,shh+3])/totoparea
}
plot(gbopsf[1,]~sh,type='l',xlab='Shell height (mm)',ylab='#/tow',lwd=3,cex.lab=1.4,main='GB Open',col='darkblue',ylim=c(0,1.1*max(sf)))
for (y in 2:numyears){
  lines(gbopsf[y,]~sh,col=colors[y],lwd=linewidths[y],lty=linetypes[y]) 
}
legend('topright',legend=modelyears,col=c('darkblue','darkred','dark green'),lwd=2,lty=c(1,1,2),cex=1.6)
if (opt == 'f')
  #savePlot(type='png',filename = paste('GBOp1315','sf',sep='-')) else
  #savePlot(type='png',filename = paste('GBOp1315','comsf',sep='-'))

maop = subset(meandf,Reg==1 & Sub %in% maopsubs)
#maopareas = as.numeric(areas[2,maopsubs])
maopareas = as.numeric(areas[maopsubs,2])
totoparea = sum(maopareas)
len = length(maop[1,]) - 3 # num sh bins
numyears = length(modelyears)
maopsf = matrix(rep(0,len*numyears),ncol=len)
rownames(maopsf) = modelyears
colnames(maopsf) = names(maop)[-1:-3]
for (y in modelyears){
  sff = subset(maop,Year==y)
  for (shh in 1:len)
#     maopsf[y-modelyears[1]+1,shh] = as.vector(maopareas) %*% as.vector(sff[,shh+3])/totoparea
    maopsf[y-modelyears[1]+1,shh] = as.vector(maopareas) %*% as.numeric(sff[,shh+3])/totoparea

}
plot(maopsf[1,]~sh,type='l',xlab='Shell height (mm)',ylab='#/tow',lwd=3,cex.lab=1.4,main='MA Open',col='darkblue',ylim=c(0,1.1*max(maopsf)))
for (y in 2:numyears){
  lines(maopsf[y,]~sh,col=colors[y],lwd=linewidths[y],lty=linetypes[y]) 
}
legend('topright',legend=modelyears,col=c('darkblue','darkred','dark green'),lwd=2,lty=c(1,1,2),cex=1.6)
if (opt == 'f')
  #savePlot(type='png',filename = paste('MAOp1315','sf',sep='-')) else
  #savePlot(type='png',filename = paste('MAOp1315','comsf',sep='-'))
    dev.off()
