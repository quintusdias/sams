setwd("SETWD_REPLACEMENT")
filename <- 's_samsout.dat'
maareas <- c(1255.6, 130.5, 670.2, 486.5, 741.7, 1408.4, 3867.5, 1735)
gbareas <- c(331.5, 341.8, 383.5, 993.8, 581.27, 346.4, 582.8, 104.9, 1320, 361.2, 1192.3)  
maopareas1 <- c(2,6,7,8)
gbopareas1 <- c(7,8,9,13,14)
#gbopareas <- c(9,13,14)
options(stringAsFactors=F)
sams <- read.csv(filename,header=T,stringsAsFactors=FALSE)
Fn <- tapply(sams$Fn,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
nsubareas <- dim(Fn)[3]-3
nmasubareas <- nsubareas - 3
maxyear = max(sams$Year)
minyear = min(sams$Year)
fyears <- minyear:(maxyear-1)
fyears2 <- minyear:maxyear
years <- minyear:maxyear
nyears <- length(years)
maopareas <- matrix(rep(1,nyears*nmasubareas),nrow=nyears)
for (y in 1:nyears){
  for (sub in 1:nmasubareas){
     if (Fn[min(y+1,nyears),1,sub]==0)
       maopareas[y:min((y+3),nyears),sub] <- 0
  }
}
maopareas[,2] <- 1
maopareas[1:4,1] <- 0
maopareas[1:4,5] <- 0
gbopareas <- matrix(rep(1,nyears*nsubareas),nrow=nyears)
gbopareas[,1:6] <- 0
gbf <- Fn[,2,1:9]
if (nsubareas>9)
  gbf <- Fn[,2,c(1:9,13:(nsubareas+3))]
for (y in 1:nyears){
  for (sub in 1:nsubareas){
    if (gbf[min(y+1,nyears),sub]==0){
      gbopareas[y:min((y+3),nyears),sub] <- 0
    }
  }
}

#sams <<- read.csv("h:/f21/sa15sq.csv",header=T)
#fyears <<- 2009:2038
#fyears2 <<- 2009:2039

FTEq <<- 327;

TF <- round(tot1(Fn)*1000)/1000
bmsmt <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
allbmsmt <- round(bmsmt[,3,10])
ebmsmt <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
ebms <- tapply(sams$EBms,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
bms <- tapply(as.numeric(sams$Bms),list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
enum <- tapply(sams$ENum,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
sdebmsmt <<- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),sd)
gbop <- function(v,nsubareas,y){
  temp <- v[y,2,1:nsubareas]*gbopareas[y,]
  if (nsubareas > 9)
    temp <- v[y,2,c(1:9,13:(nsubareas+3))]*gbopareas[y,]
  return(temp)
}
gbopsum <- function(v,nsubareas){
  numyears <- dim(v)[1]
  gbo <- rep(NA,numyears)
  for (y in 1:numyears)
     gbo[y] <- sum(gbop(v,nsubareas,y),na.rm=T)
  return(gbo)
}
maop <- function(v,y)
  return(v[y,1,1:nmasubareas]*maopareas[y,])
maopsum <- function(v){
  numyears <- dim(v)[1]
  temp <- rep(NA,numyears)
  for (y in 1:numyears)
    temp[y] <- sum(maop(v,y),na.rm=T)
  return(temp)
}
gbopebmsmt = gbopsum(ebmsmt,nsubareas)
gbOpbmsmt = gbopsum(bmsmt,nsubareas)
maopebmsmt = maopsum(ebmsmt)
maOpbmsmt = maopsum(bmsmt)
opebmsmt = round(gbopebmsmt + maopebmsmt)
Opbmsmt = round(gbOpbmsmt + maOpbmsmt)
gbbmsmt = bmsmt[,2,10]
gbclbmsmt = gbbmsmt - gbOpbmsmt
mabmsmt = round(bmsmt[,1,10])
ENum <- tapply(sams$ENum,list(sams$Year,sams$Reg,sams$Sreg),mean,na.rm=T)
ENumab <- ENum*ebmsmt/ebms
ENumab[1,1,2] <- 0
sams$CatchN[sams$CatchN==sams$CatchN[1]] <- NA 
catchn = as.numeric(sams$CatchN)
catchnum <- tapply(catchn,list(sams$Year,sams$Reg,sams$Sreg),mean,na.rm=T)
expfactor <- bmsmt[-nyears,,]/bms[-nyears,,]
for (reg in 1:2)
  for (sub in 1:nsubareas){
    temp <- expfactor[,reg,sub]
    temp[is.na(temp)] <- mean(temp,na.rm=T)
    expfactor[,reg,sub] <- temp
  }
CatchNabs <- catchnum[-1,,]*expfactor
Nabs <- enum[-nyears,,]*expfactor
#CatchNabs[is.na(CatchNabs)] <- CatchNabs
maopF <- rep(NA,nyears)
gbopF <- rep(NA,nyears)
OpF <- rep(NA,nyears)
maopenum <- rep(NA,nyears)
gbopenum <- rep(NA,nyears)
maopcatchn <- matrix(rep(NA,nmasubareas*(nyears-1)),ncol=nmasubareas)
gbopcatchn <- matrix(rep(NA,nsubareas*(nyears-1)),ncol=nsubareas)
maopcatchn <- maopsum(CatchNabs)
gbopcatchn <- gbopsum(CatchNabs,nsubareas)
maopenum <- maopsum(Nabs)
gbopenum <- gbopsum(Nabs,nsubareas)
for (y in (2:length(years))){
  meanmaebms <- (ebmsmt[y,1,1:nmasubareas]+ebmsmt[y-1,1,1:nmasubareas])/2
  meanmaopebms <- sum(meanmaebms*maopareas[y-1,])
  maopF[y-1] = Fn[y,1,1:nmasubareas]%*%(meanmaebms*maopareas[y-1,])/meanmaopebms
  meangbebms <- (ebmsmt[y,2,1:nsubareas]+ebmsmt[y-1,2,1:nsubareas])/2
  meangbopebms <- (gbopebmsmt[y-1] + gbopebmsmt[y])/2
  gbopF[y-1] = (Fn[y,2,1:nsubareas]%*%meangbebms[1:nsubareas])/meangbopebms
  if (nsubareas > 9){
     meangbebms <- (ebmsmt[y,2,]+ebmsmt[y-1,2,])/2
     gbopF[y-1] <-(Fn[y,2,1:9]%*%(meangbebms[1:9]*gbopareas[y-1,1:9])+Fn[y,2,13:(nsubareas+3)]%*%(meangbebms[13:(nsubareas+3)]*gbopareas[y-1,10:nsubareas]))/meangbopebms
  }  
  OpF[y-1] <- (maopF[y-1]*meanmaopebms+gbopF[y-1]*meangbopebms)/(meanmaopebms+meangbopebms)
}
gbopF <- round(gbopF,3)
maopF <- round(maopF,3)
#
#opebmscv <<- sqrt(sdebmsmt[,2,12]^2+sdebmsmt[,1,2]^2+sdebmsmt[,1,5]^2+sdebmsmt[,1,6]^2)/opebmsmt
bms <- tapply(as.numeric(sams$Bms),list(sams$Year,sams$Reg,sams$Sreg),mean,na.rm=T)
qebmsmin <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),min)
qebms5 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.05)) 
qebms10 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.1))  
qebms25 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.25))  
qebms50 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),median)
qebms75 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.75))  
qebms90 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.9))    
qebms01 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.01)) 
qebms001 <- tapply(sams$EBmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.001)) 

qbmsmin <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),min)
qbms5 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.05)) 
qbms10 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.1))  
qbms25 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.25))  
qbms50 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),median)
qbms75 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.75))  
qbms90 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.9))    
qbms01 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.01)) 
qbms001 <- tapply(sams$BmsMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.001)) 
qlpue5 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.05)) 
qlpue10 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.1))  
qlpue25 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.25))  
qlpue50 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),median)
qlpue75 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.75))  
qlpue90 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.9))    
qlpue01 <- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.01)) 
ebms<<- tapply(sams$ExplBms,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
sdebms <<- tapply(sams$ExplBms,list(sams$Year,sams$Reg,sams$Sreg),sd)
cvebms <<- sdebms/ebms
growth <<- tapply(sams$Growth,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
dnames <<- dimnames(Fn);
catch <<-tapply(sams$CatchMT,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T) 
qcatch10 <- tapply(sams$CatchMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.1))  
qcatch25 <- tapply(sams$CatchMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.25))  
qcatch50 <- tapply(sams$CatchMT,list(sams$Year,sams$Reg,sams$Sreg),median)
qcatch75 <- tapply(sams$CatchMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.75))  
qcatch90 <- tapply(sams$CatchMT,list(sams$Year,sams$Reg,sams$Sreg),function(x) quantile(x,0.9))    
DAS <<- tapply(sams$DAS,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
LPUE <<- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
sdLPUE <<- tapply(sams$LPUE,list(sams$Year,sams$Reg,sams$Sreg),sd)
growth <<- tot(tapply(sams$Growth,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T))
AvMC <<- tapply(sams$AvMC,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
AvWt <<- tapply(sams$AvWt,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
BotArea <<- tapply(sams$BotArea,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
CU10 <<- round(tot1(tapply(sams$CU10,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)))
C1020 <<- round(tot1(tapply(sams$C1020,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)))
C2030 <<- round(tot1(tapply(sams$C2030,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)))
C3040 <<- round(tot1(tapply(sams$C3040,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)))
C40p <<- round(tot1(tapply(sams$C40pl,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)))
recr <<- tapply(sams$Recr,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
ENum <<- tapply(sams$ENum,list(sams$Year,sams$Reg,sams$Sreg),mean,rm.na=T)
sdENum <<- tapply(sams$ENum,list(sams$Year,sams$Reg,sams$Sreg),sd)
OpArea <<- c(1408,3868,3924)
#OpENum <<- (ENum[,1,5]*OpArea[1]+ENum[,1,6]*OpArea[2]+ENum[,2,12]*OpArea[3])/sum(OpArea)
#Opbmsmt <<- round(op1(bmsmt))
Tbmsmt <<- round(tot(bmsmt))
TEbmsmt <<- round(tot(ebmsmt))
MALand <- round(shiftyears(catch[,1,10]))
GBLand <- round(shiftyears(catch[,2,10]))
GBDAS <- round(shiftyears(DAS[,2,10]))
GBOpLand <- shiftyears(round(gbopsum(catch,nsubareas)))
MAOpLand <- shiftyears(round(maopsum(catch)))
OpLand <- MAOpLand+GBOpLand
GBOpDAS <- gbopsum(DAS,nsubareas)
MAOpDAS <- maopsum(DAS)
MADAS <- round(shiftyears(DAS[,1,10]))
TLand <- round(tot1(catch))
ABC <- TLand*48/34 #assumed for year 5+
ABC[1] <- 20785
ABC[2:4] <- c(25352,31807,35000)
GCLand <- 0.055*(0.99*ABC-1.25e6/2204.6)
GBLPUE <- round(2204.6*GBLand/GBDAS)
MALPUE <- round(2204.6*MALand/MADAS)
FracOp <- round(opebmsmt/ebmsmt[,3,10]*1000)/1000
OpDAS <- shiftyears(GBOpDAS+MAOpDAS)
ClLand <- TLand - OpLand
#OpDAS1 <- op1(DAS)     #without ET
#OpDAS3 <- op3(DAS)     #with ET
#OpDAS[1] <- OpDAS1[1]
#OpDAS[2:7] <- OpDAS1[2:7]
TLand2 <- round(tot1(catch) - c(catch[-1,2,4],0))   #catch except CL2
GCOpDAS <- OpDAS*GCLand/TLand2
OpLPUE <<- round(2204.6*OpLand/OpDAS)
OpObSA <- ABC*0.01*OpLand/TLand
OpObSADAS <- OpObSA/OpLPUE*2204.6
OpRSADAS <- OpLand/TLand*1.25e6/OpLPUE
incidentaldas <- 50000/OpLPUE
LAOpDAS <- OpDAS - GCOpDAS - OpRSADAS - OpObSADAS - incidentaldas
tbms <- round(tot(bms))
tebms <<- round(tot(ebms))
#is.na(OpDAS[1]) <<- TRUE
TDAS <<- round(tot1(DAS))
TLPUE <<- round(2204.6*TLand/TDAS)
OpMC <<- round(wop2(AvMC,ebmsmt)*10)/10
TMC <<- round(tot1(AvMC)*10)/10
OpWt <<- round(wop2(AvWt,bmsmt)*10)/10
TWt <<- round(tot(AvWt)*10)/10
GBOpBotArea <- gbopsum(BotArea,nsubareas)
MAOpBotArea <- maopsum(BotArea)
OpBotArea <- GBOpBotArea+MAOpBotArea
TBotArea <- round(tot1(BotArea))
GBBotArea <- round(shiftyears(BotArea[,2,10]))
MABotArea <- round(shiftyears(BotArea[,1,10]))
#U10 <<- round(tot1(CU10))
GBF <<- round(shiftyears(Fn[,2,10])*1000)/1000
#GBOpF <<- round(shiftyears(Fn[,2,12])*1000)/1000
GBOpF <- gbopF
GBClF <<- round(shiftyears(Fn[,2,11])*1000)/1000
MAF <<- round(shiftyears(Fn[,1,10])*1000)/1000
FTDAS <<- round(LAOpDAS/FTEq,2)
OpF1 <- round(OpF,3)
maopbms <- maOpbmsmt
gbopbms <- gbOpbmsmt
maopdas <- maopsum(DAS)
gbopdas <- gbopsum(DAS,nsubareas)
maoplpue <- MAOpLand/maopdas
gboplpue <- GBOpLand/gbopdas
Opbmsmt <- round(Opbmsmt)
#OpF <- OpF3[1:length(years)]
#is.na(FTDAS[1]) <<- TRUE
#sumdf <<- data.frame(years,Tbmsmt,Opbmsmt,TEbmsmt,opebmsmt,gbbmsmt,mabmsmt,TLand,MALand,GBLand,OpLand,GBOpLand,MAOpLand,GCLand,CU10,C1020,C2030,C3040,TF,OpF3,MAF,GBF,GBOpF,GBClF,TDAS,OpDAS,FTDAS,TLPUE,OpLPUE,MALPUE,GBLPUE,TMC,OpMC,TBotArea,OpBotArea,GBBotArea,GBOpBotArea,MABotArea,MAOpBotArea)
#print(sumdf)
#demet <- data.frame(years,round(TF,2),round(OpF3,2),tbms,tebms,TMC,Tbmsmt,TEbmsmt,TLand,CU10,C1020,C2030,C3040,TLPUE,OpLPUE,MALPUE,GBLPUE,TDAS,round(OpDAS),FTDAS,TBotArea)
#names(demet) <- c('Year','TotalF','OpF','TBms','TExplBms','MeanMC','TBmsMT','TExplBmsMT','TLand','CU10','C1020','C2030','C3040','TLPUE','OpLPUE','MALPUE','GBLPUE','TDAS','OpDAS','FTDAS','TBotArea') 
#print(demet)

GBOpLPUE <- round(GBOpLand/GBOpDAS * 2204.6)
MAOpLPUE <- round(MAOpLand/MAOpDAS * 2204.6) 
GBAccLPUE <- round((GBLand-GBOpLand)/(GBDAS-GBOpDAS)*2204.6)
MAAccLPUE <- round((MALand-MAOpLand)/(MADAS-MAOpDAS)*2204.6)
#is.na(FTDAS[1]) <<- TRUE
sumdf <- data.frame(years,OpF1,TF,GBF,MAF,GBOpF,GBClF,Tbmsmt,Opbmsmt,TEbmsmt,opebmsmt,gbbmsmt,mabmsmt,TLand,round(GBLand),MALand,OpLand,GBOpLand,MAOpLand,round(GCLand),CU10,C1020,C2030,C3040,TDAS,round(OpDAS),FTDAS,TLPUE,OpLPUE,MALPUE,GBLPUE,TMC,OpMC,TBotArea,OpBotArea[1:length(years)],GBBotArea,GBOpBotArea,MABotArea,MAOpBotArea)
names(sumdf) <- c('Year','OpF','TF','GBF','MAF','GBopF','GBClF','Tbmsmt','Opbmsmt','TEbmsmt','opebmsmt','GBBmsmt','MABmsmt','TotLand','GBLand','MALand','OpLand','GBOpLand','MAOpLand','GCLand','U10','1020','2030','30+','TDAS','OpDAS','FTDAS','TLPUE','OpLPUE','MALPUE','GBLPUE','TMC','OpMC','TBotArea','OpBotArea','GBBotArea','GBOpBotArea','MABotArea','MAOpBotArea')
print(sumdf)
#adj <- c(1.09,  0.9,	0.885,	0.891,	0.895,	0.887,	0.88,	0.875,	0.876,	0.879,	0.879,	0.879,	0.879,	0.876,	0.877, 0.9)
#adj <- c(1.09,  0.9906,	0.9219,	0.8693,	0.8769,	0.9803,	0.88,	0.8961,	0.9118,	0.9231,	0.9216,	0.927,	0.931,	0.9317,	0.9297,.9)
#adj <- c(1.09,0.948,  0.902,	0.8726,	0.8952,	0.918,	0.8941,	0.8944,	0.8954,	0.8975,	0.8993,	0.902,	0.903,	0.903,	0.903,	0.9)
spreadsheet <- cbind(years,TF,round(OpF,2),round(bms[,3,10]),round(ebms[,3,10]),TMC,Tbmsmt,TEbmsmt,TLand,CU10,C1020,C2030,C3040,TLPUE,OpLPUE,MALPUE,GBLPUE,TDAS,round(OpDAS),round(FTDAS),TBotArea)
colnames(spreadsheet) <- c('Year','OverallF',  'OpF',	'Bms',	'EBms',	'MC',	'Bmsmt',	'Ebmsmt','Land','U10','1020','2030','30+','LPUE','OpLPUE','MALPUE','GBLPUE','TDAS','OpDAS','FTDAS','BotAreaSwept')
rownames(spreadsheet) <- rep('',length(spreadsheet[,1]))
print(spreadsheet)
adj <- c(1.09,0.9552,  0.9052,	0.8897,	0.9107,	0.9547,	0.8863,	0.8881,	0.892,	0.8966,	0.9009,	0.9036,	0.9036,	0.9042,	0.904,	0.9)
#adj = c(1.09,  0.9244,	0.8791,	0.8445,	0.8835,	0.9476,	0.8925,	0.8821,	0.8598,	0.8622,	0.8746,	0.8805,	0.8977,	0.9046,	0.9111,	0.9)
#adj = c(1.09,  0.5499,	0.885,	0.875,	0.896,	0.8939,	0.8909,	0.8899,	0.8931,	0.8968,	0.8974,	0.9005,	0.9011,	0.9,	0.9,	0.9)
ftarget = 0.48
adj[-1] <- round(adj[-1]*ftarget/OpF[-1],4)
madj <- as.matrix(adj[-1])
rownames(madj) <- rep("",length(adj[-1]))
print(madj)
#demet <- data.frame(years,round(TF,2),round(OpF,2),tbms,tebms,TMC,Tbmsmt,TEbmsmt,TLand,CU10,C1020,C2030,C3040,TLPUE,OpLPUE,MALPUE,GBLPUE,TDAS,round(OpDAS,0),round(FTDAS,0),TBotArea) 
#names(demet) <- c('Year','OverallF','OpF','Bms','EBms','MC','Bmsmt','Ebmsmt','Land','U10','1020','2030','30+','LPUE','OpLPUE','MALPUE','GBLPUE','TDAS','OpDAS','FTDAS','BotAreaSwept')
#print(demet)
#lpue <- data.frame(years,GBOpLPUE,MAOpLPUE,GBAccLPUE,MAAccLPUE)
