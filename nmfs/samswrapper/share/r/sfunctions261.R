#fyears <<- 2009:2038
#fyears2 <<- 2009:2039

maopareas <- c(2,6,7,8)
gbopareas <- c(7:9,13)

maclsum = function(y){
  totland = (sum(catch[y+1,1,c(1,3,4,5)]))
  TripLimit = (totland*2204.6-1e6-1.2e6)*.99/3/327
  print(paste('Total ma closed landings =',round(totland)),quote=F)
  print(paste('Trip limit =',round(TripLimit)),quote=F)
}

regland <- function(land){
  maopland <- apply(catch[-1:-2,1,c(2,6,7,8)],1,sum)
  gbopland <- apply(catch[-1:-2,2,c(9,13,14)],1,sum)
  gbrotland <- apply(catch[-1:-2,2,7:8],1,sum)
  landtable = round(cbind(years[c(-1,-length(years))],maopland,gbopland,catch[-1:-2,1,c(1,3,4,5)],gbrotland,catch[-1:-2,2,c(2,4,6)]))
  colnames(landtable) = c('Year','MAOp','GBOp','HCS','ETOp','ETCl','Dmv','GBRot','CL1','CL2','NLS')
  rownames(landtable) = rep('',dim(landtable)[1])
  print(landtable)
}
tot = function(v){
  temp = v[,3,"  All"]
  return(temp)}
 
catchsum <- function()return(sum(tot(catch)[-1]))
tot1 = function(v){
  temp = v[,3,"  All"]
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears2
  return(temp2)}

op = function(v){
  temp = apply(v[,1,maopareas],1,sum) + apply(v[,2,gbopareas],1,sum)
  opbms2 = temp
  return(opbms2)}
  
sumop1 = function(v){return(apply(v[,1,maopareas],1,sum)+apply(v[,2,gbopareas],1,sum))
} #gbopareas + nyb, li,vb

sumop2 = function(v){
  return(apply(v[,1,maopareas],1,sum)+apply(v[,2,gbopareas],1,sum))}  #gbopareas + ma all
  
sumop3 = function(v){
  return(sumop2)
  }
  
op1 = function(v){
  temp = apply(v[,1,maopareas],1,sum)+apply(v[,2,gbopareas],1,sum)  #gbopareas + nyb, li,vb
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)}
  
op3 = function(v){return(op1(v))}

  op2 = function(v){return(op1(v))}

maop = function(v) { 
  temp <- apply(v[,1,maopareas],1,sum)
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)
}

gbop = function(v){
  temp = apply(v[,2,gbop],1,sum)
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)
}

 ophc = function(v){
    temp = v[,1,3] + v[,2,12]+v[,2,13]+v[,1,5]+v[,1,6]+v[,1,2] + v[,1,7]+v[,1,4]#gbop + nyb, li,vb,et,dmv all but hc
   temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)
  }


wop1 = function(F,eb){ #takes weighted average of F in current op areas
  temp <- 0
  for (subarea in maopareas){
    temp <- temp+eb[,1,subarea]*F[,1,subarea]
  }
  for (subarea in gbopareas){
    temp <- temp+eb[,2,subarea]*F[,2,subarea]
  }
  temp <- temp/sumop1(eb)
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)}

wop3 = function(F,eb){ #takes weighted average of F in current op areas
 return(wop1(F,eb))}

wop2 = function(F,ebms){#takes weighted average of F outside gf closed areas
  temp  = (ebms[,2,12]*F[,2,12]+ebms[,2,13]*F[,2,13]+ebms[,1,10]*F[,1,10])/sumop2(ebms)
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)}
accsum = function(catch) {
   return(apply(catch[,2,1:6],1,sum)+apply(catch[,1,c(1,3,4)],1,sum))
   }
  
opma = function(v){
  temp = apply(v[,1,maopareas],1,sum)
  return(temp)
  }
  
opma1 = function(v){
  temp = opma(v)
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)}

opma2 = function(v){
  temp = v[,1,10]
  temp2 = c(temp[2:length(temp)],temp[1])
  is.na(temp2[length(temp2)]) <- TRUE
  #names(temp2)= fyears
  return(temp2)}

shiftyears = function(v){
  temp = c(v[2:length(v)],v[1])
  is.na(temp[length(temp)]) = TRUE
  names(temp) = fyears2
  return(temp)}

#gbyt1050 <- rep(0.67,numyears)
#gbyt1050[2:4] <- c(0.562,0.613,0.625)
#gbyt5090 <- rep(1.5,numyears)
#gbyt5090[2:4] <- c(1.555,1.5,1.59)

gbytop = function(ebms,land){
  baserate = 0.018588 #baseline bycatch rate yt live/scallops meats in 2012
  scallopbms = ebms[,2,9]+ebms[,2,13] + ebms[,2,7]*0.1+ebms[,2,8]*0.15;  #scallop bms in area
  scal2012 <- 3820
  yt2012 <- 869
  temp = land[,2,9]+land[,2,13] + land[,2,7]*0.1+land[,2,8]*0.15; 
  scallopland = c(temp[2:length(temp)]);  #scallop landings in area
  is.na(scallopland[length(scallopland)]) <- TRUE
  dimebms = dim(ebms);
  numyears = dimebms[1];
  gby = rep(0,numyears);
  gby[1:3] = c(706,599,1879)     #projected yt bms
  gby[4:numyears] = 2500;   #assume yt bms stays the same for rest of time period
  gboprate <<- baserate*gby/scallopbms*scal2012/yt2012;
  gbopscalland <<- scallopland
  print(paste("gbop: ",scallopland))
  bycatch = gboprate[1:length(scallopland)]*scallopland
  names(bycatch)= fyears
  names(gboprate) = fyears                            
  return(bycatch)}

#GBdk dk1 0.003361999  dk2 0.054101216	dkop 0.01858823



northwpop = function(ebmsmt,catch){
  baserate = 0.00468 #baseline bycatch rate wp live/scallops meats in 2012
  scallopbms = ebmsmt[,2,9]+ebmsmt[,2,13] + ebmsmt[,2,7]+ebmsmt[,2,8]*0.9;  #scallop bms in area
  scal2012 = 15652
  temp = catch[,2,9]+catch[,2,13] + catch[,2,7]+catch[,2,8]*0.9; 
  scallopland = c(temp[2:length(temp)]);  #scallop landings in area
  is.na(scallopland[length(scallopland)]) <- TRUE
  dimebms = dim(ebms);
  numyears = dimebms[1];
  gboprate <<- baserate/scallopbms*scal2012
  
  gbopscalland <<- scallopland
  print(paste("gbop: ",scallopland))
  bycatch <- gboprate[1:length(scallopland)]*scallopland
  names(bycatch)= fyears
  print(bycatch)
  names(gboprate) = fyears                            
  return(bycatch)}

gbwpcl = function(ebms,catch,carea){     #closed area gb yellowtail bycatch
  baserate1 = 0.0066; #baseline bycatch rate in cl1 in 2012
  baserate2 = 0.0013;    #baseline bycatch rate in cl2 in 2012
  scal12012 = 5554 #scallop ebms in cl1 acc in 2012
  scal22012 = 2501 #scallop ebms in cl2 acc in 2012
  dimebms = dim(ebms);
  numyears = dimebms[1];
  cl1rate <<- baserate1/ebms[,2,2]*scal12012;  #rate in cl 1
  cl2rate = baserate2/ebms[,2,4]*scal22012;  #rate in cl 2
  gbclscalland <<- apply(catch[2:numyears,2,1:4],1,sum)
  print(paste("gbcl: ",gbclscalland))
  if (carea == 2) 
    bycatch = cl2rate[1:(numyears-1)]*apply(catch[2:numyears,2,c(3,4)],1,sum) 
  if (carea == 1)
    bycatch = cl1rate[1:(numyears-1)]*apply(catch[2:numyears,2,c(1,2)],1,sum)*0.8; #CL1 access 80% in
  names(bycatch)= fyears;
  print(bycatch)
  return(bycatch)}
  
gbytcl = function(ebms,catch,carea){     #closed area gb yellowtail bycatch
  baserate1 = 0.00336; #baseline bycatch rate in cl1 in 2012
  baserate2 = 0.0541;    #baseline bycatch rate in cl2 in 2012
  #scal12012 = 4000; #scallop ebms in cl1 acc in 2005
  #scal22012 = 5000;  #scallop ebms in cl2 acc in 2009
  scal12012 <- 5554
  scal22012 <- 2501
  yt2012 = 869
  dimebms = dim(ebms);
  numyears = dimebms[1];
  gby = rep(0,numyears);
  gby[1:3] = c(706,599,1879)   #projected yt bms
  gby[4:numyears] = 2500;   #assume yt bms stays the same for rest of time period
  cl1rate <<- baserate1*gby/ebms[,2,2]*scal12012/yt2012;  #rate in cl 1
  cl2rate = baserate2*gby/ebms[,2,4]*scal22012/yt2012;  #rate in cl 2
  gbclscalland <<- apply(catch[2:numyears,2,1:4],1,sum)
    print(paste("gbcl: ",gbclscalland))
  if (carea == 2) 
      bycatch = cl2rate[1:(numyears-1)]*apply(catch[2:numyears,2,c(3,4)],1,sum) 
  if (carea == 1)
      bycatch = cl1rate[1:(numyears-1)]*apply(catch[2:numyears,2,c(1,2)],1,sum)*0.8; #CL1 access 80% in
  names(bycatch)= fyears;
  return(bycatch)}
  
ccyt = function(ebms,catch){
  baserate = 0.01 #cc bycatch rate in 2008
  yt2008 = 3366;
  scal2008 = 4783;
  scallopbms1 = ebms[,2,7]*0.8+ebms[,2,8]*0.7+ebms[,2,2]*0.2; #scallop bms in cc yt stock area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  dimebms = dim(ebms);
  numyears = dimebms[1];
  tempcatch = (catch[,2,7]*0.8+catch[,2,8]*0.7+catch[,2,2]*0.2);  
  scallopland = c(tempcatch[2:length(tempcatch)]); #scallop landings in cc area
  ccyt = rep(0,length(scallopland));
  ccyt[1:3] = c(5326,6353,7031);
  ccyt[4:length(scallopland)] = 7031;
  ccscalcatch <<- scallopland
  print(paste("cc: ",scallopland))
  rate = baserate*ccyt/scallopbms*scal2008/yt2008  
  bycatch = baserate*ccyt/scallopbms*scal2008/yt2008*scallopland
  names(bycatch)= fyears
  return(bycatch)}

nlsyt = function(ebms,catch){
  baserate = 0.00635; #bycatch rate in 2012
  yt2012 = 5345;  #sne yt bms in 2012
  scal2012 = 8208;
  scallopbms1 = ebms[,2,6]; #bms in nls access area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  numyears = length(scallopbms);
  sneyt = rep(0,numyears);
  sneyt[1:4] = c(5345,5436,5551,5541);
  sneyt[5:numyears] = 5430;
  rate = baserate*sneyt/scallopbms*scal2012/yt2012;         
  bycatch = rate*apply(catch[2:(numyears+1),2,5:6],1,sum)
  names(bycatch)= fyears
  return(bycatch)}

nlswp = function(ebms,catch){
  baserate = 0.0417; #bycatch rate in 2012
  scal2012 = ebms[1,2,6]*1.5;
  scallopbms1 = ebms[,2,6]; #bms in nls access area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  numyears = length(scallopbms);
  rate = baserate/scallopbms*scal2012;         
  bycatch = rate*apply(catch[2:(numyears+1),2,5:6],1,sum)
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)}

hcwp = function(ebms,catch){
  baserate = 8.97e-5 #bycatch rate in 2012
  scal2012 = ebmsmt[1,1,1]*2.22
  scallopbms1 = ebms[,1,1]; #bms in hc
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  numyears = length(scallopbms);
  rate = baserate/scallopbms*scal2012;         
  bycatch = rate*catch[2:(numyears+1),1,1]
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)}

snewp = function(ebms,catch){
  baserate = 0.001; #bycatch rate in 2013
  scallopbms1 = apply(ebmsmt[,1,6:7],1,sum)+ebmsmt[,2,8]*0.1 #bms in nls access area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  scal2013 <- scallopbms[1]
  numyears = length(scallopbms);
  rate = baserate/scallopbms*scal2013         
  bycatch = rate*(apply(catch[2:(numyears+1),1,6:7],1,sum)+catch[2:(numyears+1),2,8]*0.1)
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)}

dmvwp = function(ebms,catch){
  baserate = 9.7e-5
  scallopbms1 = ebmsmt[,1,4]
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  scal2012 <- scallopbms[1]*1.5
  numyears = length(scallopbms);
  rate = baserate/scallopbms*scal2012
  bycatch = rate*catch[2:(numyears+1),1,4]
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)
}

etwp = function(ebms,catch){
  baserate = 0.03207
  scallopbms1 = ebmsmt[,1,3]
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  scal2012 <- scallopbms[1]/1.5
  numyears = length(scallopbms);
  rate = baserate/scallopbms*scal2012
  bycatch = rate*catch[2:(numyears+1),1,3]
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)
}

maopwp = function(ebms,catch){
  baserate = 0.0107; #bycatch rate in 2012
  scal2012 = ebmsmt[1,1,5]*1.333
  scallopbms1 = ebmsmt[,1,5]  #bms in nls access area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  numyears = length(scallopbms);
  rate = baserate/scallopbms*scal2012         
  bycatch = rate*catch[2:(numyears+1),1,5]
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)}


etyt = function(ebms,catch){
  baserate = 8.33*7.57e-6; #bycatch rate in 2008
  yt2008 = 3703;  #sne yt bms in 2008
  scal2008 = 18700;
  scallopbms1 = ebms[,1,3]; #bms in et access area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  numyears = length(scallopbms);
  sneyt = rep(0,numyears);
   sneyt[1:4] = c(5345,5436,5551,5541);
  sneyt[5:numyears] = 5430;
  rate = baserate*sneyt/scallopbms*scal2008/yt2008;
  bycatch = rate*catch[2:(numyears+1),1,3]
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)}

hcyt = function(ebms,catch){
  #baserate = 9.67e-05; #bycatch rate in 2007
  baserate = 3.917e-5 #bycatch rate in 2012
  yt2012 = 5305;  #sne yt bms in 2007
  scallopbms1 = ebms[,1,1]; #bms in hc access area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)]
  scal2012 = scallopbms[1]*2.22
  numyears = length(scallopbms);
  sneyt = rep(0,numyears);
sneyt[1:4] = c(5345,5436,5551,5541);
  sneyt[5:numyears] = 5430;
  rate = baserate*sneyt/scallopbms*scal2012/yt2012;
  bycatch = rate*catch[2:(numyears+1),1,1]
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)} 


sneopyt = function(ebms,catch){
  baserate <- 0.03
  #baserate = 0.008; #baseline rate in 2012
  yt2012 = 5305;
  templandsne = 0.15*catch[,2,7]+0.2*catch[,2,8]# + apply(catch[,1,c(2,5,6)],1,sum);
  scalloplandsne = templandsne[2:length(templandsne)]; #scallop landings
  scallopbms1 = 0.15*ebmsmt[,2,7]+0.2*ebmsmt[,2,8] #bms in sne open area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)];
  scal2012 = scallopbms[1]/1.47 #scallop ebms in 2012
  dimebms = dim(ebms);
  numyears = dimebms[1]-1;
  sneyt = rep(0,numyears);
   sneyt[1:4] = c(5345,5436,5551,5541);
  sneyt[5:numyears] = 5430;
  snescalland <<- scalloplandsne
  print(snescalland)
  #1000print(paste("sneop ",scallopland))
  #ratesne = baserate*sneyt/scallopbmssne*scal2012/yt2012
  bycatch = baserate*scalloplandsne*sneyt/scallopbms*scal2012/yt2012;
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)}


maopyt = function(ebms,catch){
  baserate = 0.008; #baseline rate in 2011
  yt2011 = 5305;
  templand = apply(catch[,1,c(5,6,7)],1,sum);
  scallopland = templand[2:length(templand)]; #scallop landings
  scallopbms1 = apply(ebmsmt[,1,c(5,6,7)],1,sum); #bms in ma open area
  scallopbms = scallopbms1[1:(length(scallopbms1)-1)];
  scal2011 = scallopbms[1]*1.3
  dimebms = dim(ebms);
  numyears = dimebms[1]-1;
  sneyt = rep(0,numyears);
  sneyt[1:4] = c(5345,5436,5551,5541);
  sneyt[5:numyears] = 5430;
  print(scallopland)
  #1000print(paste("sneop ",scallopland))
  #ratesne = baserate*sneyt/scallopbmssne*scal2011/yt2011
  bycatch = baserate*scallopland*sneyt/scallopbms*scal2011/yt2011;
  names(bycatch)= fyears
  print(bycatch)
  return(bycatch)
} 


sneyt = function(ebms,catch){            
  return(sneopyt(ebms,catch)+hcyt(ebms,catch)+etyt(ebms,catch)+nlsyt(ebms,catch))
}  

masneopyt = function(ebms,catch){            
  return(sneopyt(ebms,catch)+maopyt(ebms,catch))
}