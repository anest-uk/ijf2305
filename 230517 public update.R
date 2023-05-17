#load('../../ijf2305/xnnn.rdata') #this defeats the idea of 'run from nowt' but is here for debug
#--------------------------------------------------------solution 1 - rc3/annual 
geo0 = #104 areas
  c("AL-", "B--", "BA-", "BB-", "BD-", "BH-", "BL-", "BN-", "BR-", 
    "BS-", "CA-", "CB-", "CF-", "CH-", "CM-", "CO-", "CR-", "CT-", 
    "CV-", "CW-", "DA-", "DE-", "DH-", "DL-", "DN-", "DT-", "DY-", 
    "E--", "EC-", "EN-", "EX-", "FY-", "GL-", "GU-", "HA-", "HD-", 
    "HG-", "HP-", "HR-", "HU-", "HX-", "IG-", "IP-", "KT-", "L--", 
    "LA-", "LD-", "LE-", "LL-", "LN-", "LS-", "LU-", "M--", "ME-", 
    "MK-", "N--", "NE-", "NG-", "NN-", "NP-", "NR-", "NW-", "OL-", 
    "OX-", "PE-", "PL-", "PO-", "PR-", "RG-", "RH-", "RM-", "S--", 
    "SA-", "SE-", "SG-", "SK-", "SL-", "SM-", "SN-", "SO-", "SP-", 
    "SR-", "SS-", "ST-", "SW-", "SY-", "TA-", "TF-", "TN-", "TQ-", 
    "TR-", "TS-", "TW-", "UB-", "W--", "WA-", "WC-", "WD-", "WF-", 
    "WN-", "WR-", "WS-", "WV-", "YO-")%>%
  data.table(rc9=.,nx=seq_along(.),lab=.)
x101= #annual
  seq.Date(
    from=as.Date('1994-12-31'),
    to=as.Date('2022-12-31'),
    by='y'
  )+
  c(
    rep(0,14),
    59, #extend 2008 2 months (GFC end)
    rep(0,13),
    31) #extend 2022
cocomkd('ver001\\07pra')
sfInit(par=T,cpus=ncpus())#//parallel
x102 <- #PRA prepare regression accrual - annual
  sfLapply(
    geo0[,unique(nx)],
    f230309a,
    geo=geo0, #partition task on rc3
    dfn=x101, #annual
    steprip=c(steprip='ver001\\03rip'), #in
    steppra=c(steppra='ver001\\07pra') #out ver001=annual
  )
sfStop()#//
stopifnot(length(dir('ver001\\07pra'))>8e3) #quick/approx check it completed
x103a <- #solve and reprice
  f230311b( #//parallel
    geo=geo0,
    steppra='ver001\\07pra',
    stepsip='ver001\\02sip'
  )#//
x103 <- x103a[c('ses','geo')] #x103a is large with BSO - just for MSE

#---------------------------------------------------------------------prep - DRC
dfnx1 <- c( #import inital values prior to update being last known good
  "1994-12-31",
  "1996-07-14","1997-01-17","1997-06-23","1998-01-27","1998-10-22",
  "1999-08-06","1999-12-03","2000-01-31","2001-10-12","2002-06-25",
  "2002-10-06","2003-02-10","2003-05-22","2003-08-09","2003-10-21",
  "2004-02-08","2004-05-05","2004-07-13","2005-02-01","2006-01-04",
  "2006-12-03","2007-04-26","2007-12-31","2009-02-28","2009-08-14",
  "2010-11-15","2011-09-07","2012-06-09","2013-10-16","2014-06-19",
  "2014-11-03","2015-07-23","2016-01-23","2016-04-10","2016-06-20",
  "2017-08-22","2018-04-18","2020-11-16","2021-09-20","2023-01-31"
)%>%as.Date(.) #now remove last comma^^^
cocomkd('ver002\\07pra')
sfInit(par=T,cpus=ncpus())#//parallel
x111 <- #PRA prepare regression accrual
  sfLapply(
    geo0[,unique(nx)],
    f230309a,
    geo=geo0, #partition task on rc3
    dfn=dfnx1, #drc
    steprip=c(steprip='ver001\\03rip'), #in
    steppra=c(steppra='ver002\\07pra') #out ver002=drc
  )
sfStop()#//
#----------------------------------------------------------------------prep - PV
x121 <- x103$ses$soar%>%
  .[,.(nid,m2,pv,ppm2,rcx=rc9)]%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,6))])%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,3))])
#-------------------------------------------------------solution2 - cardinal/DRC
x131 <- #GEO.cardinal
  structure(
    list(
      rc9 = 
        c("TS-", "L--", "S--", "LS-", "M--", "B--", "BS-",#7 metro areas low to mid
          "AL-", "HP-10-", "HP-16-", "HP-23-",#AL + near-price neighbours in HP
          "HP-4--", "HP-6--","HP-7--", "HP-8--", "HP-9--",
          "N--",#N for London
          "EC-3R-", "EC-4A-", "N--1C-", "SW-10-", "SW-1A-", "SW-1E-", "SW-1H-",  
          "SW-1P-", "SW-1W-", "SW-1X-", "SW-1Y-", "SW-3--", "SW-5--", "SW-7--", 
          "W--11-", "W--1B-", "W--1D-", "W--1F-", "W--1G-", "W--1H-", "W--1J-", 
          "W--1K-", "W--1S-", "W--1T-", "W--1U-", "W--1W-", "W--8--", "WC-2A-", 
          "WC-2B-", "WC-2E-", "WC-2H-", "WC-2N-", "WC-2R-"),#top price districts
      nx = c(1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 10, 10, 10, 10, 
             10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
             10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
    ), 
    class = "data.frame", 
    row.names = c(NA, -50L))%>% data.table(.)
x132 <- #DRC solve 
  f230311b(#//parallel
    geo=x131[,.(rc9,nx,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x133 <- #pca
  x132$ses$estdt[,.(date=date1,lab=rc3,xdot)]%>%
  dcast(.,date~lab,value.var='xdot')%>%
  pcaest(.)
#------------------------------------------------------------solution3 - DTC/DRC
breaks <- c(10, 541, 824, 1272, 1631, 2024, 2113, 2189, 2253) #rc6(ppm2) bins
geo1 <- x121%>% #encoded geo which is ordered and has right end-breaks
  .[nchar(rcx)==6]%>%
  .[order(ppm2)]%>%
  .[,.(
    rcx,
    qai=1:.N,
    nx=apply(sapply(breaks,`<`,1:.N),1,sum)+1
    )]
x141a <- #DTC solve x(dtc)
  f230311b(#//parallel
    geo=geo1[,.(rc9=rcx,nx,qai,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x141 <- x141a[c('ses','geo')]
x142 <- #RIB regress in beta(x,z)
  f230506b( 
    nxx=geo1[,sort(unique(nx))],
    estdtx=x141$ses$estdt, #regressand x
    pcax=x133, #regressor z
    kbar=3
  )
f230506c( #display panel
  sol1=x103,
  drc=x141,
  theta=x142$beta,
  pcax=x133)
#------------------------------------------------------------solution4 - DES/DRC
source('eennx.R') #coordinates
eennx <- as.data.table(eennx)[,.(rc6=rc,eex,nnx)]
x150 <- f230516a(
    soar=x121,
    geo=geo1[,.(rc6=rcx,qai,nx)],
    xso=eennx
)
x151 <- #returns list(geo,beta) for des-split
  f230516b(
    geo=geo1[,.(rc6=rcx,qai,nx)],
    des=x150,
    pca=x133)
#----------------------------------------------------------------------------VAR
x2 <- x133%>%
  pcaz(.)%>%
  `[`(.,j=2:3)%>%
  cumsum(.)%>%
  coredata(.)%>%
  rbind(c(0,0),.)
x3 <- sweep(
  x2,
  MAR=2,
  STAT=(apply(x2,2,min)+apply(apply(x2,2,range),2,diff)/2),
  FUN=`-`
)
x4 <- vars::VAR(y=x3[-1,],                  p=2,type='const')
x5 <- vars::VAR(y=x3[-1,],                  p=3,type='const')
x6 <- vars::VAR(y=x3[-1,][-(nrow(x3)-0:6),],p=2,type='const')
x7 <- vars::VAR(y=x3[-1,][-(nrow(x3)-0:6),],p=3,type='const')
x161 <- list(
  z=x3,
  var0  =x4,
  var0p3=x5,
  var7  =x6,
  var7p3=x7)
#-----------------------------------------------------------------------NUTS/ANN
rmifgl('geo')
source('geo2.r') #NUTS table derives from https://github.com/ygalanak/UKpc2NUTS
x1 <- data.table(geo)[,.(rc6,NUTS=ltr)]
x1[,sum(duplicated(rc6))/.N] #.0745 overlaps in rc6-nuts relation 
geo2 <- x1[x1[,.(rc6=unique(rc6))],on=c(rc6='rc6'),mult='first']%>%#duplicates are resolved randomly - surely can write this line better?!
  .[order(NUTS)]%>%
  .[,.(rc9=rc6,nx=as.integer(as.factor(NUTS)),lab=NUTS)]%>%
  .[geo1[,.(rc9=rcx)],on=c(rc9='rc9')]
x171 <- f230311b( #solve
    geo=geo2, #NUTS
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
x172 <- f230311b( #solve
    geo=geo1[,.(rc9=rcx,nx,lab=labxnnn(nx))], #DTC
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
x4 <- #sse(NUTS)
  lapply(1:10,function(i){x171[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]})%>%
  rbindlist(.)%>%
  .[,geo:='NUTS']
x5 <- #sse(DTC) = benchmark 1
  lapply(1:10,function(i,x){x172[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]})%>%
  rbindlist(.)%>%
  .[,geo:='DTC']
x6 <- #sse(RC3) = benchmark 2
  lapply(1:104,function(i,x){x103a[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]})%>%
  rbindlist(.)%>%
  .[,geo:='RC3']
x173 <- rbind(x4,x5,x6)[,.(N=sum(N),sse=sum(sse)),geo][,sserel:=round(sse/sse[1],4)]
#-----------------------------------------------------save for graphics and tabs
nn <- c(
  sol1='x103',
  pvrc='x121',
  sol2='x132',
  sol2='x133',
  sol3='x141',
  rib3='x142',
  sol4='x151',
  var='x161'
)
save(list=nn,file='xnnn.rdata') #for tab, graphic, diagnostice on this script
