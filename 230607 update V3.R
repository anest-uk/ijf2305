#------------------------------------------------------------------------geo rc3
geo0 = #all E&W 104 areas (exclude only tweedside)
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
#-------------------------------------------------------------------geo cardinal
x131 <- #GEO cardinal = 10 zones
  structure(
    list(
      rc9 = 
        c("TS-", "L--", "S--", "M--", "LS-", "B--", "BS-",#1:7 : metro areas 
          "AL-", "HP-10-", "HP-16-", "HP-23-",    #8 : AL&HP-peers
          "HP-4--", "HP-6--","HP-7--", "HP-8--", "HP-9--",
          "N--",                                  #9 : N for non-prime London
          "EC-3R-", "EC-4A-", "N--1C-", "SW-10-", #10 : top £/m2 PCL districts
          "SW-1A-", "SW-1E-", "SW-1H-", "SW-1P-", 
          "SW-1W-", "SW-1X-", "SW-1Y-", "SW-3--", 
          "SW-5--", "SW-7--", "W--11-", "W--1B-", 
          "W--1D-", "W--1F-", "W--1G-", "W--1H-", 
          "W--1J-", "W--1K-", "W--1S-", "W--1T-", 
          "W--1U-", "W--1W-", "W--8--", "WC-2A-", 
          "WC-2B-", "WC-2E-", "WC-2H-", "WC-2N-", 
          "WC-2R-"),
      nx = c(1, 2, 3, 4, 5, 6, 7,                #1:7 : metro areas 
             8, 8, 8, 8, 8, 8, 8, 8, 8,          #8 : AL&HP-peers
             9,                                  #9 : N for non-prime London
             10, 10, 10, 10, 10, 10, 10, 10, 10, #10 : top £/m2 PCL districts
             10, 10, 10, 10, 10, 10, 10, 10, 10, 
             10, 10, 10, 10, 10, 10, 10, 10, 10, 
             10, 10, 10, 10, 10, 10)
    ), 
    class = "data.frame", 
    row.names = c(NA, -50L))%>% data.table(.)
#---------------------------------------------------------------------pra annual
x101= #annual
  seq.Date(
    from=as.Date('1994-12-31'),
    to=as.Date('2022-12-31'),
    by='y'
  )+
  c(
    rep(0,14),
    59, #extend 2008 2 months to 2009-02-28 (GFC end)
    rep(0,13),
    31+28+31) #extend 2022 to 2023-03-31
cocomkd('ver001\\07pra')
sfInit(par=T,cpus=ncpus())#//parallel
x102 <- #---PRA Prepare Regression Accrual write to rc9.csv 
  sfLapply(
    geo0[,unique(nx)],
    f230309a,
    geo=geo0, #geo-bin nx(rc3)
    dfn=x101, #date-bin
    steprip=c(steprip='ver001\\03rip'), #return csv
    steppra=c(steppra='ver001\\07pra')  #accrual csv
  )
sfStop()     #//
stopifnot(length(dir('ver001\\07pra'))>8e3) 
#------------------------------------------------------------------------pra DRC
# dfnx1 <- c( #Delta R Constant dates partition time by equal z23 variance
#   "1994-12-31",
#   "1996-07-14","1997-01-17","1997-06-23","1998-01-27","1998-10-22",
#   "1999-08-06","1999-12-03","2000-01-31","2001-10-12","2002-06-25",
#   "2002-10-06","2003-02-10","2003-05-22","2003-08-09","2003-10-21",
#   "2004-02-08","2004-05-05","2004-07-13","2005-02-01","2006-01-04",
#   "2006-12-03","2007-04-26","2007-12-31","2009-02-28","2009-08-14",
#   "2010-11-15","2011-09-07","2012-06-09","2013-10-16","2014-06-19",
#   "2014-11-03","2015-07-23","2016-01-23","2016-04-10","2016-06-20",
#   "2017-08-22","2018-04-18","2020-11-16","2021-09-20","2023-03-31"
# )%>%as.Date(.) 
dfnx1 <- c(#v135 230607
"1994-12-31",
"1996-07-17","1997-01-20","1997-06-25","1998-01-31","1998-10-30",
"1999-08-07","1999-12-04","2000-02-07","2001-10-13","2002-06-25",
"2002-10-05","2003-02-12","2003-05-24","2003-08-10","2003-10-22",
"2004-02-08","2004-05-05","2004-07-13","2005-01-31","2006-01-02",
"2006-12-03","2007-04-26","2007-12-31","2009-02-28","2009-08-19",
"2010-11-20","2011-09-12","2012-06-26","2013-11-01","2014-06-24",
"2014-11-13","2015-08-07","2016-02-02","2016-04-10","2016-07-17",
"2017-09-12","2018-06-13","2020-12-30","2021-10-19","2023-03-31"
)%>%as.Date(.) 
cocomkd('ver002\\07pra') #ver002
sfInit(par=T,cpus=ncpus())#//parallel
x111 <- #PRA - DRC
  sfLapply(
    geo0[,unique(nx)],
    f230309a,
    geo=geo0,  #partition task on rc3
    dfn=dfnx1, #drc
    steprip=c(steprip='ver001\\03rip'), #in
    steppra=c(steppra='ver002\\07pra')  #out ver002=drc
  )
sfStop() #//
#----------------------------------------------------------solution 1 rc3/annual 
print('start solution 1')
x103a <- #rc3/annual solve
  f230311b( #//parallel
    geo=geo0, #solve on rc3 bins
    steppra='ver001\\07pra', #annual see x102
    stepsip='ver001\\02sip'
  )#//
x103 <- x103a[c('ses','geo','tss')] #x103a is large with BSO for MSE so reduce
#---------------------------------------------------------------aggregation - PV
x121 <-  #pva
  f230602a(
  fis=x103 #solution 1
  )
#putt(x121) #used in private update (dtc)
breaks <- #DTC breakpoints on ranked p, unched 230607
  c(10,541,824,1272,1631,2024,2113,2189,2253) 
#no change 230607
geo1 <- x121%>% 
  .[nchar(rcx)==6]%>%
  .[order(ppm2)]%>%
  .[,.(
    rcx,
    qai=1:.N,
    nx=apply(sapply(breaks,`<`,1:.N),1,sum)+1
    )]
#--------------------------------------------------------solution 2 cardinal/DRC
print('start solution 2')
x132 <- #cardinal/DRC solve 
  f230311b(#//parallel
    geo=x131[,.(rc9,nx,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
#----------------------------------------------------------------------------pca
x133 <- #pca
  x132$ses$estdt[,.(date=date1,lab=rc3,xdot)]%>%
  dcast(.,date~lab,value.var='xdot')%>%
  pcaest(.)
#attribute
#x104 <- f230603b(fis=x103a,rib=list(pca=x133,beta=pcab(x133))) #not possible due to dates
x134a <- #---RIB--- is needed even for native
  f230506b( 
    #nxx=geo0[,sort(unique(nx))],
    estdtx=x132$ses$estdt, #regressand x
    pcax=x133, #regressor z
    kbar=3
  )
x134 <- f230603b(#attribute
  fis=x132,
  rib=x134a) 
#-------------------------------------------------------------solution 3 DTC/DRC
print('start solution 3')
#allinone
#keep this one pair of FIS and RIB for testing
# x141a <- #---FIS---
#   f230311b(#//parallel
#     geo=geo1[,.(rc9=rcx,nx,qai,lab=labxnnn(nx))],
#     steppra='ver002\\07pra',#ver002=drc
#     stepsip='ver001\\02sip'
#   )#//
# x141 <- x141a[c('ses','geo','tss')]
# x142 <- #---RIB---
#   f230506b(
#     nxx=geo1[,sort(unique(nx))],
#     estdtx=x141$ses$estdt, #regressand x
#     pcax=x133, #regressor z
#     kbar=3
#   )
# f230506c( #QC display
#   sol1=x103,
#   drc=x141,
#   theta=x142$beta,
#   pcax=x133)
# #attribute
# x143 <- f230603b(fis=x141a,rib=x142)
#\allinone
x140 <- f230605a(
    geo=geo1[,.(rc9=rcx,nx,qai,lab=labxnnn(nx))],
    steppra='ver002\\07pra',
    stepsip='ver001\\02sip',
    pcax=x133
  )
f230506c( #QC display
  sol=x140 #replace this with ppm2/soar
  )


#-------------------------------------------------------------solution 4 DES/DRC
print('start solution 4')
ndesx <- 3:6 #nx for DES split
source('eennx.R') #coordinates
eenn <- as.data.table(eennx)[,.(rc6=rc,eex,nnx)]
x150 <-  #DES(rc6)
  f230516a(
    soar=x121,
    geo=geo1[,.(rc6=rcx,qai,nx)],
    xso=eenn)
x0 <- #DES heuristic tilt
  f230516b(
    geo=geo1[nx%in%ndesx,.(rc6=rcx,qai,nx)], #select nx
    des=x150,
    pca=x133)
stopifnot(x0[type=='full'][,!any(duplicated(rc6))]) #disjoint extrema
x1 <- #add DES
  unique(x0[,.(dir,type)])%>%
  .[order(type,dir),qq:=c(3,5,1,4,2)]%>% #DES rank
  .[order(qq)]%>%
  .[x0,on=c(dir='dir',type='type')]%>%
  .[,.(ndtc=nx,qq,rc6)]
x154 <- #ndes(ndtc=3:6,qq=1:5) single key=ndes 1:20
  x1[,.(ndtc,qq)]%>%
  unique(.)%>%
  .[,.(ndtc,qq,ndes=1:.N)]
geo4 <- #geo(ndes)
  x1[x154,on=c(ndtc='ndtc',qq='qq')]%>%
  .[,.(rc6,ndes)]





x156 <- f230605a(
    geo=geo4[,.(rc9=rc6,nx=ndes,lab=labxnnn(ndes))],
    steppra='ver002\\07pra',
    stepsip='ver001\\02sip',
    pcax=x133
  )
x157 <- copy(x156)
x157$rib$beta <- x156$rib$beta[x154,on=c(nx='ndes')]
f230506c( #QC display - looks odd but remember its des on rc=3:6!!
  sol=x157 #replace this with ppm2/soar
  )


# #allinone
# x151a <- #---FIS---
#   f230311b(#//parallel
#     geo=geo4[,.(rc9=rc6,nx=ndes,lab=labxnnn(ndes))],
#     steppra='ver002\\07pra',#ver002=drc
#     stepsip='ver001\\02sip'
#   )#//
# x151 <- x151a[c('ses','geo','tss')]
# x153 <- #---RIB--- 
#   x153copy <- #keep a copy x0 so the next step idempotent
#   f230506b( 
#     nxx=geo4[,sort(unique(ndes))],
#     estdtx=x151$ses$estdt, #regressand x
#     pcax=x133, #regressor z
#     kbar=3
#   )
# x153[['beta']] <- x153copy[['beta']][x154,on=c(nx='ndes')] #join qq,ndes
# x155 <-  f230603b(fis=x151a,rib=x153)
#\allinone [concern about join of key above]
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
#------------------------------------------------------------solution 5 NUTS/ANN
print('start solution 5')
rmifgl('geo')
source('geo2.r') #NUTS table derives from https://github.com/ygalanak/UKpc2NUTS
source('nutsnames.r') #assigns nname
x0 <- data.table(geo)[,.(rc6,NUTS=ltr)]
x1 <- nname[,.(code,nx)][x0,on=c(code='NUTS')]
x1[,sum(duplicated(rc6))/.N] #.0745 duplicate rc6-nuts relation one:many
geo2 <- 
  x1[,.(rc6,nx,lab=labxnnn(nx))]%>%
  .[geo1[,.(rc9=rcx)],on=c(rc6='rc9'),mult='first']#greedy duplicate allocation
x171 <- #---FIS---
  f230311b( 
    geo=geo2[,.(rc9=rc6,nx,lab)], #NUTS
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
#x171a <- f230603b(fis=x171,rib=) #no rib
#-------------------------------------------------------------solution 6 DTC/ANN
print('start solution 6')
x172 <- #---FIS---
  f230311b( 
    geo=geo1[,.(rc9=rcx,nx,lab=labxnnn(nx))], #DTC
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
#x171b <- f230603b(fis=x172,pca=x133) #no rib
#------------------------------------------------------------sse needs an overhaul with attribution of sse and mean to components -> smaller section
x4 <- #sse(NUTS)
  lapply(1:10,
         function(i){
           x171[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)] #nuts-ann
           })%>%
  rbindlist(.)%>%
  .[,geo:='NUTS']
x5 <- #sse(DTC) = benchmark 1
  lapply(1:10,
         function(i,x){
           x172[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)] #dtc-ann
           })%>%
  rbindlist(.)%>%
  .[,geo:='DTC']
x6 <- #sse(RC3) = benchmark 2
  lapply(1:104,
         function(i,x){
           x103a[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)] #rc3-ann
           })%>%
  rbindlist(.)%>%
  .[,geo:='RC3']
x173 <- 
  rbind(x4,x5,x6)%>%
  .[,.(N=sum(N),sse=sum(sse)),geo]%>%
  .[,sserel:=round(sse/sse[1],4)]
#------------------------------------------------------------solution 7 NUTS.DRC
print('start solution 7')
x180 <- f230605a(
    geo=geo2[,.(rc9=rc6,nx,lab)],
    steppra='ver002\\07pra',
    stepsip='ver001\\02sip',
    pcax=x133
  )
f230506c( #QC display - looks odd but remember its des on rc=3:6!!
  sol=x180 #replace this with ppm2/soar
  )




#allinone
# x174a <-  #---FIS---
#   f230311b( 
#     geo=geo2[,.(rc9=rc6,nx,lab)], #NUTS
#     steppra='ver002\\07pra',#DRC
#     stepsip='ver001\\02sip'
#   )
# x174 <- x174a[c('geo','ses')]
# x175 <- #---RIB---
#   f230506b( 
#     nxx=geo2[,sort(unique(nx))], #NUTS
#     estdtx=x174$ses$estdt[,rc3:=labxnnn(nx)], #NUTS
#     pcax=x133,
#     kbar=3
#   )
# x174b <- f230603b(fis=x174a,rib=x175)
# #\allinone
if(F) { #maybe replace, inputs missing now
x176 <- #lacks unwanted lppm2(theta)
  x121[geo2,on=c(rcx='rc9')]%>%
  .[,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),.(lab,nx)]%>%
  .[x175$beta,on=c(nx='nx')]%>% #all vbles of interest now included
  .[nname,on=c(lab='code')]%>%
  .[,.(lab,name,theta,rbarsq,a,at,ppm2)]
}
#------------------------------------------------------------solution 8 QNUT/DRC
print('start solution 8')
geo3 <- #geo-quintiles within NUTS regions
  x121[geo2,on=c(rcx='rc6')]%>%
  .[,.SD[,.( nid,m2,pv,ppm2,rcx,nx,qq=ceiling(5*cumsum(nid)/sum(nid))[])],lab]%>%
  .[,.(nid,rcx,n0=nx,qq,nx=(nx-1)*5+qq)]

x190 <- f230605a(
    geo=geo3[,.(rc9=rcx,nx,qai=1:.N,lab=labxnnn(nx))],
    steppra='ver002\\07pra',
    stepsip='ver001\\02sip',
    pcax=x133
  )
f230506c( #QC display - looks odd but remember its des on rc=3:6!!
  sol=x190 #replace this with ppm2/soar
  )



#allinone
# x177a <-  #---FIS---
#   f230311b(#//parallel
#     geo=geo3[,.(rc9=rcx,nx,qai=1:.N,lab=labxnnn(nx))],
#     steppra='ver002\\07pra',#ver002=drc
#     stepsip='ver001\\02sip'
#   )
# x177 <- x177a[c('ses','geo','tss')]#//
# x179 <- #---RIB---
#   f230506b( 
#     nxx=geo3[,sort(unique(nx))],
#     estdtx=x177$ses$estdt, #regressand x
#     pcax=x133, #regressor z
#     kbar=3)
# x177c <- f230603b(fis=x177a,rib=x179)
# #\allinone

if(F) { #maybe replace, inputs missing now
x178 <- #QNUT summary
  geo3[,.(nx,n0,qq)]%>% #n0 is nuts-letter-factor-code; 
  unique(.)%>%
  .[x179$beta,on=c(nx='nx')]%>%
  .[,col:=as.factor(n0)]%>%
  .[unique(geo2[,.(nx,lab)]),on=c(n0='nx')]%>%
  .[nname,on=c(i.lab='code')]%>%
  .[,col:=as.factor(paste0(n0,name))]
}
#-----------------------------------------------------save for graphics and tabs
if(F) {
  rmifgl(c('x172','x171','x103a','x132'))
  save.image('v135-230606')
}

nn <- c( #objects are labelled calc/tab/fig according to their intended use
  sol1='x103',#g t
  pvrc='x121',#  t
  sol2='x132',#  t
  pca ='x133',#g t 
  sol3='x141',#g t
  rib3='x142',#g t
  #des='x150',
  sol4='x151',#  t
  rib5='x153',#g
  var ='x161',#g t
  nuts1='x175',
  nuts5='x178',#g
  rib6='x179',
  #'x104', #sundry summaries
  'x134',
  'x143',
  'x155',
  #'x171a',
  #'x171b',
  'x174b',
  'x177c'
)
for(i in seq_along(nn)) {#individual objects save: quite fast
  save(list=nn[i],file=paste0(nn[i],'.Rdata'))
}
if(F) { #this is quite fast but should not be needed 
  for(i in seq_along(nn)) {
    load(file=paste0(nn[i],'.Rdata'))
  }
}
if(F) {#use single files instead
  save(list=nn,file='xnnn.rdata') #for tab, graphic; option to load this file
  load('xnnn.rdata')
}
if(F) {#v v slow! do not use
  save.image(file=paste0(format(Sys.time(),'%y%m%d%H%M'),'.Rdata'))
}
#save(x141a,file='x141a.Rdata')
