#----------------------------------------------------------solution 1 rc3/annual 
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
x102 <- #---PRA - annual
  sfLapply(
    geo0[,unique(nx)],
    f230309a,
    geo=geo0, #partition task on rc3
    dfn=x101, #annual
    steprip=c(steprip='ver001\\03rip'), #in
    steppra=c(steppra='ver001\\07pra')  #out ver001=annual
  )
sfStop()#//
stopifnot(length(dir('ver001\\07pra'))>8e3) #quick/approx size check
x103a <- #rc3/annual solve
  f230311b( #//parallel
    geo=geo0,
    steppra='ver001\\07pra',
    stepsip='ver001\\02sip'
  )#//
x103 <- x103a[c('ses','geo')] #x103a is large with BSO for MSE
#---------------------------------------------------------------------prep - DRC
dfnx1 <- c( 
  "1994-12-31",
  "1996-07-14","1997-01-17","1997-06-23","1998-01-27","1998-10-22",
  "1999-08-06","1999-12-03","2000-01-31","2001-10-12","2002-06-25",
  "2002-10-06","2003-02-10","2003-05-22","2003-08-09","2003-10-21",
  "2004-02-08","2004-05-05","2004-07-13","2005-02-01","2006-01-04",
  "2006-12-03","2007-04-26","2007-12-31","2009-02-28","2009-08-14",
  "2010-11-15","2011-09-07","2012-06-09","2013-10-16","2014-06-19",
  "2014-11-03","2015-07-23","2016-01-23","2016-04-10","2016-06-20",
  "2017-08-22","2018-04-18","2020-11-16","2021-09-20","2023-01-31"
)%>%as.Date(.) 
cocomkd('ver002\\07pra')
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
sfStop()#//
#----------------------------------------------------------------------prep - PV
x121 <- x103$ses$soar%>%
  .[,.(nid,m2,pv,ppm2,rcx=rc9)]%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,6))])%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,3))])
#---------------------------------------------------------solution 2 cardinal/DRC
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
x132 <- #cardinal/DRC solve 
  f230311b(#//parallel
    geo=x131[,.(rc9,nx,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x133 <- #pca
  x132$ses$estdt[,.(date=date1,lab=rc3,xdot)]%>%
  dcast(.,date~lab,value.var='xdot')%>%
  pcaest(.)
#--------------------------------------------------------------solution 3 DTC/DRC
breaks <- c(10, 541, 824, 1272, 1631, 2024, 2113, 2189, 2253) #rc6(ppm2) binbreaks
geo1 <- x121%>% 
  .[nchar(rcx)==6]%>%
  .[order(ppm2)]%>%
  .[,.(
    rcx,
    qai=1:.N,
    nx=apply(sapply(breaks,`<`,1:.N),1,sum)+1
    )]
x141a <- #DTC/DRC solve 
  f230311b(#//parallel
    geo=geo1[,.(rc9=rcx,nx,qai,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x141 <- x141a[c('ses','geo')]
x142 <- #RIB regress in beta(x,z),theta
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
#--------------------------------------------------------------solution 4 DES/DRC
source('eennx.R') #coordinates
ndesx <- 3:6 #four to split
eennx <- as.data.table(eennx)[,.(rc6=rc,eex,nnx)]
x150 <-  #des prep for all rc6
  f230516a(
    soar=x121,
    geo=geo1[,.(rc6=rcx,qai,nx)],
    xso=eennx)
x0 <- #design keyed on nx=2:9,dir=all/lo/hi,type=all/half/full
  f230516b(
    geo=geo1[nx%in%ndesx,.(rc6=rcx,qai,nx)],
    des=x150,
    pca=x133)
x1 <- #ndtc was nx=2:9;qq=1:5 was dir,type;rc6
  unique(x0[,.(dir,type)])%>%
  .[,qq:=c(3,4,2,5,1)]%>%
  .[order(qq)]%>%
  .[x0,on=c(dir='dir',type='type')]%>%
  .[,.(ndtc=nx,qq,rc6)]
x154 <- #ndtc=2:9,qq=1:5,ndes=1:40 single key ndes
  x1[,.(ndtc,qq)]%>%
  unique(.)%>%
  .[,.(ndtc,qq,ndes=1:.N)]
geo4 <- #geo for des split
  x1[x154,on=c(ndtc='ndtc',qq='qq')]%>%
  .[,.(rc6,ndes)]
#save('geo4',file='geo4.rdata')
x151a <- #DTC/DRC solve 
  f230311b(#//parallel
    geo=geo4[,.(rc9=rc6,nx=ndes,lab=labxnnn(ndes))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x151 <- x151a[c('ses','geo')]
x152 <- #RIB(ndes) no longer directly used
  f230506b( 
    nxx=geo4[,sort(unique(ndes))],
    estdtx=x151$ses$estdt, #regressand x
    pcax=x133, #regressor z
    kbar=3
  )
x153 <- #RIB keyed on ndtc,qq,ndes
  x154[x152$beta,on=c(ndes='nx')]
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
rmifgl('geo')
source('geo2.r') #NUTS table derives from https://github.com/ygalanak/UKpc2NUTS
x1 <- data.table(geo)[,.(rc6,NUTS=ltr)]
x1[,sum(duplicated(rc6))/.N] #.0745 overlaps in rc6-nuts relation 
geo2 <- 
  x1[order(NUTS)]%>%
  .[,.SD[1,],rc6]%>% #greedy/alphabetic allocation of duplicate rc6
  .[,.(rc9=rc6,nx=as.integer(as.factor(NUTS)),lab=NUTS)]%>%
  .[geo1[,.(rc9=rcx)],on=c(rc9='rc9')]
x1[,.SD[1,],rc6]
x1[x1[,.(rc6=unique(rc6))],on=c(rc6='rc6'),mult='first']
x171 <- #NUTS/annual solve
  f230311b( 
    geo=geo2, #NUTS
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
#------------------------------------------------------------solution 6 DTC/ANN
x172 <- #DTC/annual solve
  f230311b( 
    geo=geo1[,.(rc9=rcx,nx,lab=labxnnn(nx))], #DTC
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
x4 <- #sse(NUTS)
  lapply(1:10,
         function(i){
           x171[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]
           })%>%
  rbindlist(.)%>%
  .[,geo:='NUTS']
x5 <- #sse(DTC) = benchmark 1
  lapply(1:10,
         function(i,x){
           x172[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]
           })%>%
  rbindlist(.)%>%
  .[,geo:='DTC']
x6 <- #sse(RC3) = benchmark 2
  lapply(1:104,
         function(i,x){
           x103a[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]
           })%>%
  rbindlist(.)%>%
  .[,geo:='RC3']
x173 <- 
  rbind(x4,x5,x6)%>%
  .[,.(N=sum(N),sse=sum(sse)),geo]%>%
  .[,sserel:=round(sse/sse[1],4)]
#------------------------------------------------------------solution 7 NUTS.DRC
x174 <- #NUTS/DRC solve
  f230311b( 
    geo=geo2, #NUTS
    steppra='ver002\\07pra',#DRC
    stepsip='ver001\\02sip'
  )[c('geo','ses')]

x175 <- #NUTS RIB
  f230506b( 
    nxx=geo2[,sort(unique(nx))], #NUTS
    estdtx=x174$ses$estdt[,rc3:=labxnnn(nx)], #regressand x = NUTS
    pcax=x133, #regressor z = DTC
    kbar=3
  )
nname <- #NUTS names
structure(list(X1 = c("L", "K", "J", "I", "H", "G", "F", "E", 
"D", "C"), X2 = c("Wales", "South West", "South East", "London", 
"East of England", "West Midlands", "East Midlands", "Yorkshire and Humber", 
"North West", "North East")), class = "data.frame", row.names = c(NA, 
-10L))%>%data.table(.)%>%setnames(.,c('code','name'))
p.theta.lin <-  #lppm2(theta) piecewise linear trained on DTC
  geo1%>% #DTC
  .[x121,on=c(rcx='rcx'),nomatch=NULL]%>% #Soar
  .[,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),nx]%>%
  .[,lppm2:=log(ppm2)]%>%
  .[,.(nx,ppm2,lppm2)]%>%
  x142$beta[.,on=c(nx='nx')]%>% #DTC RIB
  .[order(nx)]%>%
  .[,approxfun(x=theta,y=lppm2)]
x176 <- geo2%>% #NUTS beta, ppm2, inferredppm2
  .[,.(rc6=rc9,nx,nutscode=lab)]%>%
  nname[.,on=c(code='nutscode')]%>%
  .[x121,on=c(rc6='rcx'),nomatch=NULL]%>% #soar
  .[,.(m2=sum(m2),pv=sum(pv),nid=sum(nid),ppm2=sum(pv)/sum(m2)),.(code,name,nx)]%>%
  .[x175$beta,on=c(nx='nx')]%>% #NUTS RIB
  .[,.(name,theta,rbarsq,a,at,ppm2,phat=exp(p.theta.lin(theta)))]%>%
  .[order(-ppm2)] #,cor(log(ppm2),log(phat))
geo3 <- #geo-quintiles within NUTS regions
  x121[geo2,on=c(rcx='rc9')]%>%
  .[,.SD[,.( nid,m2,pv,ppm2,rcx,nx,qq=ceiling(5*cumsum(nid)/sum(nid))[])],lab]%>%
  .[,.(nid,rcx,n0=nx,qq,nx=(nx-1)*5+qq)]
#------------------------------------------------------------solution 8 QNUT/DRC
x177 <- #QNUT/DRC solve
  f230311b(#//parallel
    geo=geo3[,.(rc9=rcx,nx,qai=1:.N,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )[c('ses','geo')]#//
x179 <- #QNUT RIB
  f230506b( 
    nxx=geo3[,sort(unique(nx))],
    estdtx=x177$ses$estdt, #regressand x
    pcax=x133, #regressor z
    kbar=3)
x178 <- #QNUT summary
  geo3[,.(nx,n0,qq)]%>% #n0 is nuts-letter-factor-code; 
  unique(.)%>%
  .[x179$beta,on=c(nx='nx')]%>%
  .[,col:=as.factor(n0)]%>%
  .[unique(geo2[,.(nx,lab)]),on=c(n0='nx')]%>%
  .[nname,on=c(i.lab='code')]%>%
  .[,col:=as.factor(paste0(n0,name))]
#-----------------------------------------------------save for graphics and tabs
nn <- c( #objects are labelled calc/tab/fig according to their intended use
  sol1='x103',#g t
  pvrc='x121',#  t
  sol2='x132',#  t
  pca ='x133',#g t 
  sol3='x141',#g t
  rib3='x142',#g t
  sol4='x151',#  t
  rib5='x153',#g
  var ='x161',#g t
  nuts5='x178',#g
  rib6='x179'
)
#individual objects quite fast
for(i in seq_along(nn)) {
  save(list=nn[i],file=paste0(nn[i],'.Rdata'))
}
for(i in seq_along(nn)) {
  load(file=paste0(nn[i],'.Rdata'))
}
#v v slow!
if(F) {save.image(file=paste0(format(Sys.time(),'%y%m%d%H%M'),'.Rdata'))}


# rib4='x152',
# nutsdrc='x174', 
# nutsrib='x176', 
# nutssol='x177', 

#7 required for g
# x103
# x133
# x141
# x142
# x153
# x161
# x178

# nn <- c( #prepped in 'public update'
#   sol1='x103',
#   pvrc='x121',
#   sol2='x132',
#   sol2='x133',
#   sol3='x141',
#   rib3='x142',
#   sol4='x151',
#   var='x161'
# )



save(list=nn,file='xnnn.rdata') #for tab, graphic; option to load this file
load('xnnn.rdata')
