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
stopifnot(length(dir('ver001\\07pra'))>8e3)
#############################################################
x103a <- #solve and reprice
  f230311b( #//parallel
    geo=geo0,
    steppra='ver001\\07pra',
    stepsip='ver001\\02sip'
  )#//
x103 <- x103a[c('ses','geo')] #x103a is large with BSO - just for MSE
#putt(x103a)
#putt(x103)
#---------------------------------------------------------------------prep - DRC
dfnx1 <- #DRCd
  c("1994-12-31", "1996-07-14", "1997-01-17", "1997-06-23", "1998-01-27", 
    "1998-10-22", "1999-08-06", "1999-12-03", "2000-01-31", "2001-10-12", 
    "2002-06-25", "2002-10-06", "2003-02-10", "2003-05-22", "2003-08-09", 
    "2003-10-21", "2004-02-08", "2004-05-05", "2004-07-13", "2005-02-01", 
    "2006-01-04", "2006-12-03", "2007-04-26", "2007-12-31", "2009-02-28", 
    "2009-08-14", "2010-11-15", "2011-09-07", "2012-06-09", "2013-10-16", 
    "2014-06-19", "2014-11-03", "2015-07-23", "2016-01-23", "2016-04-10", 
    "2016-06-20", "2017-08-22", "2018-04-18", "2020-11-16", "2021-09-20", 
    "2023-01-31")%>%as.Date(.)
cocomkd('ver002\\07pra')
sfInit(par=T,cpus=ncpus())#//parallel
x111 <- #PRA prepare regression accrual
  sfLapply(
    geo0[,unique(nx)],###############################################
    f230309a,
    geo=geo0, #partition task on rc3
    dfn=dfnx1, #drc
    steprip=c(steprip='ver001\\03rip'), #in
    steppra=c(steppra='ver002\\07pra') #out ver002=drc
  )
sfStop()#//
#putt(x111)
#----------------------------------------------------------------------prep - PV
x121 <- x103$ses$soar%>%
  .[,.(nid,m2,pv,ppm2,rcx=rc9)]%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,6))])%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,3))])
#putt(x121)
#-------------------------------------------------------solution2 - cardinal/DRC
x131 <- #GEO.cardinal
  structure(
    list(
      rc9 = 
        c("TS-", "L--", "S--", "LS-", "M--", "B--", "BS-", #7 metro areas low to mid
          "AL-", "HP-10-", "HP-16-", "HP-23-",#AL + near-price neighbours in HP
          "HP-4--", "HP-6--","HP-7--", "HP-8--", "HP-9--",
          "N--", #N for London
          "EC-3R-", "EC-4A-", "N--1C-", "SW-10-", "SW-1A-", "SW-1E-", "SW-1H-",  
          "SW-1P-", "SW-1W-", "SW-1X-", "SW-1Y-", "SW-3--", "SW-5--", "SW-7--", 
          "W--11-", "W--1B-", "W--1D-", "W--1F-", "W--1G-", "W--1H-", "W--1J-", 
          "W--1K-", "W--1S-", "W--1T-", "W--1U-", "W--1W-", "W--8--", "WC-2A-", 
          "WC-2B-", "WC-2E-", "WC-2H-", "WC-2N-", "WC-2R-"), #top price districts
      nx = c(1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 10, 10, 10, 10, 
             10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
             10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
    ), 
    class = "data.frame", 
    row.names = c(NA, -50L))%>% data.table(.)
x132 <- #DRC solve 
  f230311b(#//parallel
    #geo=x131[,.(rc9,nx,lab=labxnnn(nx))],############################
    geo=x131[,.(rc9,nx,lab=labxnnn(nx))],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x133 <- #pca
  x132$ses$estdt[,.(date=date1,lab=rc3,xdot)]%>%
  dcast(.,date~lab,value.var='xdot')%>%
  pcaest(.)
# putt(x132) #large object includes BSO for MSE
# putt(x133)
# save(x132,file='x132.rdata')
# save(x133,file='x133.rdata')
#------------------------------------------------------------solution3 - DTC/DRC
source('geo1.r') #assign geo<-drc
geo1 <- data.table(geo)
x141a <- #DTC solve x(dtc)
  f230311b(#//parallel
    geo=geo1[,.(rc9=rc6,nx,qai,lab)],
    steppra='ver002\\07pra',#ver002=drc
    stepsip='ver001\\02sip'
  )#//
x141 <- x141a[c('ses','geo')]
# save(x141,file='x141.rdata')
#putt(x141a) #large object includes BSO for MSE
#putt(x141) 
x142 <- #RIB regress in beta(x,z)
  f230506b( 
    nxx=geo1[,sort(unique(nx))],
    estdtx=x141$ses$estdt, #regressand x
    pcax=x133, #regressor z
    kbar=3
  )
#putt(x142)
#---display panel
f230506c(    
  sol1=x103,
  drc=x141,
  theta=x142$beta,
  pcax=x133
)
#----------------------------------------------------------------solution4 - DES
# f230414aFun( #DES
#   soar=x121[nchar(rcx)==6][,rc6:=rcx],
#   geo=geo1,
#   xso=f201203fd[nchar(rc)==6,.(rc6=rc,eex,nnx)], #coordinates
#   nn=NULL
# )
# des1 <- data.frame(f230414ad)
# dump('des1','des1')
#getgd(c('x133','x142'))
# getgd('x133')
# save.image(file='DES.rdata')#<<<<<<<<<<<<<<<<<<<<<
# load(file='DES.rdata')

rmifgl('des1')
source('des1')
des=data.table(des1)[,des:=des/1e6]#price residual/relative to neighbours (<50km)

sfInit(par=T,cpus=ncpus())
x1 <- 
  sfLapply(
    as.list(2:9), #nx to split
    f230417c,
    geo=geo1,
    des=des,
    pca=x133
  )
sfStop()
xbeta <- as.list(NULL)
xgeo <- as.list(NULL)
for(i in seq_along(x1)) {
  xbeta[[i]] <- x1[[i]][['beta']][order(des)][,desx:=-2:2]
  xgeo[[i]] <- x1[[i]][['geo']]
}
x2 <- rbindlist(xgeo) #split geo for solve and MSE (not done, need a key like x2[,.N,.(nx,dir,type)][,ii:=1:.N] and then solve, allowing for the existence of overlapping geo)
x3 <- rbind(
  rbindlist(xbeta)[,.(b2,b3,nx,des,desx,ppm2)],
  x142$beta[,.(b2,b3,nx,des=0,desx=0,ppm2=NA)]
)[,col:=reorder(as.factor(desx),-desx)]%>%
  .[,nxfac:=as.factor(nx)]%>%
  .[order(nx,desx)]%>%
  .[,unit:=as.factor(nx)]
x151 <- list(
  geo=x2,
  beta=x3  
)
#putt(x151)
#----------------------------------------------------------------------------VAR
#getgd('x133')
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
  var7p3=x7
)
#putt(x161)
#---------------------------------------------------------------------------NUTS
#NUTS table derives from https://github.com/ygalanak/UKpc2NUTS
#getgd('x103a') #solved previously as 'solution 1': annual areas=RC3
rm('geo')
source('geo2.r')
x1 <- data.table(geo)[,.(rc6,NUTS=ltr)]
x1[,sum(duplicated(rc6))/.N] #.0745 overlaps in rc6-nuts relation 
geo2 <- x1[x1[,.(rc6=unique(rc6))],on=c(rc6='rc6'),mult='first']%>%#duplicates resolved randomly (with no further digging)
  .[order(NUTS)]%>%
  .[,.(rc9=rc6,nx=as.integer(as.factor(NUTS)),lab=NUTS)]%>%
  .[geo1[,.(rc9=rc6)],on=c(rc9='rc9')]
x171 <- f230311b( #solve
    geo=geo2, #NUTS
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
#putt(x171) #NUTS
x172 <- f230311b( #solve
    geo=geo1[,.(rc9=rc6,nx,lab)], #DTC
    steppra='ver001\\07pra',#annual
    stepsip='ver001\\02sip'
  )
#putt(x172) #DTC
x4 <- #sse(NUTS)
  lapply(1:10,function(i){x171[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]})%>%
  rbindlist(.)%>%
  .[,geo:='NUTS']
x5 <- #sse(DTC)
  lapply(1:10,function(i,x){x172[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]})%>%
  rbindlist(.)%>%
  .[,geo:='DTC']
x6 <- #sse(RC3)
  lapply(1:104,function(i,x){x103a[['bso']][[i]][['fit']][,data.table(sse=sum(res^2),N=.N)]})%>%
  rbindlist(.)%>%
  .[,geo:='RC3']
x173 <- rbind(x4,x5,x6)[,.(N=sum(N),sse=sum(sse)),geo][,sserel:=round(sse/sse[1],4)]
#putt(x173) #sse(nuts,dtc,rc3)
x173[]


