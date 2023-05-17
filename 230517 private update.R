#run prod/vxx/dino slow preample.r, so load all pkg and lib
#private updaters of date, geo,...----------------------------------------------
geo <- #cardinals: never updated 
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

#---'pretty dump' goes into the public script
dfn <- c( #import inital values prior to update being last known good
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

#--- update drc dates, assuming RIS updated already
f230312aFun(#update DRC dates by iteration
  geo=geo, #cardinal
  dfn=dfn, #inital 
  dfnmax=max(dfn),
  signmethod='order',
  niter=10)

#--- execute this block to update 'pretty dump' above
#replace the above and also paste into public script as 'dfn'
dfn1 <- f230312ad$pca$date
{#code-to-write-'from-nowt'-public-code; (1) execute (2) delete final comma (3) paste to public
  cat('dfn <- c( #import inital values prior to update being last known good\n')
  cat('"1994-12-31",\n')
  write.table(t(matrix(as.character(dfn1),5,8)),sep=',',row.names=F,col.names=F,eol = ",\n")
  cat(')%>%as.Date(.) #now remove last comma^^^')
} #the result should look like the dfn assignment scripted above, so copy it into public script

#----------------------------------------------------------------------------DTC
#depends: x121=soar
x121 <- getlast('x121') #soar for rc3/6/12, should be updated but getlast()

breaks <- #import breaks to update 'last known good'
  c(10, 541, 824, 1272, 1631, 2024, 2113, 2189, 2253)

geo1 <- x121%>%
  .[nchar(rcx)==6]%>%
  .[order(ppm2)]%>%
  .[,.(
    rc6=rcx,
    qai=1:.N,
    nx=apply(sapply(breaks,`<`,1:.N),1,sum)+1
  )]

f230322aFun( #update DTC geo breaks by iteration (does not use PRA but could)
  nn=c(DRC='f230312ad'),
  geo0=geo1,
  drc=f230312ad,
  dmp=NULL,
  cdl=NULL,
  itermax=10,
  mintrade=.01)

#--- execute this block to update 'pretty dump' above
#replace the above and also paste into public script as 'breaks'
{#code-to-write-'from-nowt'-public-code; (1) execute (2) paste to public
  paste0('breaks <- c(',paste0(f230322ad$theta[-.N,cumsum(N)],collapse=','),')')
}

#-------------------------------------------------------eenn coordinates for DES
#depends: geo1
#output: eennd.R which is run in public script
#could be in library but 'data' better in separate local dump file 'eennd.R'
x130 <- getlast('f201203fd') #{eex,nnx}(rc) which is OS coordinate data
eennx <- x130[geo1[,.(rc6)],.(rc,eex,nnx),on=c(rc='rc6')]%>%
  as.data.frame(.)
dump(list='eennx',file='eennd.R')
rm(list='eennx')
source('eennd.R') #unpack 1/2
eennx <- data.table(eennx) #unpack 2/2
eennx[] #check it



#-------------------believed junk
# 
# 
# #f230414aFun(
#   soar=x121[nchar(rcx)==6][,rc6:=rcx]
#   geo=geo1
#   xso=eennx
#   maxrad=50
#   maxpeer=50
# 
#   x1 <- #distance rank
#     xso[geo[,.(rc6)],.(rc6,eex,nnx,one=1),on=c(rc='rc6')]%>%
#     .[.,mult='all',on=c(one='one'),allow=T]%>%
#     .[,.(rc6,other=i.rc6,r=round(sqrt((eex-i.eex)^2+(nnx-i.nnx)^2)/1000,2))]%>%
#     .[r<maxrad]%>%
#     .[order(rc6,r)]%>%
#     .[,data.table(.SD,dr=1:.N),rc6]
#   ll <- list(NULL)
#   for(i in 3:maxpeer) {
#     x2 <- x1[dr<=i]
#     ll[[i]] <- 
#       soar%>%
#       .[x1[dr<=i],on=c(rc6='other'),allow=T,nomatch=NULL]%>%
#       .[order(i.rc6,r),.(rc6=i.rc6,other=rc6,r,nid,ppm2)]%>%
#       .[,.(other,lrnk=rank(ppm2),nid,ppm2),.(rc6)]%>%
#       .[order(lrnk),.(rc6,other,arnk=(lrnk-1)/(i-1),nid,ppm2,peers=i)]%>%
#       .[rc6==other]
#   }
#   x3 <- #orthogonalise vs national rank
#     rbindlist(ll)%>%
#     .[,.(arnk=mean(arnk),nid=nid[1]),.(rc6)]%>%
#     .[soar[,.(rc6,nrnk=(rank(ppm2)-1)/(.N-1),nid,pv,m2,ppm2)],on=c(rc6='rc6'),nomatch=NULL]%>%
#     .[,.(rc6,nid,pv,m2,ppm2=round(ppm2),des=round(residuals(lm(arnk~nrnk,.))*1e6))]%>%
#     .[order(des)]
#   x3