

#603b is what is amended now
#605a is called on 4 solutions and gives all we want
#to integrate in the reports add another element in top of summary,glance,tidy: combo
#if we have one row per specification (DTC,NUTS) then 5 rows per geo, one per : dtc, nuts so 8 rows
#cols are:  
#NEW TABLE 9------------------------this could go straight after drc (and dtc?), justifying dtc
rbind(
  cbind(x3$glance[1,.(r.squared,sigma)],as.data.table(as.list(x3$tidy[lm==5,estimate]))%>%setnames(.,c('a0','ad','am','ac'))),
  cbind(x3$glance[2,.(r.squared,sigma)],as.data.table(as.list(x3$tidy[lm==4,estimate]))%>%setnames(.,c('a0','ad','am','ac'))),
  cbind(x3$glance[3,.(r.squared,sigma)],as.data.table(as.list(c(x3$tidy[lm==3,estimate],NA)))%>%setnames(.,c('a0','ad','am','ac'))),
  cbind(x3$glance[4,.(r.squared,sigma)],as.data.table(as.list(c(x3$tidy[lm==2,estimate][1],NA,x3$tidy[lm==2,estimate][2],NA)))%>%setnames(.,c('a0','ad','am','ac'))),
  cbind(x3$glance[5,.(r.squared,sigma)],as.data.table(as.list(c(x3$tidy[lm==1,estimate][1:2],NA,NA)))%>%setnames(.,c('a0','ad','am','ac')))
)[,ay:=ad*365.25][]
#new table 9:
rbind(
  x140$combo,
  x180$combo
)

#table 4------------------------------------------------------------------------
# geo <- x141$geo
# estdt <- x141$ses$stat[type=='all'][x141$ses$estdt,on=c(nx='nx')]
# x41 <- soar[geo,on=c(rcx='rc9')][,.(ppm2=sum(pv)/sum(m2),ppm2min=min(ppm2),ppm2max=max(ppm2),nid=sum(nid)),nx][,nidfrac:=round(nid/sum(nid),4)][]
# x42 <- estdt[,.(mu=mean(xdot),sigma=sd(xdot),rho=acf(xdot,lag.max=1,plot=F)$acf[2],dd=min(xdot),rsq=rsq[1]),nx]
# x43 <- x142$beta[,.(nx,b1,b2,b3,thetab=theta,dthetab=dt)]
# x44 <- x41[x42,on=c(nx='nx')][x43,on=c(nx='nx')][,.(nx,ppm2min,ppm2max,nid,rsq,mu,sigma,rho,dd,b1,b2,b3,thetab,dthetab)]%>%
#   round(.,3)%>%
#   .[,ppm2min:=round(ppm2min,-1)]%>%
#   .[,ppm2max:=round(ppm2max,-1)]%>%
#   .[] #all properties
# x45 <- x44[order(-nx),.(nx,ppm2min,ppm2max,nid.k=round(nid/1000),nid.f=round(nid/sum(nid),4),rsq,b1,b2,b3,thetab=round(thetab,2),dthetab=round(dthetab,2))]


#REPLACE TABLE 4 WITH 2 PANELs: DTC, NUTS
x45 <- f230601b(
  fis=x141,
  rib=x142,
  pva=x121)
print('table 4---------------nx properties')
print(x45)
fwrite(x45,'table4.csv')
#this can go in tables if x140 and x180 are done it is a 2panel x 10row table
#is also 
#dtc
x1 <- f230601b(
  fis=x140$fis,
  rib=x140$rib,
  pva=x121)
#nuts
x2 <- f230601b(
  fis=x180$fis,
  rib=x180$rib,
  pva=x121)
rbind(x1,x2)[,r:=sqrt(b2^2+b3^2)][,-c('b1','b2','b3')]

#this is a useful summary stat but v similar .97,.84
x1[,.(weighted.mean(rsq2,nid.k),weighted.mean(rsq,nid.k))]
x2[,.(weighted.mean(rsq2,nid.k),weighted.mean(rsq,nid.k))]

#TABLE8 # tab8 is RIB : it adds nothing to tab4 which now includes nuts
x142$beta

#TABLE5 # tab5 is RIB on des, use same outputs here





