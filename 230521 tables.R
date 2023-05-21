#dependencies: see .Rprofile for CRAN + local library c-cleanlib
#working dir should be same as 'public update' to load objects from file
#outputs are csv 
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
  nuts5='x178'#g
)
#individual objects quite fast
for(i in seq_along(nn)) {
  load(file=paste0(nn[i],'.Rdata'))
}

#------------------table 1 regstats, annual
arealabel <- pxmoreg()[,.(rc,area,name)][rc=='WC-',name:='Central London'][rc=='N--',name:='North London'][area=='TS',name:='Teesside']
x10 <- data.table(seq=1:10,rc3=c('TS-','L--','S--','LS-','M--','B--','BS-','AL-','N--','WC-'))
x12 <- x103$ses$estdt[x10[,.(rc3)],on=c(rc3='rc3')]%>%
  .[x103$ses$stat[type=='all',.(nx,rsq,nsam)],on=c(nx='nx'),allow=T,nomatch=NULL]
x13a <- x121 #f230423ad
x12a <- x12[,.(date=date1,lab=rc3,xdot)]%>%
  dcast(.,date~lab,value.var='xdot')%>%
  pcaest(.,rotate=T)%>%#do need to rotate for sensible 'rewarded' beta
  pcajscale(beta=T)%>%
  pcab(.)%>%
  .[,1,drop=F]%>%
  data.table(.,keep.rownames=T)%>%
  setnames(.,c('rc3','beta'))
x13 <- x12[,.(
  mean=mean(xdot),
  sigma=sd(xdot),
  rho=acf(xdot,lag.max=1)$acf[2],
  rsqraw=rsq[1],
  dd=min(xdot)
),nx]%>%
  lapply(.,round,digits=3)%>%
  as.data.table(.)%>%
  .[unique(x12[,.(nx,lab=rc3)]),on=c(nx='nx')]%>%
  .[x12a,on=c(lab='rc3')]%>%
  .[order(nx),.(rc3=lab,mean,sigma,rho,dd,beta=round(beta,digits=3),rsqraw=round(rsqraw,3))]%>%#%>%.[,cor(dd,beta)] #cor is .871
  arealabel[.,on=c(rc='rc3')]%>%
  x13a[,.(ppm2=round(ppm2,-1),rcx)][.,on=c(rcx='rc')]%>%
  .[order(-ppm2),.(area,name,mean,sigma,rho,dd,beta,rsqraw)]#,ppm2
print('table 1----------------regstats annual')
print(x13)
fwrite(x13,'table1.csv')
cor(x13[,.(beta,dd,mean)])
#------------------end table 1

#------------------table 2 drc per period report
names(x132) #FIS = "geo" "ses" "bso" 
x21 <- x132 #DRC solve f230311b
x22 <- x132$ses$estdt
x23 <- dcast(x22,date1~rc3,value.var='xdot')
dates <- x22[,(sort(unique(c(date0,date1))))]
days <- as.numeric(diff(dates))
x24 <- x133 #pca
dates <- c(as.Date('1994-12-31'),x24$date)
days <- as.numeric(diff(dates))
x23 <- x24$x
x25 <- pcab(x24)[,1:3]
x27 <- pcaz(x24)[,2:3]
x28 <- sweep(x27,MAR=2,STAT=apply(x27,2,mean),FUN=`-`)
apply(x28^2,2,mean) #important: tseries ML variance is exactly 1 as expected
rcen <- apply(x28^2,1,sum)
rraw <- apply(x27^2,1,sum)
x29 <- NA
i <- 1
for(i in 1:nrow(x23)) {
  x6 <- data.table(y=unlist(t(x23[i,-1])),x=x25)%>%
    setnames(.,c('y','b1','b2','b3'))
  x29[i] <- summary(lm(y~.,x6))$adj.r.squared
}
x210 <- data.table(days=days,rbarsq=x29,r=rraw)#,rcen=rcen
x211 <- cbind(data.table(end=dates[-1]),round(x210,2))
x2111 <- as.data.table(lapply(x211,as.character))
x2112 <- as.data.table(as.list(c('mean',round(apply(x211[,-1],2,mean),c(0,2,2)))))
x2113 <- cbind(x2111[1:14],x2111[15:28],rbind(x2111[29:41],x2112,use.names=F))
print('table 2---------------drc per period')
print(x2113) 
fwrite(x2113,'table2.csv')
#------------------end table 2

#table 3------------------------------------------------------------------------
soar <- x103$ses$soar
soar <- x121
x30 <- data.table(seq=labxnnn(1:10),rc3=c('TS-','L--','S--','LS-','M--','B--','BS-','AL-','N--','WC-'))
x32 <- setnames(data.table(x25,keep.rownames=T),c('rc3',paste0('b',1:3)))%>%
  .[,theta:=signal::unwrap(atan2(b3,b2))]%>%
  .[theta<(-pi/2),theta:=theta+2*pi]%>% #unwrap not working
  x30[.,on=c(seq='rc3')]%>%
  .[,.(rc3,b1,b2,b3,theta)]
x33 <- soar[,.(rcx,ppm2=round(ppm2))][x32,on=c(rcx='rc3')][,.(rcx,b1=round(b1,3),b2=round(b2,3),b3=round(b3,3),theta=round(theta,3),ppm2=round(ppm2,-1))]
print('table 3---------------rc3 beta and ppm2')
print(x33)
x33[,plot(log(ppm2),theta)]
x33[,cor(theta,log(ppm2))]
fwrite(x33,'table3.csv')
#------------------end table 3

#table 4------------------------------------------------------------------------
geo <- x141$geo
estdt <- x141$ses$stat[type=='all'][x141$ses$estdt,on=c(nx='nx')]
x41 <- soar[geo,on=c(rcx='rc9')][,.(ppm2=sum(pv)/sum(m2),ppm2min=min(ppm2),ppm2max=max(ppm2),nid=sum(nid)),nx][,nidfrac:=round(nid/sum(nid),4)][]
x42 <- estdt[,.(mu=mean(xdot),sigma=sd(xdot),rho=acf(xdot,lag.max=1)$acf[2],dd=min(xdot),rsq=rsq[1]),nx]
x43 <- x142$beta[,.(nx,b1,b2,b3,thetab=theta,dthetab=dt)]
x44 <- x41[x42,on=c(nx='nx')][x43,on=c(nx='nx')][,.(nx,ppm2min,ppm2max,nid,rsq,mu,sigma,rho,dd,b1,b2,b3,thetab,dthetab)]%>% 
  round(.,3)%>%
  .[,ppm2min:=round(ppm2min,-1)]%>%
  .[,ppm2max:=round(ppm2max,-1)]%>%
  .[] #all properties
x45 <- x44[order(-nx),.(nx,ppm2min,ppm2max,nid.k=round(nid/1000),nid.f=round(nid/sum(nid),4),rsq,b1,b2,b3,thetab=round(thetab,2),dthetab=round(dthetab,2))]
print('table 4---------------nx properties')
print(x45)
fwrite(x45,'table4.csv')
#------------------end table 4

#table 5: radial----------------------------------------------------------------
f1 <- function(nxx=4,x=x153) {
  x51 <- x[ndtc==nxx,.(qq,t1=atan2(b3,b2),r1=sqrt(b3^2+b2^2),ndtc)][,.(ndtc,qq,r1,t1)]#lapply(.,round,digits=3)%>%as.data.table(.)
  dcast(x51,ndtc~qq,value.var='r1')[dcast(x51,ndtc~qq,value.var='t1'),on=c(ndtc='ndtc')][,c(1,2,6,7,11)]%>%
      setnames(.,c('nx','r1','r5','t1','t5'))%>%.[,dtheta:=t5-t1]%>%.[,dr:=(r5-r1)*2/(r1+r5)]%>%.[]
}
x52 <- lapply(3:6,f1)%>%
  rbindlist(.)%>%
  lapply(.,round,digits=4)%>%
  as.data.table(.)
print('table 5---------------radial')
print(x52)
fwrite(x52,'table5.csv')
#------------------end table 5

#table 6: attrib----------------------------------------------------------------
x12 <- f230424a(d1='2016-03-31',x12=x133,rib=x142)
x62 <- as.matrix(as.data.table(lapply(predict(x161$var7p3,n.ahead=7)$fcst,`[`,,j='fcst')))
x63 <- data.table(x12,chat=round(t(tail(sweep(x62,STAT=x62[1,],FUN=`-`,MAR=2)%*%t(pcab(x133)[,2:3]),1)),4)[,1])%>%
  .[,.(n=1:.N,m,c,r,t,chat)]
print('table 6---------------attrib')
print(x63)
fwrite(x63,'table6.csv')
#------------------end table 6

#table 7: VAR diagnostics-------------------------------------------------------
f2 <- function(x1=x161$var0,estdt=x141$ses$estdt) {
  x3 <- as.numeric(estdt[,mean(diff(sort(unique(c(date0,date1)))))]/365.25)
  x4 <- roots(x1,mod=F)
  x2 <- rbind(
    data.table(stat='root',value=roots(x1)[1]),
    data.table(stat='serial',value=serial.test(x1)$serial$p.value),
    data.table(stat='norm',value=normality.test(x1)$jb.mul$JB$p.value[1,1]),
    data.table(stat='arch',value=arch.test(x1)$arch.mul$p.value),
    data.table(stat='Z2R2',value=summary(x1)$varresult$X002$adj.r.squared), #nb adjusted
    data.table(stat='Z3r2',value=summary(x1)$varresult$X003$adj.r.squared),
    data.table(stat='period',value=2*pi/atan2(Im(x4)[1],Re(x4)[1])*x3)
  )[,value:=round(value,3)]
  x2[]
}
x72 <- f2(x161$var0)[,.(stat,p2=value)][f2(x161$var0p3)[,.(stat,p3=value)],on=c(stat='stat')][f2(x161$var7p3)[,.(stat,p3.lag7=value)],on=c(stat='stat')]
print('table 7---------------VAR')
print(x72[])
fwrite(x72,'table7.csv')
#------------------end table 7

