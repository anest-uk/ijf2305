#show these 2 together
x151$beta #equi-price
x178[n0%in%c(1,5,9,8,7)] #equi-region

#NUTS quintiles

# x1 <- #quintiles within NUTS regions
#   x121[geo2,on=c(rcx='rc9')]%>%
#   .[,.SD[,.( nid,m2,pv,ppm2,rcx,nx,qq=ceiling(5*cumsum(nid)/sum(nid))[])],lab]%>%
#   .[,.(nid,rcx,n0=nx,qq,nx=(nx-1)*5+qq)]
# x177 <- #solve
#   f230311b(#//parallel
#     geo=x1[,.(rc9=rcx,nx,qai=1:.N,lab=labxnnn(nx))],
#     steppra='ver002\\07pra',#ver002=drc
#     stepsip='ver001\\02sip'
#   )[c('ses','geo')]#//
# x2 <- #RIB regress in beta(x,z),theta
#   f230506b( 
#     nxx=x1[,sort(unique(nx))],
#     estdtx=x177$ses$estdt, #regressand x
#     pcax=x133, #regressor z
#     kbar=3
#   )
# x178 <- 
#   x1[,.(nx,n0,qq)]%>%
#   unique(.)%>%
#   .[x2$beta,on=c(nx='nx')]%>%
#   .[,col:=as.factor(n0)]%>%
#   .[unique(geo2[,.(nx,lab)]),on=c(n0='nx')]%>%
#   .[nname,on=c(i.lab='code')]%>%
#   .[,col:=as.factor(paste0(n0,name))]
# ggplot(x178[n0%in%c(1,5,9,8,7)],aes(b2,b3,color=col))+geom_point()+geom_path()

#junk----------------------------------------------------------
x174 <- f230311b( #solve - not done until now, NUTS on drc
    geo=geo2, #NUTS
    steppra='ver002\\07pra',#DRC
    stepsip='ver001\\02sip'
  )

x175 <- #RIB regress in beta(x,z) for NUTS
  f230506b( 
    nxx=geo2[,sort(unique(nx))], #NUTS
    estdtx=x174$ses$estdt, #regressand x = NUTS
    pcax=x133, #regressor z = DTC
    kbar=3
  )

source('nname.r') #dump of nname {code,name}
nn=c(geo.dtc='geo1',
     geo.nuts='geo2',
     soar='x121',
     beta.n='x142',
     nuts.drc='x174',
     pca='x133'
     )

p.theta.lin <-  #lppm2(theta) for DTC
  geo1%>% #DTC
  .[x121,on=c(rcx='rcx'),nomatch=NULL]%>% #Soar
  .[,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),nx]%>%
  .[,lppm2:=log(ppm2)]%>%
  .[,.(nx,ppm2,lppm2)]%>%
  x142$beta[.,on=c(nx='nx')]%>% #DTC RIB
  .[order(nx)]%>%
  .[,approxfun(x=theta,y=lppm2)]

x176 <- geo2%>% #NUTS
  .[,.(rc6=rc9,nx,nutscode=lab)]%>%
  nname[.,on=c(code='nutscode')]%>%
  .[x121,on=c(rc6='rcx'),nomatch=NULL]%>% #soar
  .[,.(m2=sum(m2),pv=sum(pv),nid=sum(nid),ppm2=sum(pv)/sum(m2)),.(code,name,nx)]%>%
  .[x175$beta,on=c(nx='nx')]%>% #NUTS RIB
  .[,.(name,theta,rbarsq,a,at,ppm2,phat=exp(p.theta.lin(theta)))]%>%
  .[order(-ppm2)] #,cor(log(ppm2),log(phat))

p.theta.pw.lin <- 
  geo1[x121,on=c(rcx='rcx'),nomatch=NULL]%>%
  .[,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),nx]%>%
  .[,lppm2:=log(ppm2)]%>%
  .[order(nx),.(nx,ppm2,lppm2)]%>%
  x142$beta[.,on=c(nx='nx')]%>%
  .[order(lppm2)]%>%
  .[,approxfun(x=theta,y=lppm2)]  

x121[nchar(rcx)==6][geo2[,.(rc6=rc9,nx,lab)],on=c(rcx='rc6')]
x2 <- x1[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),nid=sum(nid),ppm2=sum(pv)/sum(m2)),.(nx)]

x174 <- f230311b( #solve
    geo=geo2, #NUTS
    steppra='ver002\\07pra',#drc
    stepsip='ver001\\02sip'
  )
x174$geo[,nutscode:=lab]
x174$geo[,lab:=labxnnn(nx)]
x174$ses$estdt[,rc3:=labxnnn(nx)]
x142 <- #RIB regress in beta(x,z)
  f230506b( 
    nxx=geo2[,sort(unique(nx))],
    estdtx=x174$ses$estdt, #regressand x
    pcax=x133, #regressor z
    kbar=3
  )
x142 #NUTS/drc RIB betas, theta
x142$beta[,summary(rsq)]

#ppm2(nuts)
#theta/lppm2 relation as a pair of functions derived from x(nx)
#theta(nuts-lppm2)
#lppm2(nuts-theta)


x1 <- x121[nchar(rcx)==6][geo2[,.(rc6=rc9,nx,lab)],on=c(rcx='rc6')]
x2 <- x1[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),nid=sum(nid),ppm2=sum(pv)/sum(m2)),.(nx)][order(ppm2)]
x2

source('NUTSnames2.r') #dump of nnames
x3 <- unique(nname[x174$geo[,.(nx,nutscode)],on=c(code='nutscode')])[order(nx)]
x4 <- x3[x2,on=c(nx='nx')][order(ppm2)] #ppm2(nuts)
x5 <- x142$beta[x4,on=c(nx='nx')][,lppm2:=log(ppm2)]
x5[,plot(lppm2,theta)]

ggplot(x5,aes(b2,b3,label=name))+geom_point()+geom_label()

geo1 #DTC
#lppm2(nx)
x6 <-  #lppm2,beta,theta(nx)
  geo1[x121,on=c(rcx='rcx'),nomatch=NULL]%>%
  .[,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),nx]%>%
  .[,lppm2:=log(ppm2)]%>%
  .[order(nx),.(nx,ppm2,lppm2)]%>%
  x142$beta[.,on=c(nx='nx')]%>%
  .[order(lppm2)]#%>%
  #.[,approxfun(x=theta,y=lppm2)]  
x7 <- x6[,approxfun(x=lppm2,y=theta)]
x8 <- x6[,approxfun(x=theta,y=lppm2)]
x5[,thetainf:=x7(lppm2)]
x5[,lppm2inf:=x8(theta)]

plot(x5[,.(theta,thetainf)])
abline(0,1)
plot(x5[,.(lppm2,lppm2inf)])
abline(0,1)
x5[,cor(lppm2,lppm2inf)]
x5[,cor(theta,thetainf)]

ggplot(x5,aes(lppm2,lppm2inf,label=name))+geom_point()+geom_label_repel(size=2)
x9 <- rbind(x6[nx<10,.(lppm2,lppm2inf=x8(theta),lab=zeroprepend(nx,1),type='n')],x5[,.(lppm2,lppm2inf=x8(theta),lab=name,type='NUTS')])[,col:=reorder(as.factor(type),lppm2)]
cor(x6[,.(lppm2,x8(theta))])
x9[col=='n',lab:='']
ggplot(x9,aes(lppm2,lppm2inf,color=col))+geom_point()+geom_label_repel(aes(label=lab),size=3,seed=11,force=3,min.segment.length=.05,direction='y')+
  xlim(c(7.5,9))+ylim(c(7.5,9))
?geom_label_repel
#nuts regress on z
