load('x142.rdata') #DTC sol3
load('x153.rdata') #DES sol4
load('x178.rdata') #NUTS sol8
#RIB keyed on ndtc,qq,ndes : DES-split on qq=1:5
#beta23
#ndes on ndtc=3:6 = 20, constant price vble des
#n: 1:5 = 25
#additional n=1,10

#BETA
#solution 3 dtc
x1 <- x142$beta[,.(ndtc=nx,b1,b2,b3,theta)][ndtc%in%c(1,10)] #just extrema
#solution 4 15x DES/DRC p=const | indexed 1:20 on ndes
x2 <- x153[,.(ndtc,qq,ndes,b1,b2,b3,theta)] #20 dtiles, n=3:6; (ndtc-3)*5+qq = ndes{1:20}
#solution 8 17x QNUT/DRC region=const
n0x <- c(1,5,9,8,7)
x3 <- x178[n0%in%n0x,.(nx,n0,qq,name,b1,b2,b3,theta)]
#GEO
x4 <- geo1[nx%in%c(1,10)] #geo for dtc, nx in 1,10
x5 <- x153[,.(ndtc,qq,ndes)][geo4,on=c(ndes='ndes')] #geo for des split; ndtc in 3:6
x6 <- geo3[n0%in%n0x] #geo for NUT-1; has n0,qq,nx; n0x in 1,5,9,8,7
#lppm2 for these using geo on x121
x7 <- rbind(
  x121[x4[,.(rcx,type='dtc',nx)],on=c(rcx='rcx')][,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2),type=type[1],qq=0),nx],
  x121[x5[,.(qq,rcx=rc6,type='des',nx=ndes)],on=c(rcx='rcx')][,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2),type=type[1],qq=qq[1]),nx],
  x121[x6[,.(qq,rcx,type='nut',nx)],on=c(rcx='rcx')][,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2),type=type[1],qq=qq[1]),nx]
)[,lppm2:=log(ppm2)][]
#x=theta_rep, y=lppm2, b2,b3
x8 <- rbind(
  x7[type=='dtc'][,.(type,ppm2,nx,qq)][x1,on=c(nx='ndtc')][,.(type,ppm2,b1,b2,b3,theta,tbar=mean(theta),qq),.(nx)],
  x7[type=='des'][,.(type,ppm2,nx,qq)][x2[,.(ndtc,ndes,b1,b2,b3,theta)],on=c(nx='ndes')][,.(type,ppm2,b1,b2,b3,theta,tbar=theta,qq),.(nx=ndtc)],
  x7[type=='nut'][,.(type,ppm2,nx,qq)][x3[,.(nx,b1,b2,b3,theta)],on=c(nx='nx')][x3[,.(n0,nx)],on=c(nx='nx')][,.(type,ppm2,b1,b2,b3,theta,tbar=theta,qq),.(nx=n0)]
)
x8[,colcode:=nx][type=='des',colcode:=20+colcode][type=='nut',colcode:=30+colcode][,col:=as.factor(colcode)]
ggplot(x8,aes(tbar,ppm2,color=col))+geom_point()+geom_path()+scale_y_log10() #here des is degenerate because x=tbar=const and y=lppm2~const ; nut is pric-dispersed
#backdate by applying z23 to the betas in x8
x9 <- cumsum(zoo(pcaz(x133)[,2:3]%*%t(x8[,.(b2,b3)])))
x10 <- sweep(x9,STAT=x9[nrow(x9),],FUN=`-`,MAR=2)%>%
  data.table(.)%>%
  setnames(.,labxnnn(1:ncol(.)))%>%
  .[,ii:=1-(.N:1)]%>%
  melt(.,id.vars='ii')
x8[,vv:=labxnnn(1:.N)]
x11 <- x10[x8,on=c(variable='vv')][,.(ii,ppm2=ppm2*exp(value),theta,col)][order(col,theta,ppm2)]
ggplotly(
  ggplot(x11,aes(theta,ppm2,color=col,frame=ii))+geom_point()+geom_path()+scale_y_log10()+ylab(paste('2023 \u00A3/m<sup>2</sup>'))+xlab(paste('\u03B8<sub>\u03B2</sub>'))
)%>% animation_opts(frame=120,transition=70,redraw=F,mode='next')


unique(x8[,.(type,nx,qq)]) #unique key
#x(nx,qq,type,m)
x141a$ses$estdt[nx%in%c(1,10)] #solution 3 key nx=1:10 = easy
x8[type=='dtc'][nx%in%c(1,10)]

x151a$ses$estdt[nx%in%1:20] #solution 4 key nx=1:20 5des-tiles on 10ndtc 
x8[type=='des'][nx%in%1:20]

#so now the harder one....
xx <- unique(geo3[,.(nx,n0,qq)])[n0%in%n0x] #the 25, with nx the key into...
x177$ses$estdt[nx%in%xx[,nx]] #which is 25x40 rows as required
#animate


x121[ x5[ndtc==6][order(qq)][substr(rc6,1,3)=='E--'][,.(rc6=unique(rc6))],on=c(rcx='rc6')]
 