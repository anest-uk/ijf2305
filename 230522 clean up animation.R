nn=c(
  sol3DTC='x142',
  sol4DES='x153',
  sol8NUTS='x178'
  )
n0x <- c(1,5,9,8,7)#selected NUTS regions
x1 <-  #n in {1,10}
  x142$beta[,.(ndtc=nx,b1,b2,b3,theta)][ndtc%in%c(1,10)]
x2 <- #solution 4 15x DES/DRC p=const | indexed 1:20 on ndes
  x153[,.(ndtc,qq,ndes,b1,b2,b3,theta)] #20 dtiles, n=3:6; (ndtc-3)*5+qq = ndes{1:20}
x3 <- #solution 8 17x QNUT/DRC region=const
  x178[n0%in%n0x,.(nx,n0,qq,name,b1,b2,b3,theta)]
x4 <-  #geo for dtc, nx in 1,10
  geo1[nx%in%c(1,10)]
x5 <-  #geo for des split; ndtc in 3:6
  x153[,.(ndtc,qq,ndes)][geo4,on=c(ndes='ndes')]
x6 <-  #geo for NUTS, select 5 regions
  geo3[n0%in%n0x]
x7 <- #lppm2 for these using geo on x121
  rbind(
  x121[x4[,.(rcx,type='dtc',nx)],on=c(rcx='rcx')][,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2),type=type[1],qq=0),nx],
  x121[x5[,.(qq,rcx=rc6,type='des',nx=ndes)],on=c(rcx='rcx')][,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2),type=type[1],qq=qq[1]),nx],
  x121[x6[,.(qq,rcx,type='nut',nx)],on=c(rcx='rcx')][,.(pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2),type=type[1],qq=qq[1]),nx]
)[,lppm2:=log(ppm2)][]
x8 <- #x=theta_rep, y=lppm2, b2,b3
  rbind(
  x7[type=='dtc'][,.(type,ppm2,nx,qq)][x1,on=c(nx='ndtc')][,.(type,ppm2,b1,b2,b3,theta,tbar=mean(theta),qq),.(nx)],
  x7[type=='des'][,.(type,ppm2,nx,qq)][x2[,.(ndtc,ndes,b1,b2,b3,theta)],on=c(nx='ndes')][,.(type,ppm2,b1,b2,b3,theta,tbar=theta,qq),.(nx=ndtc)],
  x7[type=='nut'][,.(type,ppm2,nx,qq)][x3[,.(nx,b1,b2,b3,theta)],on=c(nx='nx')][x3[,.(n0,nx)],on=c(nx='nx')][,.(type,ppm2,b1,b2,b3,theta,tbar=theta,qq),.(nx=n0)]
)
x8%>%
  .[,colcode:=nx]%>%
  .[type=='des',colcode:=20+colcode]%>%
  .[type=='nut',colcode:=30+colcode]%>%
  .[,col:=as.factor(colcode)]%>%
  .[,vv:=labxnnn(1:.N)]
# x8a <- ggplot( #test plot
#   x8,
#   aes(tbar,ppm2,color=col)
#   )+
#   geom_point()+
#   geom_path()+
#   scale_y_log10()
# print(x8a)
# 
# x8[,as.integer(col)]
# x8[,col]
# x8b <- x8[,(levels(col))]
# x8c <- gg1(length(x8b))
# x8c[3:6] <- paste0('grey',seq(from=30,to=60,by=10))
# #scm1 <- setNames(x8b,x8c) #nooooo!
# scm <- setNames(x8c,x8b) #colour codes, named by levels

#x8copy <- copy(x8)
x8 <- copy(x8copy)
y1 <- x8[,col]
levels(y1)[7:11] <- c('Northeast','Midlands','London','Southeast','Southwest')
levels(y1)[1:2] <- c('n=1 (unsplit)','n=10 (unsplit)')
levels(y1)[3:6] <- paste0('n=',3:6,' (des-split)')
#y1 <- reorder(y1,-as.numeric(y1))
x8[,col:=y1]
#x8[,col:=reorder(col,theta)]

x8a <- c(gg1(13)[c(1,13)],paste0('grey',seq(from=60,to=30,by=-10)),c(gg1(13)[c(3,5,11,9,7)]))
scm <- setNames(x8a,x8[,(levels(col))])

as.numeric(xxmap[as.numeric(x8[,col])])
as.numeric(xmap[as.numeric(x8[,col])])

x8
xmap <- c('1'=2,'2'=9,'3'=10,'4'=11,'5'=8,'6'=7,'7'=6,'8'=5,'9'=4,'10'=3,'11'=1)
xxmap <- setNames(as.numeric(names(xmap)),as.character(xmap))
xmap[c('1','2','3','4')]
x8[,col]
#<<<<got to here


ggplot( #test plot
  x8,
  aes(tbar,ppm2,color=col)
  )+
  scale_colour_manual(values=scm[c(2,9,10,11,8,7,1,6,5,4,3)])+
  geom_point()+
  geom_path()+
  scale_y_log10()


x9 <- #backdate by applying x23=z23.beta23
  pcaz(x133)[,2:3]%*%t(x8[,.(b2,b3)])%>%
  zoo(.)%>%
  cumsum(.)
x10 <-
  sweep(x9,STAT=x9[nrow(x9),],FUN=`-`,MAR=2)%>%
  data.table(.)%>%
  setnames(.,labxnnn(1:ncol(.)))%>%
  .[,ii:=(1:.N)]%>%
  melt(.,id.vars='ii')
x11 <-
  x10[x8,on=c(variable='vv')]%>%
  .[,.(ii,ppm2=ppm2*exp(value),theta,col)]%>%
  .[order(col,theta,ppm2)]
#x11[,lab:=NA]
#x11[ii==min(ii),lab:='a']


x12 <-
  ggplot(
    x11,
    aes(theta,ppm2,color=col,frame=ii)
    )+
  scale_colour_manual(values=scm[c(2,9,10,11,8,7,1,6,5,4,3)])+
  geom_point()+
  geom_path()+
  scale_y_log10()+
  ylab(paste('2023 \u00A3/m<sup>2</sup>'))+
  xlab(paste('\u03B8<sub>\u03B2</sub>'))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dashed",color='grey90'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=12,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=12,face = "plain"),
    legend.position='left')
x13 <- ggplotly(x12)%>%
  animation_opts(frame=120,transition=70,redraw=F,mode='next')%>%
    animation_button(label="time bin >")%>%
  print(.)

  