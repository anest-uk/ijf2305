#dependencies: see .Rprofile for CRAN + local library c-cleanlib
#working dir should be same as 'public update' to load objects from file
#outputs are tif 
# nn <- c( #prepped in 'public update'
#   sol1='x103',
#   sol2='x133',
#   sol3='x141',
#   rib3='x142',
#   sol4='x151',
#   nuts='x153',
#   var='x161',
#   nutq='x178'
# )
# load('xnnn.rdata')
# 
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


labelsize <- 2.5
x1 <- x103$ses$soar%>%
  .[,.(nid,m2,pv,ppm2,rcx=rc9)]%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,6))])%>%
  rbind(.,.[,.(nid=sum(nid),m2=sum(m2),pv=sum(pv),ppm2=sum(pv)/sum(m2)),.(rcx=substr(rcx,1,3))])
gg1 <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg2 <- data.table(code=(gg1(100)),lppm2=seq(from=log(1500),to=log(20000),length.out=100))
gg3 <- gg2[x1[nchar(rcx)==3,.(rcx,lppm2=log(ppm2))],on=c(lppm2='lppm2'),roll='nearest']

#1 solution 1------------------------------------
x0 <- x103[c('ses','geo')]
rcx <- c('TS-','L--','S--','LS-','M--','B--','BS-','N--','AL-','WC-')
rcx1 <- c('WC-','AL-','B--','TS-')
pcx1 <- irregpcode(rcx1)
x1 <- x0$geo[x103$ses$estdt[,.(nx,date0,date1,x,xse)],on=c(nx='nx')]%>%
  .[,ii:=1:.N,nx]%>%
  .[rc9%in%rcx]%>%
  .[x103$ses$estdt[,.(xmax=max(x)),.(nx)],on=c(nx='nx'),nomatch=NULL]%>%
  .[,col:=irregpcode(rc9)]%>%
  .[order(rc9,date1)]
x2 <- rbind(
  x1[date0==min(date0)][,x:=0][,date1:=date0][],
  x1
)[,xlab:=paste0(format(date0,'%Y'),'-01-01')]
colmap <- setNames(setkey(gg3,rcx)[rcx1][,code],pcx1)
x3 <- copy(x2)[,leg:=ifelse(date1==max(date1),col,NA)]
x3a <- x3[rc9%in%rcx1][date1==max(date1)][rc9=='WC-',x:=x+.05]
x <- ggplot(x3[rc9%in%rcx1],aes(date1,x,color=col,label=leg))+
  geom_line(size=.4)+
  geom_point(size=.2)+
  geom_label(data=x3a,size=labelsize,vjust=.25,hjust=-.14)+ ####
  xlab(NULL)+ylab('cumulative log return (x)')+
  scale_color_manual(values=colmap)+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain"),
    legend.position='none')+
  coord_cartesian(xlim=c(as.Date(c('1994-12-31','2024-12-31'))),ylim = c(-.1,2.1))
print(x)
ggsave('rplot001.tif', x, device = "tiff", dpi =1000, width=3543, height=2400, units='px')

#2 rc3 scatter--------------------------------
x0a <- x0$geo[x103$ses$estdt,on=c(nx='nx')][x103$ses$soar[,.(ppm2=sum(pv)/sum(m2)),.(rc3=substr(rc9,1,3))],on=c(rc9='rc3')][]
x1a <- dcast(x103$ses$estdt,date1~rc3,value.var='xdot')%>%
  setnames(.,old='date1',new='date')%>%
  pcaest(.,signmethod='ref',rcref=x0$geo[rc9=='WC-',lab])%>%
  pcab(.)%>%
  `[`(.,,j=2:3)%>%
  data.table(.,keep.rownames=T)%>%setnames(.,old=c('002','003'),new=c('b2','b3'))
x2a <- x103$ses$soar[,.(ppm2=sum(pv)/sum(m2)),.(rc3=substr(rc9,1,3))][rc3%in%rcx][,col:=reorder(irregpcode(rc3),ppm2)]%>%
  x0$geo[.,on=c(rc9='rc3')]
x3a <- x2a[x1a,on=c(lab='rn')]
colmap <- setNames(setkey(gg3,rcx)[x2a[,rc9]][,code],x2a[,col])
axiscol='grey80'
x3a <- x3a[,pointsize:=ifelse(is.na(col),.1,.3
                              )][order(!is.na(col))]
set.seed(124)
x <- ggplot(x3a,aes(b2,b3,label=col,color=col))+
  geom_hline(yintercept=0,color=axiscol,size=.3)+
  geom_vline(xintercept=0,color=axiscol,size=.3)+
  geom_point(aes(size=pointsize))+#size=.5
  xlab(bquote(beta[2]))+ylab(bquote(beta[3]))+
  geom_label_repel(size=labelsize,force=30,force_pull=0,direction='y')+
  scale_color_manual(values=colmap)+
  scale_size(range = c(.1, 2))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain"),
    legend.position='none')+
  coord_cartesian(ylim = c(-.01,.06),xlim=c(-.05,.066))
print(x)
set.seed(124)
ggsave('rplot002.tif', x, device = "tiff", dpi =1000, width=3543, height=2400, units='px')

#3 zm  z23(m)+shifted---------------------------
pointsize=.4
x3 <- data.table(pcaz(x133)[,2:3],keep.rownames=T)%>%
  setnames(.,c('date','b2','b3'))%>%
  .[,.(ii=1:(.N+1),date=as.Date(c('1994-12-31',date)),z2=cumsum(c(0,b2)),z3=cumsum(c(0,b3)))]%>%
  melt(.,id.vars=c('date','ii'))%>%
  .[,.(ii,date,variable=as.factor(variable),value)]%>%
  .[,z:=value-(min(value)+diff(range(value))/2),variable]
x4 <- which.max(dcast(x3,ii~variable,value.var='value')[,ccf(z2,z3,lag.max=10,plot=F)]$acf)-11
x5 <- x3[variable=='z3',.(ii=ii+x4,variable='z3lag',value,z)]
x6 <- rbind(x3[,-'date'],x5)[,linetype:=(variable=='z3lag')]
x6a <- copy(x6)[,leg:=ifelse(ii==41&variable!='z3lag',as.character(variable),NA)]%>%
  .[,leg:=ifelse(ii==43&variable=='z3lag',as.character(variable),leg)]%>%
  .[,col:=as.factor(substr(variable,2,2))]

x <- ggplot(x6a,aes(ii,z,color=col,label=leg))+ 
  geom_line(aes(linetype=linetype)) +
  geom_point(size=pointsize)+
  geom_label(size=labelsize,hjust=-.4,vjust=0.1)+
  xlab('time bin')+
  ylab('cumulative factor (z)')+
  scale_color_manual(values=c('blue','red','red'))+
  guides(linetype = 'none',color='none')+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain"))+
   coord_cartesian(xlim=c(0,50))
print(x)
ggsave('rplot003.tif', x, device = "tiff", dpi =1000, width=3543, height=2400, units='px')

#4 thetaz(m)------------------------------------
x3 <- data.table(pcaz(x133)[,2:3],keep.rownames=T)%>%
  setnames(.,c('date','zdot2','zdot3'))%>%
  .[,.(date,zdot2,zdot3,z2=cumsum(zdot2),z3=cumsum(zdot3))]%>%
  .[,z2:=z2-(min(z2)+diff(range(z2))/2)]%>%
  .[,z3:=z3-(min(z3)+diff(range(z3))/2)]%>%
  .[,thetadot:=signal::unwrap(atan2(zdot3,zdot2))+4*pi]%>%
  .[,theta:=signal::unwrap(atan2(z3,z2))+4*pi]%>%
  .[,ii:=1:.N]%>%
  melt(.,id.vars=c('ii','date'),measure.vars=c('theta','thetadot'))
  periodm <- -(2*pi)/coef(lm(value~ii,x3[variable=='theta']))[2]
  perioddays <- as.numeric(round(periodm*x3[variable=='theta',diff(range(as.Date(date)))/diff(range(ii))],0))
  periodyrs <- as.numeric(round(periodm*x3[variable=='theta',diff(range(as.Date(date)))/diff(range(ii))]/365.25,1))
x <- ggplot(x3,aes(ii,value,color=variable))+
  geom_point(size=.8)+
  geom_smooth(data=x3[variable=='thetadot'],method='lm', formula= y~x,size=.2,se=F)+
  theme_bw() +
    scale_y_continuous(
      minor_breaks = seq(0 , 100, 5), 
      breaks = seq(0, 100, pi),
      labels = label_number(accuracy = 0.01)
      )+
    scale_x_continuous(
      breaks = c(14,40),
      )+
    theme(
      legend.position='none',
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain")
    )+
  ylab(bquote('factor phase '~theta[z]~'='~tan^-1~'('~z[3]/z[2]~')'))+
  xlab('time bin')+
  scale_color_manual(values=rev(gg2[c(40,90),code]))+
  geom_segment(x=x3[,max(ii)-periodm],y=0,xend=x3[,max(ii)-periodm],yend=2*pi,size=.04,linetype='dotted',color='grey20')+
  geom_segment(x=x3[,max(ii)-periodm],y=0,xend=x3[,max(ii)],yend=0,size=.04,linetype='dotted',color='grey20')+
  annotate('text',x=x3[,max(ii)]-periodm,y=0,label=as.character(as.Date('2023-02-28')-perioddays),hjust=1.1,vjust=-.4,size=2,angle=270)+
  annotate('text',x=x3[,max(ii)]-periodm/2,y=0,label=paste0('one period = ',periodyrs,' yrs'),hjust=.55,vjust=-.13,size=2,angle=0)+
  annotate('text',x=x3[,max(ii)],y=0,label=paste0('z'),hjust=-.55,vjust=.5,size=4,angle=0,color=gg2[40,code])+
  annotate('text',x=x3[,max(ii)],y=0,label=paste0('.'),hjust=-.8,vjust=-.2,size=6,angle=0,color=gg2[40,code])+
  annotate('text',x=x3[,max(ii)],y=1.5,label=paste0('z'),hjust=-.55,vjust=.5,size=4,angle=0,color=gg2[90,code])
print(x)
ggsave('rplot004.tif', x, device = "tiff", dpi =1000, width=3543, height=2400, units='px')
  
#5 b2/b3 the wheel--------------------------
x8 <- rbind(
  x142$beta[,.(theta,b2,b3,label=nx,type='price-bin',lab=reorder(nx,-nx))]
)[,type:=as.factor(type)]
rad=.017
wheel <- data.table(theta=seq(from=0,to=2*pi,length.out=50))[,x:=rad*cos(theta)][,y:=rad*sin(theta)][,lab:=as.character(11)]
colmap <- setNames(c(gg1(10),'grey50'),as.character(1:11))
x8[,lab:=as.character(lab)][,xunit:=rad*cos(theta)][,yunit:=rad*sin(theta)][,xin:=.8*xunit][,yin:=.8*yunit][,labx:=paste0(ifelse(lab%in%c(1,10),'n=',''),lab)]#[,labx:=ifelse((as.numeric(lab)-1)%%3,'',lab)]
x8[lab==1,labx:=paste0(labx,'    ')]
x8[lab==10,labx:=paste0('      ',labx)]
x <- 
ggplot(x8,aes(b2,b3,color=lab))+
  geom_text(aes(xin,yin,label=labx),size=3)+
  geom_point(data=wheel,aes(x,y,color=lab),size=.001)+
  geom_point(data=x8,aes(xunit,yunit,color=lab),size=1)+
  geom_point(data=x8,aes(b2,b3,color=lab),size=2)+#geom_line(data=x8[type=='price-bin'])
  geom_hline(yintercept=0,color=axiscol,size=.3)+
  geom_vline(xintercept=0,color=axiscol,size=.3)+
  xlab(bquote(beta[2]))+ylab(bquote(beta[3]))+
  scale_color_manual(values=colmap)+
  guides(color = 'none')+
  theme_bw() +
  theme(
    legend.position='none',
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain")
  )
print(x)
ggsave('rplot005.tif', x, device = "tiff", dpi =1000, width=3543, height=2000, units='px')

#6 xn(m)--------------------------------
x9 <- x141$ses$estdt[,.(
    date0,
    date1,
    days,
    xdot,
    x,
    lab=rc3,
    ii=1:.N),                   
    by=nx]
nxx <- c(1,4,7,10)
nxx <- 1:10
x9a <- rbind(
  x9[ii==1,.(ii=0,x=0,nx,col)],
  x9[,.(ii,x,nx,col)]
  )%>%
  .[nx%in%nxx]%>%.[,col:=reorder(as.factor(nx),-nx)]%>%
  .[,leg:=ifelse(ii==39&nx%in%c(1,3,7,10),as.character(col),NA)]
x9b <- x9a[!is.na(leg)]
x9c <- rbind(x9b[nx!=10],x9b[nx==10,.(ii=38.2,x=x+.07,nx,col,leg)])
colmap <- setNames(c(gg1(10),'grey50'),as.character(1:11))
x <- ggplot(x9a,aes(ii,x,color=col,label=leg))+#,label=leg
  geom_line(size=.4)+
  geom_point(size=.2)+
  geom_label(data=x9c,hjust=-.55,size=labelsize,vjust=.2)+
  xlab('time bin')+ylab('cumulative log return (x)')+
  scale_color_manual(values=colmap)+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain"),
    legend.position='none')+
  coord_cartesian(xlim=c(0,44),ylim = c(-.1,2.2))
print(x)
ggsave('rplot006.tif', x, device = "tiff", dpi =1000, width=3543, height=3000, units='px')

#NUTS 7 -----------------------------
n0x <- c(1,5,9,8,7) #select regions
#nxx <- 3:6
ndtcx <- x153[,sort(unique(ndtc))] #already selected ndtc
x1 <- 
  copy(x153)%>% #betas
  .[,lab:='']%>%
  .[ndtc==max(ndtc)&qq==max(qq),lab:='DES>0']%>%
  .[ndtc==max(ndtc)&qq==min(qq),lab:='DES<0']%>%
  .[,shade:='black']%>%
  .[order(ndtc,qq)]
x1[qq==min(qq),shade:='red']
x1[qq==max(qq),shade:='blue']
x2 <- 
  copy(x178)%>% #NUTS quintiles
  .[,nxfac:=reorder(x178$col,-rank(x178$theta))]%>%
  .[n0%in%n0x]%>%
  .[,lab:='']
x2[qq==1,lab:=c('SW','SE','London','Midlands','NE')]
x3 <- copy(x2)[,b2:=.97*b2]
x4 <- 
  ggplot(
    data=x1,
    aes(b2,b3,label=lab)
  )+
  geom_hline(yintercept=0,color=axiscol,size=.3)+
  geom_vline(xintercept=0,color=axiscol,size=.3)

for(i in seq_along(ndtcx)) { #equiprice
  x4 <- x4+
    geom_point( #black triangles
      data=x1[.N:1]%>%
        .[ndtc==ndtcx[i]],
      size=.4,
      shape=25
    )+
    geom_path( #dotted line
      data=x1[.N:1][ndtc==ndtcx[i]],
      linetype='dotted',
      size=.3)
}
x4 <- x4+ #region lines colour keyed
  geom_point(
    data=x2[n0%in%n0x],
    size=.75,
    shape=22,
    aes(color=nxfac,fill=nxfac)
  )+
  geom_path(
    data=x2[n0%in%n0x],
    aes(color=nxfac),
    size=.6
  )
x4 <- x4+ 
  geom_point(#low des red triangles
    data=x1[qq==min(qq)],
    size=1,
    shape=25,
    color='red',
    fill='red'
  )+
  geom_point(#high des blue triangles
    data=x1[qq==max(qq)],
    size=1,
    shape=25,
    color='blue',
    fill='blue'
  )
x4 <- x4+ 
  geom_text_repel( #red DES label
    data=x1[lab=='DES<0'],
    aes(label=lab),
    size=3,
    color='red',
    direction='y',
    nudge_y=.005,
    min.segment.length=.001
  )+
  geom_text_repel( #blue des label
    data=x1[lab=='DES>0'],
    aes(label=lab),
    size=3,
    color='blue',
    direction='y',
    nudge_y=-.005,
    min.segment.length=.001
  )+
  geom_text( #region color labels, not color legend 
    data=x3,
    aes(label=lab,color=nxfac),
    size=3,
    nudge_x=.002,
    nudge_y=.0027,
    hjust=0)
scm <- setNames(gg1(10)[c(1,3,5,7,8)],levels(x2[,nxfac])[rev(c(1,2,4,6,10))])
x <- x4+
  ylim(c(-.02,.035))+xlim(c(-.04,.045))+
  theme(text=element_text(size=7))+
  scale_fill_manual(values=rev(scm))+
  scale_color_manual(values=rev(scm))+
  xlab(bquote(beta[2]))+ylab(bquote(beta[3]))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    legend.position='none',
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain")
  )
print(x)
ggsave('rplot007.tif', x, device = "tiff", dpi =1000, width=3543, height=2400, units='px')

#var 8-------------------------------
nahead=20
x4 <- predict(x161$var0,n.ahead=nahead)
x5 <- data.table(z=x161$z[,1],type='hist')[,ii:=1:.N]
x6 <- data.table(z=x161$z[,2],type='hist')[,ii:=1:.N][]
x7 <- data.table(ii=x5[,max(ii)]+1:nahead,type='forc',z=x4$fcst$X002[,'fcst'])
x8 <- data.table(ii=x6[,max(ii)]+1:nahead,type='forc',z=x4$fcst$X003[,'fcst'])
x <- ggplot(x5,aes(ii,z))+
  geom_line(data=x5,linetype='solid',color='blue')+
  geom_line(data=x7,linetype='dotted',color='blue')+
  geom_point(data=x7,size=.3,color='blue')+
  geom_label(data=x7[ii==max(ii)][,leg:='z2'],aes(label=leg),size=labelsize,color='blue')+
  geom_line(data=x6,linetype='solid',color='red')+
  geom_line(data=x8,linetype='dotted',color='red')+
  geom_point(data=x8,size=.3,color='red')+
  geom_label(data=x8[ii==max(ii)][,leg:='z3'],aes(label=leg),size=labelsize,color='red')+
  ylab('factor (z)')+xlab('time bin')+
  coord_cartesian(xlim=c(0,61),ylim = c(-6.1,6.1))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(size=.4,linetype = "dotted",color='grey80'),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=7,face='plain'),
    axis.line.y.left=element_line(size=.1),
    axis.line.x.bottom=element_line(size=.1),
    axis.text=element_text(size=6,face = "plain"),
    legend.position='none')
print(x)
ggsave('rplot008.tif', x, device = "tiff", dpi =1000, width=3543, height=2400, units='px')

