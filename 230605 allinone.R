# x45 <- f230601b(
#   fis=x141,
#   rib=x142,
#   pva=x121)
#-------------------------------------------------------------solution 3 DTC/DRC
# x141a #---FIS---
# x142 #---RIB---
x1 <- f230601b(
  fis=x141,
  rib=x142,
  pva=x121)
x142$beta[,.(rbarsq)]
x142$beta[,mean(rbarsq)] #.948
x142$beta[-1,mean(rbarsq)] #.948
#------------------------------------------------------------solution 7 NUTS.DRC
x2 <- f230601b(
  fis=x174a,
  rib=x175,
  pva=x121)
x175$beta[,.(rbarsq)]
x175$beta[,mean(rbarsq)] #.968



# f230605a <- function(
#     geo=x131[,.(rc9,nx,lab=labxnnn(nx))],
#     steppra='ver002\\07pra',
#     stepsip='ver001\\02sip',
#     pcax=x133
# ){
#   x1 <- #FIS
#     f230311b(#//parallel
#       geo=geo,
#       steppra=steppra,#ver002=drc
#       stepsip=stepsip
#     )#//
#   x2 <- #RIB
#     f230506b( 
#       estdt=x1$ses$estdt, #regressand x
#       pca=pcax #regressor z
#     )
#   x3 <- #ATT
#   f230603b(
#     fis=x1,
#     rib=x2
#   )
#   x4 <- list(
#     fis=x1[names(x1)!='bso'],#bso is big
#     rib=x2,
#     att=x3
#     )
#   x4
# }


# f230603b <- #summary of attribution
#   function(
#     fis=x141a,
#     rib=x142,
#     t0=as.Date('1994-12-31'),
#     kat=list(m=1,c=2:3)
# ) {
#     x1 <- as.list(seq_along(fis$bso))
#     x2 <- lapply(x1,f230603a,fis=fis,kat=kat,rib=rib)
#     x3 <- rbindlist(x2)
#     x5 <- x3[,.(fit2.m,fit2.c,fit2.r,fit2.t,res1,tot)]
#     x6 <- rbind(
#       as.data.table(lapply(x5,mean)),
#       as.data.table(lapply(x5,sd)),
#       as.data.table(lapply(x5^2,sum)),
#       as.data.table(lapply(x5,mean))
#     )%>%
#       cbind(data.table(sum=c('mean','sd','sse','mse')),.)
#   x8 <- x9 <- as.list(NULL)
#   x7 <- lm(fit2.t~fit2.m+fit2.c,x5)
#   x8[[1]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=1]
#   x9[[1]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=1]
#   x7 <- lm(fit2.t~fit2.m,x5)
#   x8[[2]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=2]
#   x9[[2]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=2]
#   x7 <- lm(tot~fit2.m+fit2.c,x5)
#   x8[[3]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=3]
#   x9[[3]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=3]
#   x7 <- lm(tot~fit2.m,x5)
#   x8[[4]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=4]
#   x9[[4]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=4]
#   x10 <- list(
#     summary=x6,
#     glance=rbindlist(x8),
#     tidy=rbindlist(x9)
#   )
#   x10
# }



# f230506c <- #display panel 
#   function(    
#     sol=x140,
#     pva=x121,
#     fis=sol$fis,
#     rib=sol$rib,
#     theta=sol$rib$beta, 
#     pcax=sol$rib$pca,
#     t0=as.Date('1994-12-31'),
#     kat=list(m=1,c=2:3)
#   ) {
#     ll <- as.list(NULL)
#     x1 <- copy(fis$ses$estdt)%>%
#       .[,date:=date1]%>%
#       .[nx%in%round(seq(from=1,to=max(nx),length.out=4))]%>%
#       .[,ii:=1:.N,nx]%>%
#       .[,col:=as.factor(rc3)]%>%
#       .[,lab:=rc3]
#     ll[[1]] <- #x(bin)
#       ggplot(x1,aes(ii,x,color=col))+
#       theme(legend.position="none")+
#       geom_line(size=.2)+
#       geom_point(size=.2)+
#       xlab('timebin')+ylab('x=cum. log return')
#     x3 <- pcaz(pcax)%>%
#       .[,2:3]%>%
#       suppressWarnings(rbind(t(as.matrix(rep(0,2))),.)) #warning re colnames
#     x4a <- cumsum(x3)
#     x4 <- sweep(x4a,MAR=2,STAT=(apply(x4a,2,min)+apply(apply(x4a,2,range),2,diff)/2),FUN=`-`)
#     x5 <- x4%>%
#       data.table(.,keep.rownames=T)%>%
#       setnames(.,c('date','z2','z3'))%>%
#       melt(.,id.vars='date')%>%
#       .[,col:=as.factor(variable)]%>%
#       .[,ii:=1:.N,variable]
#     ll[[2]] <- ggplot(x5,aes(ii,value,color=col))+
#       theme(legend.position="none")+
#       geom_line(size=.3)+
#       geom_point(size=.2)+
#       xlab('timebin')+ylab('z')
#     x6 <- data.table(apply(x3^2,1,sum))%>%
#       .[,ii:=1:.N]
#     ll[[3]] <- ggplot(x6,aes(ii,V1))+geom_bar(stat='identity')+
#       xlab('timebin')+ylab('var23')
#     ll[[4]] <- ggplot(theta,aes(nx,dt))+geom_bar(stat='identity')+
#       scale_x_reverse(breaks=1:10)
#     x1 <- theta
#     rad <- x1[,ceiling(max((b2^2+b3^2)^.5)/.01)*.01]
#     x2 <- x1[,.(b1,b2,b3,theta,nx,lab,col=as.factor(lab),dt,xc=rad*cos(theta),yc=rad*sin(theta),rr=round(sqrt(b2^2+b3^2)/2,3))]
#     x3 <- data.table(x=rad*cos(2*pi*(1:100)/100),y=rad*sin(2*pi*(1:100)/100))
#     x4 <- ggplot(x2)+
#       geom_point(aes(xc,yc))+
#       geom_point(aes(b2,b3))+
#       geom_path(data=x3,aes(x,y),color='gray70')+
#       geom_text(aes(b2,b3,label=nx,hjust=0,vjust=0))+
#       geom_text(aes(xc,yc,label=round(theta,2),hjust=0,vjust=0))+
#       geom_text(data=x2,aes(b2/2,b3/2,label=rr,angle=(theta+floor((theta+pi/2)/pi)*pi)*180/pi),size=3)+
#       ylab(bquote(beta[3]))+
#       xlab(bquote(beta[2]))
#     for(i in 1:10){
#       x4 <- x4+geom_line(data=(rbind(x2[i,.(b2,b3)],x2[i,.(b2,b3)]*0)),aes(b2,b3),linetype=3)
#     }
#     ll[[5]] <- x4
#     x0 <- fis$ses$estdt[,.(lab=as.factor(rc3),date=date1,xdot)]
#     
#     # x7a <- drc$geo[sol1$ses$soar[,.(m2=sum(m2),pv=sum(pv),ppm2=round(sum(pv)/sum(m2))),.(rc6=substr(rc9,1,6))],on=c(rc9='rc6')]%>%
#     x7a <- fis$geo[pva,on=c(rc9='rcx')]%>%
#       .[,.(minppm2=min(ppm2),maxppm2=max(ppm2)),nx]%>%
#       .[order(nx)]
#     x7 <- x7a%>%
#       .[,.(range=paste0(minppm2,'-',maxppm2)),nx]%>%
#       .[,paste0(paste0(range[1:4],collapse=';'),'\n',paste0(range[5:8],collapse=';'),'\n',paste0(range[8:.N],collapse=';'))]%>%
#       paste0(.,' no-overlap=',x7a[,all(maxppm2[-.N]<=minppm2[-1])])
#     annotx <- paste0(
#       '\n',
#       ' bins=',x0[,sum(sort(unique(date))<='2007-12-31')],",",
#       x0[,sum(sort(unique(date))>'2009-02-28')],"\n",
#       ' mindays=',x0[,sort(unique(date))]%>%.[which(.<='2007-12-31')]%>%diff(.)%>%min(.)%>%as.integer(.),",",
#       x0[,sort(unique(date))]%>%.[which(.>='2009-02-28')]%>%diff(.)%>%min(.)%>%as.integer(.),"\n",
#       ' deltathetarange=',theta[,round(diff(range(dt,na.rm=T)),4)],"\n",
#       ' rc6bins=',fis$geo[,.N,nx][,paste0(N,collapse=',')],"\n",
#       ' drc=',paste0(pcax$date[1:5],collapse=','),'...',"\n",
#       'ppm2=',x7,'\n',
#       paste0(' run at: ',format(Sys.time(),'%y%m%d %H:%M'))
#     )
#     ll[[6]] <- 
#       ggplot() + 
#       xlim(0,100)+
#       ylim(0,100)+
#       annotate("text", x =5, y = 90 ,size=3, label = annotx,hjust=0,vjust=1) + 
#       theme_void()
#     multiplot(plotlist=ll,cols=3)
#   }
# 
