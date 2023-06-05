accrue2 <-
function(
    pdate=round(seq(from=fur[,min(buydate)],to=fur[,max(selldate)],length.out=10)),
    fur = f221029bd #has Date fields buydate,selldate
  ) {
    x1 <- pdate <- round(sort(unique(pdate)))
    x1[length(x1)] <- fur[,max(selldate)] #always accrue all dates at the end
    x2 <- structure(outer(x1, fur[, selldate], `-`)
                    , class = 'numeric')
    x2[] <- pmax(0, x2[]) #numeric matrix
    x3 <-   structure(outer(x1, fur[, buydate], `-`)
                      , class = 'numeric')
    x3[] <- pmax(0, x3[])
    structure(
      cbind(
        t(x3[1, , drop = F]), 
        t(diff(x3 - x2))
      ), #accrue all days up to pdate[1]
      dimnames=list(
        fur[, idhash.selldate], 
        as.character(pdate[])
      )
    )
  }
cocomkd <-
function(type='coco') {
  shell(paste0("rd /s /q .\\",type),intern=T)
  mkdirn(type)
}
coread <-
function(
    rcx='AL-',#rcx=f230306a(), #
    step='nac',
    colClasses=NULL, #for example colClasses=c(deed_date='character'),nrow=10
    nrows=Inf
) {
  x1 <- dir(step)
  if(length(x1)==0) {stop(paste0('step: ',step,' no files found'))}
  x2 <- grep(grepstring(rcx,caret=T),x1)
  x3 <- x1[x2]
  ext <- tolower(unique(unlist(lapply(strsplit(x3,split='\\.'),`[`,i=2))))
  stopifnot(length(ext)==1) #only one type of extension allowed, csv or rdata
  x4 <- list(NULL)
  i <- 1
  for(i in seq_along(x3)) {
    fp <- paste0(step,'/',x3[i])
    if(ext=='rdata') {
      load(file=fp)
    } else if(ext=='csv') {
      x <- fread(file=fp,colClasses=colClasses,nrows=nrows)
      if(is.data.table(x)&is.null(colClasses)) { #undo autorecognition
        for(i2 in seq_along(x)) {
          x[[i2]] <- as.character(x[[i2]])
        }
      }
    } else {
      stop(paste0('file not found step=',step,'rcx=',paste0(rcx,collapse=',')))
    }
    x4[[i]] <- x
  }
  rbindlist(x4)
}
cowrite <-
function(
    x0=f201203dd,
    dirnam='f201203d',
    newdir=T,
    keyx='rc3', #file per unique value of keyx
    format=c('rdata','csv'),
    csv=F #in addition write all.csv
) {
  format <- match.arg(format)
  if(newdir) {suppressWarnings(cocomkd(dirnam))}
  stopifnot(keyx%in%names(x0))
  x1 <- setkeyv(copy(x0),keyx)
  x2 <- sort(unique(x1[,eval(parse(text=keyx))]))
  for(i in seq_along(x2)) {
    x <- x1[x2[i]]
    if(format=='rdata') {
      save(x,file=paste0(dirnam,'/',x2[i],'.RData'))
    } else {
      fwrite(x,file=paste0(dirnam,'/',x2[i],'.csv'))
    }
  }
  if(csv) {
    fwrite(x0,file=paste0(dirnam,'/all.csv'))
  }
  x4 <- data.table(file.info(paste0(dirnam,'/',dir(paste0(dirnam,'/')))),keep.rownames=T)#[,rc3:=x2]
  x4
}
das <-
function(refda,saleda) {
  x1 <- structure(outer(refda, saleda, `-`), class = 'numeric')
  x1[] <- pmax(0, x1[])
  x2 <- cbind(t(x1[1, , drop = F]),t(diff(x1)))
  setnames(as.data.table(x2),as.character(refda))
}
f221206a <-
function( 
    bso, #list
    geoin=f221121bd[,.(date=pvdate,rc6,nx=ceiling(qai*5))]#, #geo used for bso - but only used if histrank=T, which is deprecated
    #histrank=F
) {
  jrc <- names(geoin)[grep('^rc',names(geoin))]%>%setNames(.,.) #now bso has rc6 and rc9 but only one is returned
  x1 <- copy(bso)
  x5 <- as.list(NULL) #stat
  for(i in seq_along(x1)) {
    x5[[i]] <- x1[[i]][['stat']]%>%
      #.[,.(rsq,nsam,sigma,type,nx=as.integer(substr(names(x1)[i],12,13)),update=substr(names(x1)[i],1,10))]
      .[,.(rsq,nsam,sigma,type,nx=x1[[i]][['nx']],update=strsplit(names(x1),split='\\.')[[1]][1])]
  }
  x6 <- rbindlist(x5)
  x3 <- #estdt
    x1%>% 
    lapply(.,`[[`,i='estdt')%>%
    rbindlist(.)
  x2 <- #soar=solve.reprice.aggregate.rank
    x1%>% #bso
    lapply(.,`[[`,i='pvid')%>%
    rbindlist(.)%>% #combine
    setkey(.,idhash,selldate)%>%
    .[.[,.(idhash=unique(idhash))],mult='last']%>% #last sale per id
    .[,.(nid=.N,pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),jrc]%>% #aggregate
    .[order(ppm2),c('nid','pv','m2','ppm2',jrc),with=F]%>%
    .[,xrnk:=rank(ppm2)]%>%
    .[,qai:=cumsum(nid)/sum(nid)]
  # if(histrank) { #deprecated option - no help in practice, also untested after adding rc9 support
  #   x7 <- #discount ppm2 back, average rank over cycle
  #     x3[date1>(max(date1)-365*18),.(date=date1,nx,x)]%>%
  #     .[.[date==max(date),.(px=x,nx)],on=c(nx='nx')]%>%
  #     .[,.(date,nx,disc=x-px)]%>% #discount(nx,date)
  #     .[geoin[,.(rc6,nx)],on=c(nx='nx'),allow=T]%>% #discount(rc6,date)
  #     .[x2[,.(rc6,ppm2,nid)],on=c(rc6='rc6')]%>% #ppm2,nid
  #     .[,.(date,rc6,lppm2=log(ppm2)+disc,nid)]%>% 
  #     .[,.(rc6,rnk=rank(lppm2)),date]%>% #rank(lppm2) history 
  #     .[,.(hrnk=mean(rnk)),rc6]%>% #mean rank(rc6)
  #     .[x2[,.(rc6,nid)],on=c(rc6='rc6')]%>% #add nid for qai
  #     .[order(hrnk)]%>% #reorder
  #     .[,.(rc6,hrnk,qai=cumsum(nid/sum(nid)))]%>% #quantile above
  #     .[x2[,!c('hrnk','qai')],on=c(rc6='rc6')]%>% #soar with hrnk
  #     .[,.(pv,m2,nid,ppm2,hrnk,qai,rc6)]
  # } else { #approved option
    x7 <- x2%>% #soar with terminal rank
      .[,c('pv','m2','nid','ppm2','xrnk','qai',jrc),with=F]%>%
      setnames(.,old='xrnk',new='rnk')
      #.[,.(pv,m2,nid,ppm2,rnk=xrnk,qai,rc6)] 
  # }
  x5 <- list(soar=x7,estdt=x3,stat=x6) 
  x5
}
f221209a <-
function(
    geo=f221230ad$geo,
    dfn=f230215c(), #yearend series
    fur=f221029bd,
    applygeo=T
) {
  dfn <- sort(unique(dfn))
  if(applygeo) {
    x0 <- fur[geo,on=names(geo)[grep('^rc.',names(geo))]%>%setNames(.,.),allow=T]
  } else {
    x0 <- fur
  }
  x1 <- accrue2( #accrue
    fur = x0,
    pdate = dfn[-1])%>%
    data.table(.,keep.rownames=T)%>%
    .[
      x0[,.(idhash.selldate,rc9,retsa)],
      on=c(rn='idhash.selldate')
      ]%>%
    setnames(.,old='rn',new='idhash.selldate')
  x1
}
f230309a <-
function( 
    nxx=1,
    geo,
    dfn,
    steprip=c(steprip='ver001\\03rip'),
    steppra=c(steppra='ver001\\07pra')
) {
  x0 <- 
    coread(
      rcx=geo[nx==nxx][,rc9],
      step=steprip,
      colClasses=list(numeric=c('retraw','retsa'),Date=c('buydate','selldate'))
    )
  pra <-
    f230508a(
      dfn=dfn, 
      x0=x0
    )
  print(paste0('pra done, start cowrite to...',steppra))
  cowrite(x=pra,dirnam=steppra,key='rc9',format='csv',newdir=F)
  geo[nx==nxx,rc9]%>% 
    `%in%`(.,unique(substr(dir(steppra),1,nchar(geo[1,rc9]))))%>%
    all(.)%>%
    stopifnot(.)
  print('cowrite done.')
}
f230311a <-
function( 
  nxx=1,
  steppra='ver001\\07pra', 
  stepsip='ver001\\02sip',
  d0='1994-12-31',
  geo,
  q1=.4 #outlier fraction
) { 
  stopifnot(
    'nx'%in%names(geo) &
      'rc9'%in%names(geo) &
      length(nxx)==1 &
      nxx%in%geo[,nx]
  )
  if(nxx==9) {print(geo)}
  geox <- geo[nx==nxx]
  x1 <- coread(rcx=geox[,rc9],step=stepsip,colClasses=list(Date='selldate'))[,rc6:=substr(rc9,1,6)][,rc3:=substr(rc9,1,3)]
  x2 <- coread(rcx=geox[,rc9],step=steppra,colClasses=list(numeric='retsa'))%>%
    .[,!grepl('^rc',names(.)),with=F]%>% #more than one
    .[,order(nchar(names(.)),names(.)),with=F]%>%
    .[order(idhash.selldate)]
  x2a <- coread(rcx=geox[,rc9],step='ver001\\03rip',colClasses=list(numeric='retsa'))[x2[,.(idhash.selldate)],on=c(idhash.selldate='idhash.selldate')][,.(idhash.selldate,buydate)]
  stopifnot(all.equal(x2[,idhash.selldate],x2a[,idhash.selldate]))
  tdate <- as.Date(sort(unique(c(d0,names(x2)[grep('[0-9]{4}-',names(x2))]))))
  ddate <- diff(as.integer(tdate))
  x3 <- lm(retsa~.-1,x2[,-'idhash.selldate'])
  x4 <- residuals(x3) #residuals all
  x5 <- summary(x3)   #summary all
  x6 <- x2[!(x4<quantile(x4,q1/2)|x4>quantile(x4,1-(q1/2)))] #select inlier
  x7 <- lm(retsa~.-1,data=x6[,-'idhash.selldate'])  #regression inlier
  x8 <- summary(x7)   #summary inlier
  x9 <- tcrossprod(ddate)*x8$cov.unscaled*x8$sigma^2 #estimator covariance
  x10 <- nrow(x9)
  x11 <- rep(0,x10)
  for(i in 1:x10) {
    x11[i] <-  #cumulative lookback estimation s.e.
      sqrt(sum(x9[i:x10,i:x10]))
  }
  jnam <- #allow for fact that geox may have a misnamed rcn (ie rc6/9) col, so detect the correct join field name
    names(geox)[grep('^rc',names(geox))]%>%
    `names<-`(.,value=paste0('rc',nchar(geox[1,.,with=F])))
  x12 <-             #---estdt---1/4
    data.table(as.matrix(x7$coefficients),keep.rownames=T)%>%
    .[,type:='inlier']%>%
    .[,rn:=gsub('\`','',rn)]%>%
    .[,.(
      date0=tdate[-length(tdate)],
      date1=as.Date(rn),
      xdot.daily=V1,
      type,
      days=ddate)]%>%
    .[,xdot:=xdot.daily*days]%>%
    .[,x:=cumsum(xdot),type]%>%
    .[,xdotse:=x8$coefficients[,2]*days]%>%
    .[,xse:=x11]%>%
    .[,nx:=nxx]%>%
    unique(geo[,.(nx,rc3=lab)])[.,on=c(nx='nx'),allow=T] #untested
  x13 <- x1[geox[,.(rc9)],on=jnam]  #sale
  x14 <-das(  #accrue current holding period 'days after sale'
    refda=tdate[-1],  
    saleda=x13[,selldate])
  x15 <- predict( #estimated post-purchase return
    x7,
    newdata=as.data.table(x14),
    se.fit=T)%>%
    .[c('fit','se.fit')]%>%
    as.data.table(.)%>%
    setnames(.,c('post','se.post'))
  x16 <- data.table( #---fit---2/4
    x2[,.(idhash.selldate)],
    x2a[,.(buydate)],#ordering checked already
    fit=fitted(x3),
    res=residuals(x3))
  x17 <-             #---pvid---3/4
    data.table(cbind(x13[,-c("pxraw","rc3")],x15))%>%
    .[,pv:=round(pxsa*exp(post),-2)]%>%
    .[,se.pv:=round(sqrt(exp((se.post^2))-1)*pv,-2)]%>% #present value
    .[,se.px:=round(sqrt(exp((se.post^2+x8$sigma^2))-1)*pv,-2)]#present price
  x18 <-cbind(       #---stat---3/4 (r-squared for uncentred regression)
    data.table(
      rsq=c(rsq0=1-crossprod(residuals(x3))/crossprod(x2[,retsa]),
            rsq1=1-crossprod(residuals(x7))/crossprod(x6[,retsa])),
      tss=c(crossprod(x2[,retsa]),crossprod(x6[,retsa])),
      nsam=c(nrow(x2),nrow(x6)),
      sigma=c(x5$sigma,x8$sigma),
      type=c('all','inlier')))
  x19 <- list(
    estdt=x12,
    pvid=x17,
    fit=x16,
    stat=x18,
    nx=nxx
  )
  x19
}
f230311b <-
function(
    geo,
    steppra='ver001\\07pra',
    stepsip='ver001\\02sip',
    parx=(3<geo[,length(unique(nx))])
) {
  
  sfInit(par=parx,cpus=min(ncpus(),geo[,length(sort(unique(nx)))]))
  x1 <- sfLapply(
    geo[,sort(unique(nx))],
    f230311a, 
    geo=geo,
    steppra=steppra,
    stepsip=stepsip)
  sfStop()
  t1 <- max(names(coread(step=steppra))%>%.[grep('....-..-..',.)]) #parse out final date
  names(x1) <- paste0(t1,'.',zeroprepend(seq_along(x1),3)) #list label for 1206a
  x2 <- f221206a(bso=x1,geo=geo)
  x2a <- f230531a(x2) #timeseries summary
  x4 <- list(geo=geo,ses=x2,bso=x1,tss=x2a)
  x4
}
f230312a <-
function( 
    nxx=geo[,min(nx)],
    stepdfn='ver001\\06dfn',
    stepgeo='ver001\\04geo',
    dfn=coread(rcx='xxx',
               step=stepdfn,
               colClasses=c(dfn='Date'),
               nrows=Inf)%>%
      .[,dfn],    
    geo=coread(rcx='xxx',
               step=stepgeo,
               colClasses=list(integer='nx'), 
               nrows=Inf),
    steprip='ver001\\03rip',
    outthresh=.4
    ) {
  x1 <- #rip read
    coread(
      rcx=geo[nx==nxx][,rc9],
      step=steprip,
      colClasses=list(numeric=c('retraw','retsa'),Date=c('buydate','selldate'))
    )
  x2 <- #xy construct like pra
    f221209a(
      geo=geo[nx==nxx], 
      fur=x1,
      dfn=dfn,
      applygeo=F
    )%>%
    .[,grep('[0-9]{4}-|retsa',names(.)),with=F]
  x4 <- lm(
    retsa~.-1,
    x2 #all
    )
  x5 <- residuals(x4)
  x6 <- lm(
    retsa~.-1,
    x2[!(x5<quantile(x5,outthresh/2)|x5>quantile(x5,1-(outthresh/2)))] #inlier
    )%>%
    .[['coefficients']]%>%
    data.table(
      xdotd=as.numeric(.),
      date=as.Date(substr(names(.),2,11)))%>% 
    .[,days:=as.numeric(diff(c(min(dfn),date)))]%>%
    .[,xdot:=as.numeric(xdotd*days)]%>%
    .[,x:=cumsum(xdot)]%>%
    .[,.(nx=nxx,rsqraw=summary(x4)$r.squared,date,xdotd,days,xdot,x,lab=geo[nx==nxx][1,lab])]%>%
    .[,ii:=1:.N,lab]%>%
    .[,col:=as.factor(lab)]
  x6
}
f230314a <-
function(
    nxx=3,
    pcax, #pca
    geox=#geo
      coread(
        rcx='xxx',
        step='ver001\\04geo'),
    dfnx=#dfn
      c(as.Date('1994-12-31'),pcax$date),
    kbar=3
) {
  x4 <- f230312a( #solve single nx -> estdt for regressand
    nxx=nxx,
    dfn=dfnx,    
    geo=geox,
    steprip='ver001\\03rip',
    outthresh=.4
  )
  x5 <- pcaz(pcax)[,1:kbar,drop=F] #z regressor
  x6 <- data.table(x=x4[,xdot],x5)
  x7a <- lm(x~.,x6)
  x7b <- summary(x7a)
  x7 <- #beta 'RIB'
    x7a$coefficients[(1+(1:kbar))]
  x8 <- atan2(x7[3],x7[2])%>% #put theta into range (-1/2 to +3/2)pi
    `+`(x=.,y=ifelse(.<(-pi/2),2*pi,0))
  stopifnot(x8>-pi/2&x8<3*pi/2)
  x9 <- as.data.table(as.list(c(x7,x8,nx=nxx,lab=x4[1,lab])))%>%
    setnames(.,c(paste0('b',1:kbar),'theta','nx','lab'))%>%
    .[,.(
      b1=as.numeric(b1),b2=as.numeric(b2),b3=as.numeric(b3),
      theta=round(as.numeric(theta),4),
      nx=as.numeric(nx),
      lab,
      rsq=x7b$r.squared,
      rbarsq=x7b$adj.r.squared
      )
      ]
  x10 <- list(pca=pcax,x=x4,beta=x9)
  stopifnot(x10[['beta']][,all.equal(labxnnn(nx),lab)])
  stopifnot(x10[['x']][,all.equal(labxnnn(nx),lab)])
  x10
}
f230417b <-
function(
    nxx=7,
    nn=c(DES='f230414ad',DTC='f230322ad',DRC='f230312ad'),
    geo=f230322ad$geo,
    des=f230414ad,
    pca=f230312ad$pca,
    nmin=20,
    verbose=F,
    dodrops=T,
    dorib=T #solve for rib
) {
  x1 <- des[geo,on=c(rc6='rc6')]
  x2 <- x1[,.(ppm2n=weighted.mean(ppm2,m2),desn=weighted.mean(des,m2)),.(nx)]
  x3 <- x1[x2,on=c(nx='nx')][,desrel:=des-desn]
  x13 <- NULL
  if(dodrops) {
    x8 <- as.list(NULL)
    for(i in 1:2) { #lo and hi des
      x0=c(-1,1)[i]
      x4 <- x3[nx==nxx][order(desrel*x0)] #desrel-ordered *up or down*
      x6 <- list(x2[nx==nxx][,idrop:=0][,rc6:=NA]) #initial: no drops
      x4drop <- x4[1,rc6]
      for(i1 in 2:x4[,.N-nmin]) {
        x4x <- x4[rc6==x4drop] #the drop
        x4 <- x4[rc6!=x4drop] #drop it
        x6[[i1]] <- #update aggregates
          x4[,.(
            nx=nxx,
            ppm2n=weighted.mean(ppm2,m2),
            desn=weighted.mean(des,m2),
            idrop=i1-1,
            drop=x4drop
          )]
        delta <- x6[[i1]][,ppm2n]-x6[[i1-1]][,ppm2n]#change in ppm2
        stopifnot(sign(delta)==-sign(x4x[1,ppm2]-x6[[i1-1]][,ppm2n])) #check delta corresponds to relative add
        pvstgt <- sign(x6[[i1]][,ppm2n]-x6[[1]][,ppm2n]) # ppm2 is -/+ vs target
        x7 <- x4%>%
          .[(desrel-x6[[i1]][,desn])*x0<0]%>%  #select 1: des(drop) below current aggregate
          .[(ppm2-x6[[i1]][,ppm2n])*pvstgt>0] #select 2: ppm2 is +/- vs target
        if(nrow(x7)<1||x6[[i1]][,0.4<abs(desn)]) break() #stopping criterion
        x4drop <- x7[order(des*x0)][1,rc6] #drop lowest des
        if(verbose) {
          print(paste0(x4drop,' des=',x4[rc6==x4drop,round(des,2)],' desn=',x6[[length(x6)]][,round(desn,2)]))
        }
      }
      x8[[i]] <- rbindlist(x6[-1])[,dir:=x0][,idd:=dir*idrop]
    }
    x9 <- rbindlist(x8)[order(idd)]
    x10a <- setkey(x3[nx==nxx],rc6)[!x9[dir>0,drop]] #hi des
    x10b <- setkey(x3[nx==nxx],rc6)[!x9[dir<0,drop]] #low des
    if(dorib) {
      x12a <- f230314a(  #solve+rib
        nx=1,
        pcax=pca, #pca
        geox=x10a[,.(rc9=rc6,nx=1,qai=qai,lab=paste0('x',zeroprepend(1,3)))] #hi des
      )
      x12b <- f230314a( 
        nx=2,
        pcax=pca, #pca
        geox=x10b[,.(rc9=rc6,nx=2,qai=qai,lab=paste0('x',zeroprepend(2,3)))] #lo des
      )
      x13 <-rbind(
        x12a$beta%>%
          .[,des:=x10a[,weighted.mean(des,m2)]]%>%
          .[,ppm2:=x10a[,weighted.mean(ppm2,m2)]]
        ,
        x12b$beta%>%
          .[,des:=x10b[,weighted.mean(des,m2)]]%>%
          .[,ppm2:=x10b[,weighted.mean(ppm2,m2)]]
      )%>%
        .[,nx:=nxx]%>%
        .[]
    }
    x14 <- rbind(x10a[,dir:='hi'],x10b[,dir:='lo'])
  } else { #here no drops, estimate entire bin
    x10 <- x3[nx==nxx]
    if(dorib) {
      x13 <- f230314a(  #solve+rib
        nx=0,
        pcax=pca, #pca
        geox=x10[,.(rc9=rc6,nx=0,qai=qai,lab=paste0('x',zeroprepend(0,3)))]
      )$beta%>%
        .[,des:=x10[,weighted.mean(des,m2)]]%>%
        .[,ppm2:=x10[,weighted.mean(ppm2,m2)]]%>%
        .[,nx:=nxx]%>%
        .[]
    }
    x14 <- copy(x10)[,dir:='all']
  }
  x15 <- list(geo=x14,beta=x13)
  x15
}
f230417c <-
function(
    nxx=7,
    nn=c(DES='f230414ad',DTC='f230322ad',DRC='f230312ad'),
    geo=f230322ad$geo,
    des=f230414ad,
    pca=f230312ad$pca,
    nmin=50,
    verbose=F
) {
  halfway <- geo[nx==nxx,round(.N/2)]
  x1 <- f230417b(nxx=nxx,nn=nn,geo=geo,des=des,pca=pca,nmin=Inf,verbose=verbose,dodrops=F,dorib=F)
  x2 <- f230417b(nxx=nxx,nn=nn,geo=geo,des=des,pca=pca,nmin=halfway,verbose=verbose,dorib=F)
  x3 <- f230417b(nxx=nxx,nn=nn,geo=geo,des=des,pca=pca,nmin=nmin,verbose=verbose,dorib=F)
  # x4 <- rbind(
  #   x1[['beta']][,nmin:=Inf][,type:='all'],
  #   x2[['beta']][,nmin:=halfway][,type:='half'],
  #   x3[['beta']][,nmin:=nmin][,type:='full']
  # )
  x5 <- rbind(
    x1[['geo']][,nmin:=Inf][,type:='all'],
    x2[['geo']][,nmin:=halfway][,type:='half'],
    x3[['geo']][,nmin:=nmin][,type:='full']
  )
  #x6 <- list(geo=x5)#,beta=x4)
  x5
}
f230424a <-
function(
    d1='2016-03-31',
    x12=f230312ad$pca,
    rib=x142,
    nn=c('f230312ad','x142')
) { #attribution
  #getgd(nn)
  i1 <- which(x12$date>as.Date(d1))
  d0 <- x12$date[i1[1]]
  d1 <- x12$date[i1[-1]] #5 periods
  beta <- rib$beta[,.(b1,b2,b3)]
  years <- (Sys.Date()-d0)/365.25
  i2 <- i1[-1]
  x13 <- as.list(NULL)
  x13[[1]] <- as.matrix(x12$x[i2,-1])%*% #m
    pcah(x12)[,1,drop=F]%*%
    t(beta[,1,drop=F])
  x13[[2]] <- as.matrix(x12$x[i2,-1])%*%pcah(x12)[,2:3,drop=F]%*%t(beta[,2:3,drop=F]) #c
  x13[[4]] <- as.matrix(x12$x[i2,-1]) #t
  x13[[3]] <- x13[[4]]-(x13[[1]]+x13[[2]])
  #x13[[3]] <- x03-(x01+x02) #r
  x14 <- lapply(x13,function(x){apply(x,FUN=sum,MAR=2)})%>%
    lapply(.,round,digits=4)%>%
    as.data.table(.)%>%
    setnames(.,c('m','c','r','t'))
  x14
}
f230506a <-
function(
  nxx=3,
  estdtx=x16$ses$estdt,
  pcax=x17,
  kbar=3
) {
  x1 <- estdtx[nx==nxx][,lab:=rc3]
  x2 <- pcaz(pcax)[,1:kbar,drop=F]
  x3 <- data.table(x=x1[,xdot],x2)
  x4 <- lm(x~.,x3)
  x5 <- summary(x4)
  x6 <- x4$coefficients[(1+(1:kbar))]
  x7 <- atan2(x6[3],x6[2])%>% #put into range (-1/2 to +3/2)pi
    `+`(x=.,y=ifelse(.<(-pi/2),2*pi,0))
  lapply(as.data.table(as.list(x6)),class)
  x8 <- as.data.table(as.list(c(as.list(x6),x7,nx=nxx,lab=x1[1,rc3])))%>%
    setnames(.,c(paste0('b',1:kbar),'theta','nx','lab'))%>%
    .[,r:=sqrt(b2^2+b3^2)]%>%
    .[,rsq:=x5$r.squared]%>%
    .[,rbarsq:=x5$adj.r.squared]%>%
    .[,a:=x5$coefficients[1]]%>%
    .[,at:=x5$coefficients[1,3]]
  x9 <- list(pca=pcax,x=x1,beta=x8)
  stopifnot(x9[['beta']][,all.equal(labxnnn(nx),lab)])
  stopifnot(x9[['x']][,all.equal(labxnnn(nx),lab)])
  x9
}
f230506b <-
function(  
  nxx=estdtx[,sort(unique(nx))],
  estdtx=x16$ses$estdt,
  pcax=x17,
  kbar=3
) {
  x2 <- x1 <- as.list(NULL)
  i <- 1
  for(i in seq_along(nxx)) {
    x3 <- f230506a(nxx=nxx[i],estdtx=estdtx,pcax=pcax,kbar=kbar)
    x1[[i]] <- x3$x
    x2[[i]] <- x3$beta
  }
  x4 <- list(
  estdt=rbindlist(x1),
  pca=pcax,
  beta=rbindlist(x2)[,dt:=c(NA,diff(theta))]#diff may not be meaningful
  )
  x4
}
f230506c <-
function(    
    sol=x140,
    pva=x121,
    fis=sol$fis,
    rib=sol$rib,
    theta=sol$rib$beta, 
    pcax=sol$rib$pca,
    t0=as.Date('1994-12-31'),
    kat=list(m=1,c=2:3)
  ) {
    ll <- as.list(NULL)
    x1 <- copy(fis$ses$estdt)%>%
      .[,date:=date1]%>%
      .[nx%in%round(seq(from=1,to=max(nx),length.out=4))]%>%
      .[,ii:=1:.N,nx]%>%
      .[,col:=as.factor(rc3)]%>%
      .[,lab:=rc3]
    ll[[1]] <- #x(bin)
      ggplot(x1,aes(ii,x,color=col))+
      theme(legend.position="none")+
      geom_line(size=.2)+
      geom_point(size=.2)+
      xlab('timebin')+ylab('x=cum. log return')
    x3 <- pcaz(pcax)%>%
      .[,2:3]%>%
      suppressWarnings(rbind(t(as.matrix(rep(0,2))),.)) #warning re colnames
    x4a <- cumsum(x3)
    x4 <- sweep(x4a,MAR=2,STAT=(apply(x4a,2,min)+apply(apply(x4a,2,range),2,diff)/2),FUN=`-`)
    x5 <- x4%>%
      data.table(.,keep.rownames=T)%>%
      setnames(.,c('date','z2','z3'))%>%
      melt(.,id.vars='date')%>%
      .[,col:=as.factor(variable)]%>%
      .[,ii:=1:.N,variable]
    ll[[2]] <- ggplot(x5,aes(ii,value,color=col))+
      theme(legend.position="none")+
      geom_line(size=.3)+
      geom_point(size=.2)+
      xlab('timebin')+ylab('z')
    x6 <- data.table(apply(x3^2,1,sum))%>%
      .[,ii:=1:.N]
    ll[[3]] <- ggplot(x6,aes(ii,V1))+geom_bar(stat='identity')+
      xlab('timebin')+ylab('var23')
    ll[[4]] <- ggplot(theta,aes(nx,dt))+geom_bar(stat='identity')+
      scale_x_reverse(breaks=1:10)
    x1 <- theta
    rad <- x1[,ceiling(max((b2^2+b3^2)^.5)/.01)*.01]
    x2 <- x1[,.(b1,b2,b3,theta,nx,lab,col=as.factor(lab),dt,xc=rad*cos(theta),yc=rad*sin(theta),rr=round(sqrt(b2^2+b3^2)/2,3))]
    x3 <- data.table(x=rad*cos(2*pi*(1:100)/100),y=rad*sin(2*pi*(1:100)/100))
    x4 <- ggplot(x2)+
      geom_point(aes(xc,yc))+
      geom_point(aes(b2,b3))+
      geom_path(data=x3,aes(x,y),color='gray70')+
      geom_text(aes(b2,b3,label=nx,hjust=0,vjust=0))+
      geom_text(aes(xc,yc,label=round(theta,2),hjust=0,vjust=0))+
      geom_text(data=x2,aes(b2/2,b3/2,label=rr,angle=(theta+floor((theta+pi/2)/pi)*pi)*180/pi),size=3)+
      ylab(bquote(beta[3]))+
      xlab(bquote(beta[2]))
    for(i in 1:10){
      x4 <- x4+geom_line(data=(rbind(x2[i,.(b2,b3)],x2[i,.(b2,b3)]*0)),aes(b2,b3),linetype=3)
    }
    ll[[5]] <- x4
    x0 <- fis$ses$estdt[,.(lab=as.factor(rc3),date=date1,xdot)]
    
    # x7a <- drc$geo[sol1$ses$soar[,.(m2=sum(m2),pv=sum(pv),ppm2=round(sum(pv)/sum(m2))),.(rc6=substr(rc9,1,6))],on=c(rc9='rc6')]%>%
    x7a <- fis$geo[pva,on=c(rc9='rcx')]%>%
      .[,.(minppm2=min(ppm2),maxppm2=max(ppm2)),nx]%>%
      .[order(nx)]
    x7 <- x7a%>%
      .[,.(range=paste0(minppm2,'-',maxppm2)),nx]%>%
      .[,paste0(paste0(range[1:4],collapse=';'),'\n',paste0(range[5:8],collapse=';'),'\n',paste0(range[8:.N],collapse=';'))]%>%
      paste0(.,' no-overlap=',x7a[,all(maxppm2[-.N]<=minppm2[-1])])
    annotx <- paste0(
      '\n',
      ' bins=',x0[,sum(sort(unique(date))<='2007-12-31')],",",
      x0[,sum(sort(unique(date))>'2009-02-28')],"\n",
      ' mindays=',x0[,sort(unique(date))]%>%.[which(.<='2007-12-31')]%>%diff(.)%>%min(.)%>%as.integer(.),",",
      x0[,sort(unique(date))]%>%.[which(.>='2009-02-28')]%>%diff(.)%>%min(.)%>%as.integer(.),"\n",
      ' deltathetarange=',theta[,round(diff(range(dt,na.rm=T)),4)],"\n",
      ' rc6bins=',fis$geo[,.N,nx][,paste0(N,collapse=',')],"\n",
      ' drc=',paste0(pcax$date[1:5],collapse=','),'...',"\n",
      'ppm2=',x7,'\n',
      paste0(' run at: ',format(Sys.time(),'%y%m%d %H:%M'))
    )
    ll[[6]] <- 
      ggplot() + 
      xlim(0,100)+
      ylim(0,100)+
      annotate("text", x =5, y = 90 ,size=3, label = annotx,hjust=0,vjust=1) + 
      theme_void()
    multiplot(plotlist=ll,cols=3)
  }
f230508a <-
function(
    dfn=f230215c(), #yearend series
    x0=f221029bd #final update return
) {
  dfn <- sort(unique(dfn))
  x1 <- accrue2(
    fur = x0,
    pdate = dfn[-1])%>%
    data.table(.,keep.rownames=T)%>%
    .[
      x0[,.(idhash.selldate,rc9,retsa)],
      on=c(rn='idhash.selldate')
      ]%>%
    setnames(.,old='rn',new='idhash.selldate')
  x1
}
f230516a <-
function(
    soar=x121[nchar(rcx)==6][,rc6:=rcx],  #pva 230602a 
    geo=geo1,
    xso=eennx,
    maxrad=50,
    maxpeer=50
) {
  x1 <- #distance rank
    xso[geo[,.(rc6)],.(rc6,eex,nnx,one=1),on=c(rc6='rc6')]%>%
    .[.,mult='all',on=c(one='one'),allow=T]%>%
    .[,.(rc6,other=i.rc6,r=round(sqrt((eex-i.eex)^2+(nnx-i.nnx)^2)/1000,2))]%>%
    .[r<maxrad]%>%
    .[order(rc6,r)]%>%
    .[,data.table(.SD,dr=1:.N),rc6]
  x0 <- soar[nchar(rcx)==6]%>%
    .[,rc6:=rcx]
  ll <- list(NULL)
  for(i in 3:maxpeer) {
    x2 <- x1[dr<=i]
    ll[[i]] <- 
      x0%>%
      .[x1[dr<=i],on=c(rc6='other'),allow=T,nomatch=NULL]%>%
      .[order(i.rc6,r),.(rc6=i.rc6,other=rc6,r,nid,ppm2)]%>%
      .[,.(other,lrnk=rank(ppm2),nid,ppm2),.(rc6)]%>%
      .[order(lrnk),.(rc6,other,arnk=(lrnk-1)/(i-1),nid,ppm2,peers=i)]%>%
      .[rc6==other]
  }
  x3 <- #orthogonalise vs national rank
    rbindlist(ll)%>%
    .[,.(arnk=mean(arnk),nid=nid[1]),.(rc6)]%>%
    .[x0[,.(rc6,nrnk=(rank(ppm2)-1)/(.N-1),nid,pv,m2,ppm2)],on=c(rc6='rc6'),nomatch=NULL]%>%
    .[,.(rc6,nid,pv,m2,ppm2=round(ppm2),des=residuals(lm(arnk~nrnk,.)))]%>%
    .[order(des)]
  x3
}
f230516b <-
function(
    geo=geo1[,.(rc6=rcx,qai,nx)],
    des=x150, #des 230516a
    pca=x133, #pca pcaest
    nxx=geo[,unique(nx)]#,#nx to split, in reality 2:9 
  #  parx=F
  ) { 
    x1 <- lapply(
      as.list(nxx), 
      f230417c,
      geo=geo,
      des=des,
      pca=pca
    )
    x2 <- rbindlist(x1)
    x2
  }
f230531a <-
function(
    ses=x103$ses #fis 230311b
){
  x2 <- #join parts of 311b for stats
    ses$estdt%>%
    .[ses$stat[type=='all',.(nx,rsq,nsam)],on=c(nx='nx'),allow=T,nomatch=NULL]
  x3 <- #'beta1' 
    x2[,.(date=date1,lab=rc3,xdot)]%>%
    dcast(.,date~lab,value.var='xdot')%>%
    pcaest(.,rotate=T)%>%#rotate for 'rewarded' beta
    pcajscale(beta=T)%>% #capm style
    pcab(.)%>%
    .[,1,drop=F]%>%
    data.table(.,keep.rownames=T)%>%
    setnames(.,c('rc3','beta'))
  x4 <- #tseries properties
    x2[,.(
      mean=mean(xdot),
      sigma=sd(xdot),
      rho=acf(xdot,lag.max=1,plot=F)$acf[2],
      rsqraw=rsq[1],
      dd=min(xdot)
    ),nx]%>% #join fundamentals
    as.data.table(.)%>%
    .[unique(x2[,.(nx,lab=rc3)]),on=c(nx='nx')]%>%
    .[x3,on=c(lab='rc3')]%>%
    .[order(nx),.(rc3=lab,mean,sigma,rho,dd,beta,rsqraw)]%>%
    .[,.(rcx=rc3,mean,sigma,rho,dd,beta,rsqraw)]
  x4
}
f230601a <-
function(
    fis=x141, #fis 230311b that generated rib
    rib=x142  #rib 230506b
){
  fis$tss[rib$beta,on=c(rcx='lab')]%>%
    .[,.(
      nx,
      lab=rcx,
      rsq=rsqraw,
      mu=mean,
      sigma,
      rho,
      dd,
      b1,
      b2,
      b3,
      thetab=theta,
      dthetab=c(NA,diff(theta)),r=sqrt(b2^2+b3^2)
      )]
}
f230601b <-
function(
    fis=x141, #fis 230311b that generated rib
    rib=x142, #rib 230506b
    pva=x121, #pva 230602a 
    x1=f230601a(fis=fis,rib=rib)
){
  geo <- fis$geo #solution 3 DTC/DRC
  ses <- fis$ses 
  tss <- fis$tss
  estdt <- ses$stat[type=='all']%>%
    .[ses$estdt,on=c(nx='nx')]
  x2 <- pva[geo,on=c(rcx='rc9')]%>%
    .[,.(ppm2=sum(pv)/sum(m2),ppm2min=min(ppm2),ppm2max=max(ppm2),nid=sum(nid)),.(nx,lab)]%>%
    .[,nidfrac:=round(nid/sum(nid),4)]
  x3 <- x2[x1,on=c(nx='nx')]
  x4 <- 
    x3%>%
    .[order(nx),dthetab:=c(NA,diff(thetab))]%>%
    .[order(-nx),
      .(nx,
        ppm2min,
        ppm2max,
        nid.k=round(nid/1000),
        nid.f=round(nid/sum(nid),4),
        rsq,
        b1,
        b2,
        b3,
        thetab=thetab,
        dthetab=dthetab
      )]
  x4
}
f230602a <-
function(
    fis=x103 #fis 230311b  'solution 1' 
) {
  x1 <- fis$ses$soar%>% #soar: SOlve+reprice/Aggregate/Rank
  .[,.(
    nid,
    m2,
    pv,
    ppm2,
    rcx=rc9 #---------------------rc9=sector
  )]%>%
  rbind(.,
        .[,.(
          nid=sum(nid),
          m2=sum(m2),
          pv=sum(pv),
          ppm2=sum(pv)/sum(m2)
        ),
        .(
          rcx=substr(rcx,1,6) #-rc6=district
        )])%>%
  rbind(.,
        .[,.(
          nid=sum(nid),
          m2=sum(m2),
          pv=sum(pv),
          ppm2=sum(pv)/sum(m2)
        ),
        .(
          rcx=substr(rcx,1,3) #--rc3=area
        )]
  )%>%
  .[]
  x1
}
f230603a <-
function(
    nx=1,
    t0=as.Date('1994-12-31'),
    fis=x141a,
    kat=list(m=1,c=2:3),
    rib=x142
) {
   # browser()
  x0 <- fis$bso[[nx]][['fit']]
  x1 <- pcaz(rib$pca)[,unlist(kat)]
  x2 <- #zdot daily rate
    x1%>%
    sweep(.,STAT=as.numeric(diff(c(t0,index(x1)))),MAR=1,FUN=`/`)%>%
    data.table(.)%>%
    setnames(.,paste0('z',1:3))%>%
    .[,.(date=index(x1),days=as.numeric(diff(c(t0,index(x1)))),z1,z2,z3)]
  x3 <- #assigned daily
    data.table(daily=seq.Date(from=t0,to=max(index(x1)),by='d'))%>%
    x2[.,on=c(date='daily'),roll=-Inf]%>%
    .[,.(date,days,z1=cumsum(z1),z2=cumsum(z2),z3=cumsum(z3))]
  x4 <- #product with beta
    x3[,-c('date','days')]%>%
    sweep(.,STAT=as.matrix(rib$beta[,unlist(kat),with=F])[nx,,drop=T],MAR=2,FUN=`*`)%>%
    data.table(.)%>%
    .[,.(m=z1,c=z2+z3)]%>%
    cbind(x3[,.(date)],.)
  x5 <- #join buy, sell, and collate
    x4[,.(date,mbuy=m,cbuy=c)][x0,on=c(date='buydate')]%>%
    .[,selldate:=as.Date(substr(idhash.selldate,18,30))]%>%
    .[x4[,.(date,msell=m,csell=c)],on=c(selldate='date'),nomatch=NULL]%>%
    .[,.(
      idhash.selldate,
      buydate=date,
      fit2.m=msell-mbuy,
      fit2.c=csell-cbuy,
      fit2.r=fit-((msell-mbuy)+(csell-cbuy)),
      fit2.t=fit,
      res1=res,
      tot=fit+res
    )]
  x5
}
f230603b <-
function(
    fis=x141a,
    rib=x142,
    t0=as.Date('1994-12-31'),
    kat=list(m=1,c=2:3)
) {
    x1 <- as.list(seq_along(fis$bso))
    x2 <- lapply(x1,f230603a,fis=fis,kat=kat,rib=rib)
    x3 <- rbindlist(x2)
    x5 <- x3[,.(fit2.m,fit2.c,fit2.r,fit2.t,res1,tot)]
    x6 <- rbind(
      as.data.table(lapply(x5,mean)),
      as.data.table(lapply(x5,sd)),
      as.data.table(lapply(x5^2,sum)),
      as.data.table(lapply(x5,mean))
    )%>%
      cbind(data.table(sum=c('mean','sd','sse','mse')),.)
  x8 <- x9 <- as.list(NULL)
  x7 <- lm(fit2.t~fit2.m+fit2.c,x5)
  x8[[1]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=1]
  x9[[1]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=1]
  x7 <- lm(fit2.t~fit2.m,x5)
  x8[[2]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=2]
  x9[[2]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=2]
  x7 <- lm(tot~fit2.m+fit2.c,x5)
  x8[[3]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=3]
  x9[[3]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=3]
  x7 <- lm(tot~fit2.m,x5)
  x8[[4]] <- broom::glance(x7)%>%data.table(.)%>%.[,lm:=4]
  x9[[4]] <- broom::tidy(x7)%>%data.table(.)%>%.[,lm:=4]
  x10 <- list(
    summary=x6,
    glance=rbindlist(x8),
    tidy=rbindlist(x9)
  )
  x10
}
f230605a <-
function(
    geo=x131[,.(rc9,nx,lab=labxnnn(nx))],
    steppra='ver002\\07pra',
    stepsip='ver001\\02sip',
    pcax=x133
){
  x1 <- #FIS
    f230311b(#//parallel
      geo=geo,
      steppra=steppra,#ver002=drc
      stepsip=stepsip
    )#//
  x2 <- #RIB
    f230506b( 
      estdt=x1$ses$estdt, #regressand x
      pca=pcax #regressor z
    )
  x3 <- #ATT
  f230603b(
    fis=x1,
    rib=x2
  )
  x4 <- list(
    fis=x1[names(x1)!='bso'],#bso is big
    rib=x2,
    att=x3
    )
  x4
}
grepstring <-
function(x=regpcode(metro()),dollar=F,caret=T) {
  if(caret) x <- paste0('^',x)
  if(dollar) x <- paste0(x,'$')
  paste(x,collapse='|')
}
irregpcode <-
function(x) {
  x1 <- substr(x,1,pmin(6,nchar(x)))
  x2 <- substr(x,pmin(7,nchar(x)),nchar(x))
  gsub(patt=' $',rep='',x=paste(gsub(patt='\\-',rep='',x=x1),gsub(patt='\\-',rep='',x=x2)))
}
labxnnn <-
function(n,len=3) {
  paste0('x',zeroprepend(n,len))
}
metro <-
function() {c('N','E','SE','SW','NW','BN','CM','OX','RG','NG','AL','LU','RO','GU','KT','CR','HA')}
mkdirn <-
function(dd) {
    if (all(is.na(file.info(dd))))
        suppressWarnings(shell(paste0("mkdir ", dd)))
}
multiplot <-
function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
ncpus <-
function(
  nsplit=3 #number of separate entire tasks
)
{
shell(
  "wmic cpu get NumberOfCores,NumberOfLogicalProcessors",
  intern=TRUE
)%>%
  strsplit(.," ")%>%
  `[[`(.,i=2)%>%
  as.numeric(.)%>%
  min(.,na.rm=T)%>%
  min(.,8)
}
parsepcode <-
function(pc=c('AL1 1AD','AL1 1BD','AL1 1CD')) {
    x <- lapply(pc,ppc)%>%
      lapply(.,data.table)%>%
      lapply(.,t)%>%
      Reduce(rbind,.)%>%
      data.frame(.)%>%
      lapply(.,unlist)%>%
      suppressWarnings(.)
    x <- lapply(x,`names<-`,NULL)
    names(x) <- names(ppc(pc[1]))
    x
  }
pcab <-
function( # - [ ] beta : only when g=1 are these covariance-related, otherwise 'sensitivity' to a scaled factor
    xest
  ) {
    nbar <- ncol(xest$x[, -'date'])
    kbar <- ncol(xest$eig$vectors)
    if (xest$par$iscale == 'cov') {
      sigma <- rep(1, nbar)
    } else if (xest$par$iscale == 'cor') {
      sigma <- xest$sigma
    }
    x3 <- diag(sigma) %*%
      xest$eig$vectors %*%
      diag(sqrt(xest$eig$values)) %*%
      tanrot(xest$tantheta,jbar=kbar) %*%
      diag(xest$g[1:kbar])
    rownames(x3) <- names(xest$x[, -'date'])
    colnames(x3) <- zeroprepend(1:ncol(x3),3)
    x3
  }
pcadrc <-
function(
    pca,
    date0=as.Date('1994-12-31'),
    kbar=3
) {
  dates <- c(date0,pca$date)
  x1 <- NA
  for(i in 1:nrow(pca$x)) {
    x1[i] <- 
      data.table(
        y=unlist(t(pca$x[i,-1])),
        x=pcab(pca)[,1:kbar]
      )%>%
      setnames(.,c('y',paste0('b',1:kbar)))%>%
      lm(y~.,.)%>%
      summary(.)%>%
      `[[`(.,j='adj.r.squared')
  }
  x2 <- data.table(
    days=as.numeric(diff(dates)),       #days
    rbarsq=x1,                          #rbarsq
    r=apply(pcaz(pca)[,2:kbar]^2,1,sum),#k=2,3 variance
    start=dates[-length(dates)],        #period start
    end=dates[-1]                       #period end
  )
  x2 
}
pcaest <-
function(# - [ ]  eigen, polarity
    x = data.table(date, retmat),
    iscale = c('cov', 'cor'),
    method = c( 'ML','unbiased'),
    signmethod=c('ordered','reference'),
    rcref='WC-3', #reference series name
    refpol=c(1,-1,-1), #polarity associated
    rollsum=1, #apply rollsum - not used
    verbose=T,
    center=T,
    pcawin=x[,range(date)],
    rotwin=c(x[,sort(date)[2]],pcawin[2]), #default to eliminate first period (SNR motive)
    rotate=T,
    krot=2:3,
    doplot=F
  ){
    iscale <- match.arg(iscale)
    method <- match.arg(method)
    signmethod <- match.arg(signmethod)
    pcawin <- sort(round(as.Date(pcawin)))
    stopifnot(is.Date(pcawin)&all(pcawin%in%x[,date])&length(pcawin)==2)
    rotwin <- sort(round(as.Date(rotwin)))
    stopifnot(is.Date(rotwin)&all(rotwin%in%x[,date])&length(rotwin)==2)
    ipcawin <- setNames(match(pcawin,x[,date]),pcawin)
    irotwin <- setNames(match(rotwin,x[,date]),rotwin)
    nbar <- ncol(x) - 1
    x0 <- rollapply(x[,-'date'],width=rollsum,FUN=sum,partial=T,align='right')
    if(rollsum!=1&verbose) {print(paste0('rollsum=',rollsum,' in pcaest'))}
    x1 <- cov.wt(x0[ipcawin[1]:ipcawin[2],],
                 method = method,
                 center = center,
                 cor = T)
    x2 <- x1[[iscale]]
    x3 <- eigen(x = x2)
    dimnames(x3$vectors) <- dimnames(x1$cov) 
    thresh <- sqrt(.Machine$double.eps)
    if(any(x3$values<thresh)) {
      kbar <- max(which(x3$values>thresh))
      x3$values <- x3$values[1:kbar]
      x3$vectors <- x3$vectors[,1:kbar,drop=F]
    } else {
      kbar <- ncol(x2)
    }
    if(signmethod=='ordered') {
      print('ordered method')
      signfinder <-
        function(evec, pola = c(1, -1, -1)[1:min(3,length(evec))]) {
          #sign by regression on 1st 3 even zero phase cos(x.centred); pola is tgt for last
          n <- length(evec) - 1
          x <-
            data.table(
              y = evec,
              f0 = rep(1, n + 1),
              f1 = -cos(pi * (0:n) / n),
              f3 = cos(-2 * pi * (0:n) / n)
            )
          x1 <- summary(lm(y ~ . - 1, x))$coefficients
          if(any(is.na(x1[,2]))){
            x2 <- sign(sum(x1[, 1] * pola))
          } else {
            x2 <- sign(sum(x1[, 3] * pola))#sum of t-stats (scale-invariant)
          }
          x2
        }
      x4 <- unlist(lapply(data.table(x3$vectors), signfinder))
      if(any(is.na(x4))) {x4 <- rep(1,length(x4))}
    } else {
      stopifnot(rcref%in%rownames(x3$vectors))
      iref <- match(rcref,rownames(x3$vectors))
      jref <- seq_along(refpol)
      x4 <- c(refpol*sign(x3$vectors[iref,jref]),rep(1,ncol(x3$vectors)-length(jref)))
    }
    x3$vectors <- sweep(x3$vectors,
                        STAT = x4,
                        MAR = 2,
                        FUN = `/`)
    x4 <- list(
      x = x,
      xrs = x0, #rs 'rollsum applied'
      date = x[, date],
      sigma=sqrt(diag(x1$cov)),
      xx = x2,
      eig = x3,
      tantheta = rep(0, nbar),
      g = rep(1, nbar),
      par = list(
        method = method,
        iscale = iscale,
        xsect = '',
        rollsum=rollsum,
        kbar=kbar,
        ipcawin=ipcawin,
        irotwin=irotwin
      )
    )
    if(rotate) {
      x4 <- pcarot0(x4)
    }
    if(doplot) {plot(cumsum(pcaz(x4))[,1:3],scr=1,col=1:3)}
    x4
  }
pcah <-
function( # - [ ] h holdings (unit variance factor portfolios)
    xest
  ) {
    nbar <- ncol(xest$x[, -'date'])
    kbar <- ncol(xest$eig$vectors)
    if (xest$par$iscale == 'cov') {
      sigmainv <- rep(1, nbar)
    } else if (xest$par$iscale == 'cor') {
      sigmainv <- 1 /xest$sigma
    }
    x3 <- diag(sigmainv) %*%
      xest$eig$vectors %*%
      diag(1 / sqrt(xest$eig$values)) %*%
      tanrot(xest$tantheta,jbar=kbar) %*%
      diag(1 / xest$g[1:kbar])
    rownames(x3) <- names(xest$x[, -'date'])
    colnames(x3) <- zeroprepend(1:ncol(x3),3)
    x3
  }
pcajscale <-
function( # - [ ] column scalar
    xest,
    jscale = c('var', 'gross', 'long', 'short'),
    beta1=F #modified default to FALSE
  ) {
    jscale <- match.arg(jscale)
    x1 <- copy(xest)
    x1$g <- rep(1, ncol(xest$x) - 1) #unscaled (=varscaled)
    x2 <- pcah(x1)
    if (jscale ==         'gross') {
      x1$g <- apply(abs(x2), 2, sum)*.5
    } else if (jscale ==  'long')  {
      x1$g <- apply(x2 * (x2 > 0), 2, sum)
    } else if (jscale ==  'short') {
      x1$g <- apply(abs(x2) * (x2 < 0), 2, sum)
    }
    x1$par$jscale <- jscale
    if(beta1) { #mean(beta)=1 
      print(paste0('mean pcab=',mean(pcab(x1)[,1])))
      x1$g[1] <- x1$g[1]/mean(pcab(x1)[,1])
      x1$par$beta1 <- T
    } else {
      x1$par$beta1 <- F
    }
    x1
  }
pcaobj <-
function( # - [ ] objective function for rotation
    tantheta = 0,
    xest = y2a,
    j = 2,
    years=20
  ) {
    stopifnot(1 < j) #see definition of rotation
    dstart <- Sys.Date()-years*365.25
    xest$tantheta[j] <- tantheta
    diff(range(cumsum(pcaz(xest)[dstart<=xest$date, j])))
  }
pcarot0 <-
function( # - [ ] class='pcaest' | min-range-rotator taking pcaest as input
    x1, #pcaest object
    krot=2:3,
    irotwin=x1$par$irotwin[1]:x1$par$irotwin[2]
  ) {
    krot <- setdiff(krot,1)
    krot <- krot[krot<=ncol(x1$x[,-'date'])]
    f1=function(rr=0,jj=2,xm,irotwin) { #apply rr with jj
      x1 <- copy(xm)
      x1$tantheta[jj] <- rr
      xx <- diff(range(cumsum(pcaz(xest=x1)[irotwin])[,jj,drop=F]))
      xx
    }
    kbar <- x1$par$kbar 
    initialgrid <- ((-20:20)/21)+.01
    kset <- sort(unique(pmin(krot,kbar)))
    for(i in seq_along(kset)) {
      k <- kset[i]
      start <- initialgrid[which.min(sapply(initialgrid,f1,xm=x1,jj=k,irotwin=irotwin))]
      x1$tantheta[k] <- nlm(f=f1,p=start,j=k,xm=x1,irotwin=irotwin)$estimate
    }
    x1 #pcaest object with updated [['tantheta']]
  }
pcaz <-
function( # - [ ]  factor timeseries
    xest = pcaestd,
    x = xest$x,
    h = pcah(xest)
  ) {
    zoo(as.matrix(x[, -'date']) %*% h, x[, date])
  }
ppc <-
function(pc='EC2R 8AH') {
  if(nchar(pc)<2) return(list(area=ifelse(grepl('[A-Z,a-z]',pc),paste0(toupper(pc),'--'),''),district='',sector='',unit=''))
  chkpcode(pc)
  pc <- toupper(pc)
  gg <- gregexpr(patt=' ',pc)
  x <- strsplit(pc,split=' ')
  out <- unlist(lapply(x,'[[',1))
  nout <- nchar(out)
  inum <- as.numeric(regexpr("[0-9]",out))
  area <- pc
  sector <- unit <- district <- rep('',length(pc))
  area[inum==2] <- substr(out[inum==2],1,1)
  area[inum==3] <- substr(out[inum==3],1,2)
  district[inum==2] <- substring(out[inum==2],2)
  district[inum==3] <- substring(out[inum==3],3)
  if(any(lapply(x,length)>1)) { #inbound code exists
    stopifnot(all(lapply(x,length)==2)) #exists for all
    inb <- unlist(lapply(x,'[[',2))
    nin <- nchar(inb)
    sector <- substr(inb,1,1)
    unit <- substring(inb,2,nin)
  }
  list(area=area,district=district,sector=sector,unit=unit)
}
pxmoreg <-
function(
) {
  x <-
    structure(list(area = c("GY", "JE", "AL", "CB", "CM", "CO", "HP",
                            "IP", "LU", "NR", "PE", "SG", "SS", "DE", "DN", "LE", "LN", "NG",
                            "S", "BR", "CR", "DA", "EN", "HA", "IG", "KT", "RM", "SM", "TW",
                            "UB", "WD", "IM", "E", "EC", "N", "NW", "SE", "SW", "W", "WC",
                            "BF", "BX", "GIR", "QC", "XX", "DH", "DL", "HG", "HU", "LS",
                            "NE", "SR", "TS", "WF", "YO", "BT", "BB", "BD", "BL", "CA", "CH",
                            "CW", "FY", "HD", "HX", "L", "LA", "M", "OL", "PR", "SK", "WA",
                            "WN", "AB", "DD", "DG", "EH", "FK", "G", "HS", "IV", "KA", "KW",
                            "KY", "ML", "PA", "PH", "TD", "ZE", "BN", "CT", "GU", "ME", "MK",
                            "OX", "PO", "RG", "RH", "SL", "SO", "TN", "BA", "BH", "BS", "DT",
                            "EX", "GL", "PL", "SN", "SP", "TA", "TQ", "TR", "CF", "LD", "LL",
                            "NP", "SA", "SY", "B", "CV", "DY", "HR", "NN", "ST", "TF", "WR",
                            "WS", "WV"), 
                   name = c("Guernsey", "Jersey", "St. Albans", "Cambridge",
                            "Chelmsford", "Colchester", "Hemel", "Ipswich", "Luton", "Norwich",
                            "Peterborough", "Stevenage", "Southend", "Derby", "Doncaster",
                            "Leicester", "Lincoln", "Nottingham", "Sheffield", "Bromley",
                            "Croydon", "Dartford", "Enfield", "Harrow", "Ilford", "Kingston",
                            "Romford", "Sutton", "Twickenham", "Southall", "Watford", "Isle of Man",
                            "London", "London", "London", "London", "London", "London", "London",
                            "London", "British Forces", "Non-geographic", "Girobank HQ, Bootle",
                            "Awarding Bodies", "Amazon.com returns", "Durham", "Darlington",
                            "Harrogate", "Hull", "Leeds", "Newcastle", "Sunderland", "Cleveland",
                            "Wakefield", "York", "Belfast", "Blackburn", "Bradford", "Bolton",
                            "Carlisle", "Chester", "Crewe", "Blackpool", "Huddersfield",
                            "Halifax", "Liverpool", "Lancaster", "Manchester", "Oldham",
                            "Preston", "Stockport", "Warrington", "Wigan", "Aberdeen", "Dundee",
                            "Dumfries", "Edinburgh", "Falkirk", "Glasgow", "Comhairle nan Eilean Siar",
                            "Inverness", "Kilmarnock", "Kirkwall", "Kirkaldy", "Motherwell",
                            "Paisley", "Perth", "Galashiels", "Shetland", "Brighton", "Canterbury",
                            "Guildford", "Medway", "Milton Keynes", "Oxford", "Portsmouth",
                            "Reading", "Redhill", "Slough", "Southampton", "Tonbridge", "Bath",
                            "Bournemouth", "Bristol", "Dorchester", "Exeter", "Gloucester",
                            "Plymouth", "Swindon", "Salisbury", "Taunton", "Torquay", "Truro",
                            "Cardiff", "Llandrindod", "Llandudno", "Newport", "Swansea",
                            "Shrewsbury", "Birmingham", "Coventry", "Dudley", "Hereford",
                            "Northampton", "Stoke on Trent", "Telford", "Worcester", "Walsall",
                            "Wolverhampton"), 
                   region = c("Channel Islands", "Channel Islands",
                              "southeast", "east", "east", "east", "southeast", "east", "southeast", "east",
                              "east", "east", "east", "East Midlands", "East Midlands", "East Midlands",
                              "East Midlands", "East Midlands", "East Midlands", "greater london",
                              "greater london", "greater london", "greater london", "greater london",
                              "greater london", "greater london", "greater london", "greater london",
                              "greater london", "greater london", "greater london", "Isle of Man",
                              "london", "london", "london", "london", "london", "london", "london",
                              "london", "Non-geographic", "Non-geographic", "Non-geographic",
                              "Non-geographic", "Non-geographic", "northeast", "northeast",
                              "northeast", "northeast", "northeast", "northeast", "northeast",
                              "northeast", "northeast", "northeast", "Northern Ireland", "northwest",
                              "northwest", "northwest", "northwest", "northwest", "northwest",
                              "northwest", "northwest", "northwest", "northwest", "northwest",
                              "northwest", "northwest", "northwest", "northwest", "northwest",
                              "northwest", "Scotland", "Scotland", "Scotland", "Scotland",
                              "Scotland", "Scotland", "Scotland", "Scotland", "Scotland", "Scotland",
                              "Scotland", "Scotland", "Scotland", "Scotland", "Scotland", "Scotland",
                              "southeast", "southeast", "southeast", "southeast", "southeast",
                              "southeast", "southeast", "southeast", "southeast", "southeast",
                              "southeast", "southeast", "southwest", "southwest", "southwest",
                              "southwest", "southwest", "southwest", "southwest", "southwest",
                              "southwest", "southwest", "southwest", "southwest", "Wales",
                              "Wales", "Wales", "Wales", "Wales", "Wales", "West Midlands",
                              "West Midlands", "West Midlands", "West Midlands", "West Midlands",
                              "West Midlands", "West Midlands", "West Midlands", "West Midlands",
                              "West Midlands"), 
                   regionx = c("channel.islands", "channel.islands",
                               "southeast", "east", "east", "east", "southeast", "east", "southeast", "east",
                               "east", "east", "east", "east.midlands", "east.midlands", "east.midlands",
                               "east.midlands", "east.midlands", "east.midlands", "greater.london",
                               "greater.london", "greater.london", "greater.london", "greater.london",
                               "greater.london", "greater.london", "greater.london", "greater.london",
                               "greater.london", "greater.london", "greater.london", "isle.of.man",
                               "london", "london", "london", "london", "london", "london", "london",
                               "london", "non-geographic", "non-geographic", "non-geographic",
                               "non-geographic", "non-geographic", "northeast", "northeast",
                               "northeast", "northeast", "northeast", "northeast", "northeast",
                               "northeast", "northeast", "northeast", "northern.ireland", "northwest",
                               "northwest", "northwest", "northwest", "northwest", "northwest",
                               "northwest", "northwest", "northwest", "northwest", "northwest",
                               "northwest", "northwest", "northwest", "northwest", "northwest",
                               "northwest", "scotland", "scotland", "scotland", "scotland",
                               "scotland", "scotland", "scotland", "scotland", "scotland", "scotland",
                               "scotland", "scotland", "scotland", "scotland", "scotland", "scotland",
                               "southeast", "southeast", "southeast", "southeast", "southeast",
                               "southeast", "southeast", "southeast", "southeast", "southeast",
                               "southeast", "southeast", "southwest", "southwest", "southwest",
                               "southwest", "southwest", "southwest", "southwest", "southwest",
                               "southwest", "southwest", "southwest", "southwest", "wales",
                               "wales", "wales", "wales", "wales", "wales", "west.midlands",
                               "west.midlands", "west.midlands", "west.midlands", "west.midlands",
                               "west.midlands", "west.midlands", "west.midlands", "west.midlands",
                               "west.midlands"), 
                   rc = c("GY-", "JE-", "AL-", "CB-", "CM-", "CO-",
                          "HP-", "IP-", "LU-", "NR-", "PE-", "SG-", "SS-", "DE-", "DN-",
                          "LE-", "LN-", "NG-", "S--", "BR-", "CR-", "DA-", "EN-", "HA-",
                          "IG-", "KT-", "RM-", "SM-", "TW-", "UB-", "WD-", "IM-", "E--",
                          "EC-", "N--", "NW-", "SE-", "SW-", "W--", "WC-", "BF-", "BX-",
                          "GIR", "QC-", "XX-", "DH-", "DL-", "HG-", "HU-", "LS-", "NE-",
                          "SR-", "TS-", "WF-", "YO-", "BT-", "BB-", "BD-", "BL-", "CA-",
                          "CH-", "CW-", "FY-", "HD-", "HX-", "L--", "LA-", "M--", "OL-",
                          "PR-", "SK-", "WA-", "WN-", "AB-", "DD-", "DG-", "EH-", "FK-",
                          "G--", "HS-", "IV-", "KA-", "KW-", "KY-", "ML-", "PA-", "PH-",
                          "TD-", "ZE-", "BN-", "CT-", "GU-", "ME-", "MK-", "OX-", "PO-",
                          "RG-", "RH-", "SL-", "SO-", "TN-", "BA-", "BH-", "BS-", "DT-",
                          "EX-", "GL-", "PL-", "SN-", "SP-", "TA-", "TQ-", "TR-", "CF-",
                          "LD-", "LL-", "NP-", "SA-", "SY-", "B--", "CV-", "DY-", "HR-",
                          "NN-", "ST-", "TF-", "WR-", "WS-", "WV-")), 
              .Names = c("area",
                         "name", "region", "regionx", "rc"), row.names = c(NA, -129L), class = "data.frame")
  data.table(x)
}
regpcode <-
function(rawcode=c('AL1 1AD','AL1 1BD','AL1 1CD'),x=parsepcode(rawcode)) {
    rawcode <- gsub(patt='  ',rep=' ',rawcode)
    Reduce(paste0,lapply(x,pad1))
  }
rmifgl <-
function(
  x #character=names of non-function objects in .GlobalEnv
  ) {
  for(i in seq_along(x)) {
    if(
      exists(x[i],envir=globalenv())
      &&
      mode(get(x[i],envir=globalenv()))!='function'
    ) {
      rm(list=x[i],envir=globalenv())
      }
  }
}
rr3 <-
function( # - [ ]  rotate 2 out of jbar in the (x1,xjrot) plane
    tantheta = 1,
    jrot = 3,
    jbar = 3
  ) {
    x1 <- diag(jbar)
    if (1 < jrot) {
      x1[matrix(c(c(1, jrot, 1, jrot),
                  c(1, 1, jrot, jrot)), 4, 2)] <- rrr2(tantheta)
    }
    x1
  }
rrr2 <-
function( # - [ ]  #rotate 2D from a real number
    tantheta = .1
  ) {
    th <- atan(tantheta)
    matrix(c(cos(th), -sin(th), sin(th), cos(th)), 2, 2)
  }
tanrot <-
function( # - [ ]  anglevector -> rotation
    tantheta = rep(0, 3),
    jbar = 5
  ) {
    tantheta[1] <- 0 #all rotations are perp. to plane {1,j}
    x1 <- list(diag(jbar))
    for (j in which(tantheta!=0)) {
      x1[[j]] <- rr3(tantheta = tantheta[j],
                     jrot = j,
                     jbar = jbar)
    }
    Reduce(`%*%`, x1)
  }
zeroprepend <-
function(x,ntotal) {
  x <- as.character(x)
  stopifnot(all(nchar(x)<=ntotal)) #otherwise x is right-truncated
  z <- paste(rep("0",ntotal),collapse="")
  zz <- rep(z,length(x))
  substr(zz,1+nchar(zz)-nchar(x), nchar(zz)) <- x
  zz
}
