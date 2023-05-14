putrd <-
function(x, desc = deparse(substitute(x)), i = idxrd() + 1,usedesc=FALSE) {
    if(usedesc && 'desc'%in%names(attributes(x))) { desc <- attr(x,'desc') }
    n <- numtotxt(i) #formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste(c(n, as.character(as.Date(Sys.time())), abbrev(desc,len=35)), collapse = rddelim())
    if (i == 0) {
        save(x, file = paste0(rdroot(), "/rd/", fnam, ".RData"))
    } else {
        i0 <- idxrd()
        save(x, file = paste0(rdroot(), "/rd/", fnam, ".RData"))
        i1 <- idxrd()
        ifelse(i1 == i0 + 1, i1, NA)
    }
}
getrd <-
function(i = idxrd()) {
    n <- formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste0(paste0(dirrd()[n], collapse = rddelim()), ".RData")
    load(file = paste0(rdroot(), "/rd/", fnam))
    x
}
dirrd <-
function() {
    dd <- paste0(rdroot(), "/rd")
    l1 <- lapply(lapply(lapply(dir(dd), strsplit, split = "\\."), "[[", 1), "[", 1)
    num <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 1))
    dat <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 2))
    des <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 3))
    if(is.null(num)) shell(rdroot()) #this is a means to get rdroot() echoed from server
    setkeyv(data.table(data.frame(num = num, dat = dat, des = des)), "num")[]
}
coread <-
function(
    rcx='AL-',#rcx=f230306a(), #
    step='nac',
    colClasses=NULL, #for example colClasses=c(deed_date='character'),nrow=10
    nrows=Inf
) {
  # - [ ] 
  x1 <- dir(step)
  x2 <- grep(grepstring(rcx,caret=T),x1)
  x3 <- x1[x2]
  ext <- tolower(unique(unlist(lapply(strsplit(x3,split='\\.'),`[`,i=2))))
  stopifnot(length(ext)==1) #only one type of extension allowed
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
  # - [ ] cowrite belongs in pkgx
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
f230408aFun <-
function(
    nn=c('f230415bd'),
    parx=T,
    ncpu=8,
    #x0=data.table(seq=1:10,rc3=c('TS-','L--','S--','LS-','M--','B--','BS-','AL-','N--','WC-')),
    # sud=coread(
    #   rcx='xxx',
    #   colClasses=list(Date='start'),
    #   step='ver001\\05sud'),
    geo=f230415bd[,.(rc9=rcx,lab,nx,ii)],
    dfn=seq.Date(from=as.Date('1995-12-31'),to=as.Date('2022-12-31'),by='y')+c(rep(0,13),59,rep(0,14)),
    stepsip='ver001\\02sip',
    steprip='ver001\\03rip',
    steppra='ver010\\07pra-ann'#,
    #nn='pxmocad'
){
  # - [ ] PAA pv from annual augmented {PRA+FIS}(dfn,geo) AKA 'solution 1'
  getgd(nn)
  cocomkd(steppra)
  sfInit(par=parx,cpus=ncpu)
  x1 <- #pra for fis=230311b; called for side-effect
    sfLapply(
      geo[,sort(unique(nx))],
      f230309a,
      geo=geo,
      dfn=dfn,
      applygeo=F,
      steprip=steprip,
      steppra=steppra
      )
  sfStop()
  x2 <- f230311b( #fis
    geo=geo,
    steppra=steppra,
    stepsip=stepsip)
  f230408ad <<- x2[c('geo','ses','bso')]
  putt(f230408ad)
}
f221209a <-
function(
    geo=f221230ad$geo,
    dfn=f230215c(), #yearend series
    fur=f221029bd, #final update return
    applygeo=T
) {
  #browser() # a bit of doubt about the provenance/use of this fn as of 230310 but it is called
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
      x0[,.(idhash.selldate,rc6,rc9,retsa)],
      on=c(rn='idhash.selldate')
      ]%>%
    setnames(.,old='rn',new='idhash.selldate')
  #print('exit f221209a')
  x1
}
f230309a <-
function(
    nxx=1,
    geo,
    dfn,
    steprip=c(steprip='ver001\\03rip'),
    steppra=c(steppra='ver001\\07pra'),
    applygeo=F #changed 0423
) {
  rip <- 
    coread(
      rcx=geo[nx==nxx][,rc9],
      step=steprip,
      colClasses=list(numeric=c('retraw','retsa'),Date=c('buydate','selldate'))
    )
  pra <-
    f221209a(
      geo=geo[nx==nxx], #this could be removed, or use appplygeo=F
      fur=rip,
      dfn=dfn,
      applygeo=applygeo
    )
  print(paste0('pra done, start cowrite to...',steppra))
  cowrite(x=pra,dirnam=steppra,key='rc9',format='csv',newdir=F)
  print('cowrite done.')
}
f230311b <-
function(#parx
    geo=coread(rcx='xxx',
               step=stepgeo,
               colClasses=list(integer='nx'), 
               nrows=Inf),
    stepgeo='ver001\\04geo',
    stepdfn='ver001\\06dfn',
    steppra='ver001\\07pra',
    stepsip='ver001\\02sip',
    dfn=coread(rcx='xxx',step=stepdfn,colClasses=list(Date='dfn'))[,dfn],
    parx=T
) {
  #new FIS: parallel wrapper to f230311a and function for f230311aFun()
  sfInit(par=parx,cpus=min(ncpus(),geo[,length(sort(unique(nx)))]))
  x1 <- sfLapply(
    geo[,sort(unique(nx))],
    f230311a, #parse bso -> ses
    geo=geo,
    #dfn=dfn, #remove 0313
    steppra=steppra,
    stepsip=stepsip)
  sfStop()
  names(x1) <- paste0(max(dfn),'.',zeroprepend(seq_along(x1),3)) #labelling required by 1206a
  x2 <- f221206a(bso=x1,geo=geo)
  x3 <- list(geo=geo,ses=x2,bso=x1)
  x3
}
f221206a <-
function( 
    bso=quintiles, #list
    geoin=f221121bd[,.(date=pvdate,rc6,nx=ceiling(qai*5))], #geo used for bso - but only used if histrank=T, which is deprecated
    histrank=F
) {
  jrc <- names(geoin)[grep('^rc',names(geoin))]%>%setNames(.,.) #now bso has rc6 and rc9 but only one is returned
  x1 <- copy(bso)
  x5 <- as.list(NULL) #stat
  for(i in seq_along(x1)) {
    x5[[i]] <- x1[[i]][['stat']]%>%
      #.[,.(rsq,nsam,sigma,type,nx=as.integer(substr(names(x1)[i],12,13)),update=substr(names(x1)[i],1,10))]
      .[,.(rsq,nsam,sigma,type,nx=as.integer(strsplit(names(x1),split='\\.')[[1]][2]),update=strsplit(names(x1),split='\\.')[[1]][1])]
  }
  x6 <- rbindlist(x5)
  x3 <- #estdt
    x1%>% 
    lapply(.,`[[`,i='estdt')%>%
    rbindlist(.)
  #browser()
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
    # .[,.(nid=.N,pv=sum(pv),m2=sum(m2),ppm2=sum(pv)/sum(m2)),rc6]%>% #aggregate
    # .[order(ppm2),.(nid,pv,m2,ppm2,xrnk=rank(ppm2),qai=cumsum(nid)/sum(nid),rc6)]
  if(histrank) { #deprecated option - no help in practice, also untested after adding rc9 support
    x7 <- #discount ppm2 back, average rank over cycle
      x3[date1>(max(date1)-365*18),.(date=date1,nx,x)]%>%
      .[.[date==max(date),.(px=x,nx)],on=c(nx='nx')]%>%
      .[,.(date,nx,disc=x-px)]%>% #discount(nx,date)
      .[geoin[,.(rc6,nx)],on=c(nx='nx'),allow=T]%>% #discount(rc6,date)
      .[x2[,.(rc6,ppm2,nid)],on=c(rc6='rc6')]%>% #ppm2,nid
      .[,.(date,rc6,lppm2=log(ppm2)+disc,nid)]%>% 
      .[,.(rc6,rnk=rank(lppm2)),date]%>% #rank(lppm2) history 
      .[,.(hrnk=mean(rnk)),rc6]%>% #mean rank(rc6)
      .[x2[,.(rc6,nid)],on=c(rc6='rc6')]%>% #add nid for qai
      .[order(hrnk)]%>% #reorder
      .[,.(rc6,hrnk,qai=cumsum(nid/sum(nid)))]%>% #quantile above
      .[x2[,!c('hrnk','qai')],on=c(rc6='rc6')]%>% #soar with hrnk
      .[,.(pv,m2,nid,ppm2,hrnk,qai,rc6)]
  } else { #approved option
    x7 <- x2%>% #soar with terminal rank
      .[,c('pv','m2','nid','ppm2','xrnk','qai',jrc),with=F]%>%
      setnames(.,old='xrnk',new='rnk')
      #.[,.(pv,m2,nid,ppm2,rnk=xrnk,qai,rc6)] 
  }
  x5 <- list(soar=x7,estdt=x3,stat=x6) #used to return geo, but now return soar which makes clear it's derivation
  x5
}
cocomkd <-
function(type='coco') {
  #shell('rmdir coco')
  shell(paste0("rd /s /q .\\",type),intern=T)
  mkdirn(type)
}
