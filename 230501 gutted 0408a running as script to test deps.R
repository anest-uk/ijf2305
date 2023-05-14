# source('dumptest.r')
# source('slib.r')
# root.global <<- "C:\\Users\\Giles\\AppData\\Local\\aabb\\aappd\\"
# setv(app='ppd',ver=132)

#this ok, also // version below
# f230309a(
#       geo=geo,
#       dfn=dfn,
#       applygeo=F,
#       steprip=steprip,
#       steppra=steppra
# )


#f230408aFun <- function(
    nn=c('f230415bd')#,
  getgd(nn)
    parx=T#,
    ncpu=8#,
    #x0=data.table(seq=1:10,rc3=c('TS-','L--','S--','LS-','M--','B--','BS-','AL-','N--','WC-')),
    # sud=coread(
    #   rcx='xxx',
    #   colClasses=list(Date='start'),
    #   step='ver001\\05sud'),
    geo=f230415bd[,.(rc9=rcx,lab,nx,ii)]#,
    dfn=seq.Date(from=as.Date('1995-12-31'),to=as.Date('2022-12-31'),by='y')+c(rep(0,13),59,rep(0,14))#,
    stepsip='ver001\\02sip'#,
    steprip='ver001\\03rip'#,
    steppra='ver010\\07pra-ann'#,
    #nn='pxmocad'
#){
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
#}