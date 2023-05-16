#setwd('../')
#source('slib.r')
setwd("c:/users/giles/anest.repo/ijf2305")
getwd() #ok
dir() #ok
source('C:/Users/Giles/anest.repo/anest.lib1/R/aa010util.r')
source('C:/Users/Giles/anest.repo/anest.lib2/R/lib2.r')
source('C:/Users/Giles/anest.repo/anest.step2/R/step2.r')
source('C:/Users/Giles/anest.repo/anest.lfm2/R/aapca.r')
x1 <- fread('a-funlist.R',header=F)[,V1]%>%
  sort(.)
x1%in%ls()%>%
  all(.)%>%
  stopifnot(.)
dump( #ok
  list=x1,
  file='c-cleanlib.r'
  )

