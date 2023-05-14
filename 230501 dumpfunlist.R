#setwd('../')
source('slib.r')
setwd("c:/users/giles/anest.repo/ijf2305")
getwd() #ok
dir() #ok
x1 <- fread('230501a funlist.R',header=F)[,V1]
#%>% .[!grepl('^#',.)]
dump( #ok
  list=x1,
  file='230501 dumptest.r'
  )

