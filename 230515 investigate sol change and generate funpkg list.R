pcax1 <- x133 #from the minimal script LOC
y1 <- 'C:/Users/Giles/AppData/Local/aabb/aappd/rd/31063_2023-05-12_appppdtypex133ver00134.RData'
load(y1)
pcax0 <- x

pcax <- pcax0
x3 <- pcaz(pcax)%>%
    .[,2:3]%>%
    suppressWarnings(rbind(t(as.matrix(rep(0,2))),.)) 
x6 <- data.table(apply(x3^2,1,sum))%>%
    .[,ii:=1:.N]
ggplot(x6,aes(ii,V1))+geom_bar(stat='identity')+
    xlab('timebin')+ylab('var23')

all.equal(pcax0$date,pcax1$date) #yes
all.equal(pcax0$x,pcax1$x) #no
#so now know that two solutions are different: 
#sol1 which gives different pv for same geo
#sol2 cardinal/drc shown here to differ
pcax0$x-pcax1$x

require(anest.step2)
x1 <- fread('a-funlist.R',header=F)[,V1]%>%
  sort(.)
x2 <- as.list(NULL)
for(i in seq_along(x1)) {
x2[[i]] <- data.table(fun=x1[i],lib=gsub('package:','',attributes(pryr::where(x1[i]))[1]))
}
x3 <- rbindlist(x2)[order(lib,fun)]
fwrite(x3,file='d-libpkg.csv',sep=',')
