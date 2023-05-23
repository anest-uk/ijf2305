x1 <- as.factor(letters[c(1,3,2)])
levels(x1) #a,b,c
x2 <- as.factor(letters[1:3])
levels(x2) #a,b,c

levels(x1) #same order of levels
levels(x2)
as.numeric(x1) #pointers change
as.numeric(x2)
ggplot(data.table(lab=x1,val=1:3),aes(x1,x1,fill=lab))+geom_bar(stat='identity') #same order plot, same ordder legend
ggplot(data.table(lab=x2,val=1:3),aes(x1,x1,fill=lab))+geom_bar(stat='identity')

#so the facto