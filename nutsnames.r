nname <- #NUTS names
structure(list(
  X1 = c("L", "K", "J", "I", "H", "G", "F", "E", 
"D", "C"), 
X2 = c("Wales", "South West", "South East", "London", 
"East of England", "West Midlands", "East Midlands", "Yorkshire and Humber", 
"North West", "North East"),
nx=c(4,7,9,10,8,5,6,3,2,1)
), class = "data.frame", 
row.names = c(NA, 
-10L))%>%
  data.table(.)%>%setnames(.,c('code','name','nx'))%>%
  .[order(-nx)]#%>%.[c(4,3,2,6,10)] #add select 230522

