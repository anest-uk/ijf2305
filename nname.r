nname <-
structure(list(X1 = c("L", "K", "J", "I", "H", "G", "F", "E", 
"D", "C"), X2 = c("Wales", "South West", "South East", "London", 
"East of England", "West Midlands", "East Midlands", "Yorkshire and The Humber", 
"North West", "North East")), class = "data.frame", row.names = c(NA, 
-10L))%>%data.table(.)%>%setnames(.,c('code','name'))
