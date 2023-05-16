x1 <- c( 
'colorspace',
'data.table',
'devtools',   
'fasttime',   
'ggplot2',    
'ggrepel',    
'lubridate',
'magrittr',   
'plotly',     
'quadprog',   
'scales',
'signal',     
'snow',      
'snowfall',   
'urca',       
'vars',       
'zoo'       
)
x2 <- sort(unique(x1))
x3 <- suppressPackageStartupMessages(sapply(x2,library,character.only=T))
