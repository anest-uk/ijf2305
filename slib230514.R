x1 <- c( 
'data.table',
'devtools',
'fasttime',
'snowfall',
'zoo',
'ggplot2',
'magrittr',
'plotly',
'quadprog',
'signal',
'snow',
'snowfall',
'urca',
'vars',
'colorspace',
'ggrepel')
x2 <- sort(unique(x1))
x3 <- suppressPackageStartupMessages(sapply(x2,library,character.only=T))
