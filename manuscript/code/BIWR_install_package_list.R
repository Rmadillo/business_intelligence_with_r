#####################################################################
# Business Intelligence with R
# Dwight Barry
# Package installation list
#
# 
#
######################################################################

# From CRAN
install.packages(c("acs", "arules", "arulesViz", "asympTest", "beanplot", "boot", "bootES", "ca", "choroplethr", "choroplethrMaps", "classInt", "cluster", "clustvarsel", "data.table", "dbscan", "devtools", "DMwR", "dplyr", "DT", "dygraphs", "epitools", "extremeStat", "flexdashboard", "forecast", "gamlss.tr", "gdata", "GGally", "ggExtra", "googleVis", "gridExtra", "htmlTable", "htmlwidgets", "httr", "jsonlite", "leaflet", "likert", "lubridate", "maps", "maptools", "mc2d", "mclust", "mgcv", "mvoutlier", "NeatMap", "orddom", "pairsD3", "plotly", "pscl", "psych", "qcc", "quantreg", "RColorBrewer", "readxl", "reshape2", "rjags", "rmarkdown", "RODBC", "segmented", "shiny", "simpleboot", "sqldf", "strucchange", "survival", "survminer", "tidyr", "tolerance", "useful", "vcd", "vegan", "VIM", "XLConnect", "XML", "xtable", "zipcode", "zoo"), dependencies = TRUE)

# From CRAN, but may need to run separately if you don't already have it
install.packages("ggplot2", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)

# From Bioconductor
source("https://bioconductor.org/biocLite.R")
biocLite(c("graph", "Rgraphviz", "GenomicRanges", "BiocInstaller")) 

# From Github
devtools::install_github("vqv/ggbiplot")
devtools::install_github("twitter/AnomalyDetection")
devtools::install_github("hrbrmstr/taucharts")
devtools::install_github("rasmusab/bayesian_first_aid")
