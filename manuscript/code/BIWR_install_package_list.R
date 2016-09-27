#####################################################################
# Business Intelligence with R
# Dwight Barry
# Package installation list
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_install_package_list.R
#
######################################################################

# From CRAN
install.packages(c("abd", "acs", "arules", "arulesViz", "asympTest", "beanplot", "boot", "bootES", "ca", "choroplethr", "choroplethrMaps", "classInt", "cluster", "clustvarsel", "data.table", "dbscan", "devtools", "DMwR", "dplyr", "DT", "dygraphs", "epitools", "eurostat", "extremeStat", "flexdashboard", "forecast", "gamlss.tr", "gdata", "GGally", "ggExtra", "ggseas", "googleVis", "gridExtra", "htmlTable", "htmlwidgets", "httr", "jsonlite", "kulife", "leaflet", "likert", "lubridate", "maps", "maptools", "mc2d", "mclust", "mgcv", "mvoutlier", "NeatMap", "orddom", "pairsD3", "polycor", "plotly", "pscl", "psych", "qcc", "quantreg", "RColorBrewer", "RCurl", "readxl", "reshape2", "rjags", "rmarkdown", "RODBC", "segmented", "shiny", "simpleboot", "sqldf", "strucchange", "survival", "survminer", "tidyr", "tolerance", "useful", "vcd", "vegan", "VIM", "XLConnect", "XML", "xtable", "zipcode", "zoo"), dependencies = TRUE)

# If you get the Error in install.packages : Updating loaded packages message
# use sessionInfo() to see which packages are loaded or in the namespace
# and remove those packages from the above list, then update.packages for those

# From CRAN, but may come with your RStudio install
# If so, you will get the error mentioned above
# You can also run update.packages instead
install.packages("ggplot2", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)

# From Bioconductor
source("https://bioconductor.org/biocLite.R")
biocLite(c("graph", "Rgraphviz", "GenomicRanges", "BiocInstaller")) 

# From Github
devtools::install_github("vqv/ggbiplot")
devtools::install_github("twitter/AnomalyDetection")
devtools::install_github("hrbrmstr/taucharts")
devtools::install_github("timelyportfolio/parcoords")
devtools::install_github("rasmusab/bayesian_first_aid")
