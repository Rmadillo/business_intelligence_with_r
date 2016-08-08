################################################################ 
# Analysis of Canada's 2011 Public Service Employment Survey
# R-"makefile" to run all programs
# Created by: Dwight Barry
# Last edited: 13 Sept 2014
################################################################

# Set the working directory
setwd("~/Projects/PSES2011")

# Load the packages used in this analysis
require(gdata)
require(ggplot2)
require(reshape2)
require(tidyr)
require(dplyr)
require(xtable)
require(rmarkdown)
require(vcd)

# Import raw data from web and clean for analysis,
# and write data to flat files to save future bandwidth
# Uncomment if this is the first time run on the local drive
# source("./data/PSES_ImportData.R")

# Load data for analysis once import is finished,
# then add to a SQLite database to save memory
source("./data/PSES_LoadData.R")

# Obtain summary statistics on answer percentages
# for each survey question
source("./analysis/PSES_SummaryStatistics.R")

# Create heatmaps of answer percentages
# for each survey question group
source("./analysis/PSES_HeatMaps.R")

# Create mosaic plots of Questions 18a and 54
# to help support management planning discussions
# Note: must run PSES_HeatMaps.R before running this script
source("./analysis/PSES_Mosaics.R")

# Create a pdf handout of the complete set of PSES questions
render("./reports/PSES_Questions.Rmd", "pdf_document")

# Create a slide deck for the management planning discussion
render("./reports/PSES_ManagementSlides.Rmd", "ioslides_presentation")

# Provide status of R's session at time of analysis
print(date())
print(sessionInfo())

# Clean up
# Uncomment and run if desired
# rm(list = ls())

#### End of File ####
