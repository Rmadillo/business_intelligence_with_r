
#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 1 Code: Entire project, code only
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_1_Code.R
#
#####################################################################


## Set up

# Creates directory/ies, use recursive=TRUE in the first
# command to create the entire path at once
dir.create("~/BIWR/Chapter1/Code", recursive=TRUE)
dir.create("~/BIWR/Chapter1/Data")
dir.create("~/BIWR/Chapter1/Results")

# Set the working directory
setwd("~/BIWR/Chapter1")

# This package will allow us to interpolate between missing time series values
require(zoo)

# These packages provide functions for easy data wrangling
require(dplyr)
require(reshape2)

# This package provides automated forecasting of time series data
require(forecast)

# This package allows us to create publication-quality plots
require(ggplot2)

# This package allows creation of javascript widgets for use in webpages
require(htmlwidgets)

# This packages uses htmlwidgets to make time series widgets
require(dygraphs)


## Acquire data

# Download the zip file into the data folder
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip", destfile="Data/household_power_consumption.zip")

# Unzip the data from the zip file into the Data folder
unzip("Data/household_power_consumption.zip", exdir="Data")

# Read the data into R 
# NAs are represented by blanks and ? in this data, so need to change
power = read.table("Data/household_power_consumption.txt", sep=";", header=T, 
  na.strings=c("?",""), stringsAsFactors=FALSE)


## Wrangling data

# str gives you the structure of the dataset
str(power)

# Convert date to an ISO date 
power$Date = as.Date(power$Date, format="%d/%m/%Y")

# Create a DateTime object
power$DateTime = as.POSIXct(paste(power$Date, power$Time))

# Obtain the Month and Year for each data point
power$Month = format(power$Date,"%Y-%m")

# Add the first to each Y-m combo and convert back to ISO Date
power$Month = as.Date(paste0(power$Month, "-01"))

# Verify the changes
str(power)

# Get an overview of the variables
summary(power)

# Use ifelse to count each minute that is NA
power$Missing = ifelse(is.na(power$Global_active_power), 1, 0)

# Use dplyr's group_by function to group the data by Date
power_group_day = group_by(power, Date)

# Use dplyr's summarize function to summarize by our NA indicator 
# (where 1 = 1 minute with NA)
power_day_missing = summarize(power_group_day, Count_Missing = sum(Missing))

# Download the 'calendarHeat' function from revolutionanalytics.com
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

# Plot the calendar graph to view the missing data pattern
calendarHeat(power_day_missing$Date, power_day_missing$Count_Missing, 
  varname="Missing Data", color="w2b")

# Use zoo to perform interpolation for missing time series values
power$Global_active_power_locf = na.locf(power$Global_active_power)

# Compare the original and interpolated distributions
# Reshape the two variables into long form for ggplot
power_long = melt(power, id.vars= "DateTime", measure.vars=c("Global_active_power", "Global_active_power_locf"))

# Create density plot
density_plot = ggplot(power_long, aes(value, fill=variable, color=variable)) +
  geom_density(alpha=0.75) +
  facet_wrap(~variable)

# Display plot
density_plot

# Save density plot to Results folder
# Note that ggave uses "dpi" to set image resolution
ggsave("Results/density_plot.png", width=6, height=4, dpi=600, units="in")

# Use dplyr to group by month
power_group = group_by(power, Month)

# Use dplyr to get monthly max demand and total use results
power_monthly = summarize(power_group, 
  Max_Demand_kW = max(Global_active_power_locf), 
  Total_Use_kWh = sum(Global_active_power_locf)/60)

# Remove partial months from data frame
power_monthly = power_monthly[2:47,]

# Convert Month to Date
power_monthly$Month = as.Date(paste0(power_monthly$Month, "-01"))

# Look at structure of the result
str(power_monthly)

## Analytics

### Explore the data

# Create plot of total use by month
total_use_plot = ggplot(power_monthly, aes(Month, Total_Use_kWh)) +
  geom_line(col="blue", lwd=1) 

# Display plot
total_use_plot

# Save plot as hi-res png to Results subfolder
ggsave("Results/total_use_plot.png", width=6, height=4, dpi=600, units="in")

### Run a forecasting model

# Create a time series object of monthly total use
total_use_ts = ts(power_monthly$Total_Use_kWh, start=c(2007,1), frequency=12)

# Automatically obtain the forecast for the next 6 months
# using the forecast package's forecast function
# See ?forecast for more details
total_use_fc = forecast(total_use_ts, h=6)

# View the forecast model results
summary(total_use_fc)

# Export a copy of the model results into a text file in the Results folder
sink("Results/Forecast_Model.txt")
summary(total_use_fc)
sink()

# View the forecast plot
plot(total_use_fc)

# Save the plot to the Results folder
# Note that resolution is called 'res' here
png("Results/total_use_forecast.png", width=6, height=4, res=600, units="in")
plot(total_use_fc)
dev.off() 


## Reporting

### Create an interactive HTML plot

# Create a data frame with the original data
# and placeholders for the forecast details
use_df = data.frame(Total_Use = power_monthly$Total_Use_kWh, 
  Forecast = NA, Upper_80 = NA, 
  Lower_80 = NA, Upper_95 = NA, Lower_95 = NA)

# Create a data frame for the forecast details
# with a placeholder column for the original data
use_fc = data.frame(Total_Use = NA, Forecast = total_use_fc$mean, Upper_80 = 
  total_use_fc$upper[,1], Lower_80 = total_use_fc$lower[,1], 
  Upper_95 = total_use_fc$upper[,2], Lower_95 = total_use_fc$lower[,2])

# Union the two data frames into one using rbind
use_ts_fc = rbind(use_df, use_fc)

# Create a time series of the data and forecast results
total_use_forecast = ts(use_ts_fc, start=c(2007, 1), freq=12)

# Create the widget
energy_use_prediction_widget = dygraph(total_use_forecast, 
  main = "Predicted Monthly Electricty Use (kWh)",
  ylab = "Total kWh", width=900, height=500) %>% 
  dySeries(c("Total_Use"), label = "Actual kWh Usage") %>%
  dyEvent(x = "2008-08-01", "Went on vacation", labelLoc = "top") %>%
  dyRangeSelector(dateWindow = c("2008-09-15", "2011-06-01")) %>%
  dyLegend(width = 800)

# Display the widget in the Viewer window
# Hit the Zoom button for a pop-out 
energy_use_prediction_widget

# Save the widget as a stand-alone html file to Results folder
saveWidget(energy_use_prediction_widget, "energy_use_prediction_widget.html")
