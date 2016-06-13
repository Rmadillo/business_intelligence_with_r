#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 6 Code: Trends and Time
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_6_Code.R
#
######################################################################

## Describing trends in non-temporal data
  
require(ggplot2)
require(quantreg)

### Smoothed trends

ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="loess") +
  theme_bw()

require(mgcv)

ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="gam", formula=y~s(x)) +
  theme_bw()


### Quantile trends

ggplot(bike_share_daily, aes(x=temp, y=casual)) +
  xlab("Daily Mean Normalized Temperature") +
  ylab("Number of Casual Bike Uses") +
  geom_point(col="gray50") +
  stat_quantile(aes(color = ..quantile..), quantiles = c(0.05, 0.1, 0.25,
  0.5, 0.75, 0.9, .95)) +
  scale_color_gradient2(midpoint=0.5, low="steelblue", mid="brown", 
  high="steelblue ") +
  theme_bw()

### Simple linear trends

ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="lm") +
  theme_bw()


### Segmented linear trends

require(segmented)

bike_segment = segmented(lm(cnt~atemp, data=bike_share_daily), ~atemp, psi=0.6)

bike_segment$psi

psi = bike_segment$psi[2]

ggplot(bike_share_daily, aes(x=atemp, y=cnt, group = atemp > psi)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="lm") +
  theme_bw()


## The many flavors of regression

# lm()
# GLMs
# glm()
# glm.nb()
# Proportional odds
# polr()
# Multinomial regression 
# multinom()
# Zero-inflated Poisson regression 
# require (pscl)
# zeroinfl() 
# Zero-truncated Poisson regression 
# require (gamlss.tr)
# gamlss() 
# GAM 
# require(mgcv)
# gam() 


## Plotting regression results

require(dwplot)

# Create a model object
bad_model = lm(cnt ~ atemp * hum * windspeed, data=bike_share_daily)

# Plot coefficients and CIs
dwplot(bad_model)


## Working with temporal data

# http://cran.r-project.org/web/views/TimeSeries.html).

# as.Date is usually best for dates 
# as.POSIXct is usually best for times

require(lubridate)

india_natl_holidays = c("Jan 26, 2014", "Apr 13, 2014", "Aug 15, 2014", "Oct 02, 2014", "Dec 25, 2014")

india_natl_holidays = as.Date(india_natl_holidays, format="%b %d, %Y")

str(india_natl_holidays)

Date[1:5], format: "2014-01-26" "2014-04-13" "2014-08-15" "2014-10-02" "2014-12-25"

birthdays = c("20000101", "19991201", "19760704", "20140314")

ymd(birthdays)

age = ymd(today()) - ymd(birthdays)

round(age/eyears(1), 1) # eyears is a lubridate function, not a typo!

age = ymd(20141201) - ymd(birthdays)

round(age/eyears(1), 0)

?strptime 


## Calculate a mean or correlation in circular time (clock time)

require(psych)

wake_time = c(7.0, 7.5, 6.5, 6.25, 7.0, 7.25, 7.25)

sleep_time = c(23.5, 0.5, 23.75, 0.25, 23.25, 23.5, 0.75)

wake_sleep = data.frame(wake_time, sleep_time)

circadian.mean(wake_sleep$sleep_time)

circadian.cor(wake_sleep)


## Plotting time-series data

library(eurostat)

demo_fmonth = get_eurostat('demo_fmonth')

demo_fmonth$date = as.Date(paste(substr(demo_fmonth$time, 1, 4), substr(demo_fmonth$month, 2, 3), "01", sep="-"))
    
library(dplyr)

UK_births = filter(demo_fmonth, geo == "UK" & month != 'TOTAL' & month != 'UNK' & date >= '2003-01-01' & date <= '2012-12-01')

UK_births = arrange(UK_births, date)

UK_births_ts = ts(UK_births$values, start=c(2003, 1), frequency=12)

plot(UK_births_ts, xlab="Date", ylab="Count", main="Monthly Births in the UK (2003-2012)", col="darkblue")

require(ggplot2)

ggplot(UK_births, aes(x=Date, y=Value)) +
  geom_line(col="darkblue") +
  ylab("Count") +
  theme_minimal()


## Detecting autocorrelation

acf(UK_births_ts)


## Plotting monthly patterns

monthplot(UK_births_ts, main="Monthly Patterns in UK Births (2003-2012)", xlab="Month", ylab="Count", col="darkblue", lwd=2, lty.base=2, lwd.base=1, col.base="gray40")

require(forecast)

seasonplot(UK_births_ts, main="Seasonal Trends in UK Births (2003-2012)", col=rainbow(10), year.labels=TRUE, year.labels.left=TRUE, cex=0.7, cex.axis=0.8)


## Decomposing time series into components

### Additive decomposition

plot(decompose(UK_births_ts), col="darkblue")


### Decomposing when the variance changes

data(AirPassengers)

plot(AirPassengers)

require(forecast)

AirPassengers_lambda = BoxCox.lambda(AirPassengers)

plot(BoxCox(AirPassengers, AirPassengers_lambda))

plot(decompose(BoxCox(AirPassengers, AirPassengers_lambda)))


## Finding cycles in the noise: Using spectral analysis to identify periodicity

data(sunspot.year)

sunny = TSA::periodogram(diff(sunspot.year))

sunny_df = data.frame(freq = sunny$freq, spec = sunny$spec)

sunny_df[which.max(sunny_df[,2]),]


## Plotting survival curves

require(survival)
require(survminer)

# Get data and add names of cancer type

data(tongue, package = "KMsurv")

tongue$tumor = ifelse(tongue$type==1, "Aneuploid",  "Diploid")

# Create survival model
tongue_survival = survfit(Surv(time = time, event = delta) ~ tumor, data = tongue)

# Plot the curves and table
ggsurvplot(tongue_survival, risk.table = TRUE, conf.int = TRUE)


## Evaluating quality with control charts

require(qcc)
infections = c(6, 2, 5, 1, 3, 4, 2, 6, 3, 2, 4, 7, 1, 1, 4, 4, 1, 5, 2, 3, 5, 2, 3, 2, 4)

patient_days = c(985, 778, 1010, 834, 750, 729, 1002, 639, 985, 578, 976, 540, 829, 723, 908, 1017, 1097, 1122, 1234, 1022, 1167, 1098, 1201, 1045, 1141)

month_name = month.abb[c(1:12, 1:12, 1:1)]

infection_control = qcc(infections, sizes=patient_days/1000, type="u", labels=month_name, axes.las=2, xlab="Month", ylab="", digits=2, title="Hospital Acquired Infections Rate per 1,000 Patient Days\n u-chart for Jan 2012 - Jan 2014")

infection_control_qcc = data.frame(Month = month_name, infection_control$limits, Rate = (infections / patient_days)*1000)


## Identifying possible breakpoints in a time series

# http://www.eia.gov/beta/MER/index.cfm?tbl=T12.01#/?f=M&start=200101&end=201506&charted=0-1-13    

require(strucchange)

US_co2 = read.table("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T12.01", sep=",", header=T)

US_co2 = filter(US_co2, Column_Order == 14 & YYYYMM >= 200102 & substr(YYYYMM, 5, 7) != 13)

US_co2_ts = ts(US_co2[,3], freq=12, start=c(2001,2))
  
# Obtain breakpoints estimate
breakpoints(US_co2_ts ~ 1)

# Plot the time series
plot(US_co2_ts, main=expression(paste("Breakpoint in Monthly US"~CO[2]~Emissions)), ylab="Million Metric Tons", col="darkblue", lwd=1.5)

# Plot the line at the optimal breakpoint
lines(breakpoints(US_co2_ts ~ 1), col="darkgreen")

# Plot a 90% confidence interval
lines(confint(breakpoints(US_co2_ts ~ 1), level=0.90), col="darkgreen")

# Add breakpoint location text
text(2008.8, 555, "Jan 2009", cex=0.75, col="darkgreen", pos=4, font=3)


## Exploring relationships between time series: cross-correlation

# Load dplyr to subset the BLS data
require(dplyr) 

# Download CPI time series data from US BLS
CPI_xport = read.table("http://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation", header=T, sep="\t", strip.white=T)

# CPI for motor fuel, 2000-2014
fuel = filter(CPI_xport, series_id == "CUUR0000SETB" & year >= 2000 & year <= 2014 & period != "M13")

# CPI for airline fare, 2000-2014              
fare = filter(CPI_xport, series_id == "CUUR0000SETG01" & year >= 2000 & year <= 2014 & period != "M13")

# Create time series
fuel_ts = ts(fuel$value, start=c(2000, 1), freq=12)

fare_ts = ts(fare$value, start=c(2000, 1), freq=12)

# CCF *requires* detrended data
# Get detrended values of each time series

fuel_detrend = na.omit(decompose(fuel_ts)$random)

fare_detrend = na.omit(decompose(fare_ts)$random)

acf(ts.union(fuel_detrend, fare_detrend))
  
# Calculate CCF with +/- 4 year span
ccfvalues = ccf(fuel_detrend, fare_detrend, lag.max=48)

ccfvalues

# Function to find CCF
Max_CCF = function(a, b) {
  d = ccf(a, b, plot = FALSE, lag.max = length(a)-5)
  cor = d$acf[,,1]
  abscor = abs(d$acf[,,1])
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  absres = data.frame(abscor,lag)
  absres_max = res[which.max(absres$abscor),]
  return(absres_max)
}

# Determine maximum CCF value
Max_CCF(fuel_detrend, fare_detrend)


## Basic forecasting

require(forecast)
require(dplyr)

# Download data and load into R
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip", destfile = "Data/household_power_consumption.zip")

unzip("Data/household_power_consumption.zip", exdir="Data")

power = read.table("Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

# Convert and create date objects
power$Date = as.Date(power$Date, format="%d/%m/%Y")

power$Month = format(power$Date,"%Y-%m")

power$Month = as.Date(paste0(power$Month, "-01"))

# Interpolate missing values and summarize monthly total power use
power$Global_active_power_locf = na.locf(power$Global_active_power)

power_group = group_by(power, Month)

power_monthly = summarise(power_group, Total_Use_kWh = sum(Global_active_power_locf)/60)

# Remove partial months from data frame
power_monthly = power_monthly[2:47,]

# Create a time series object of Total Usage
total_use_ts = ts(power_monthly$Total_Use_kWh, start=c(2007,1), frequency=12)

# Automatically obtain the forecast for the next 6 months
total_use_fc = forecast(total_use_ts, h=6)

# View the forecast model results
summary(total_use_fc)

plot(total_use_fc)


##### End of File #####
