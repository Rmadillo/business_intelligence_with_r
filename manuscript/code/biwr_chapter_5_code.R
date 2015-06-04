# Chapter 5: Trends and Time

bike_share_daily = read.table("/home/db/RDSCB/archive/Chapter3/Data/bike_share_daily.csv", 
                              sep=",", header=T,
                            colClasses=c("character", "Date", "factor", "factor", "factor", "factor",
                                         "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric",
                                         "integer", "integer", "integer"))
levels(bike_share_daily$season) = c("Winter", "Spring", "Summer", "Fall")
levels(bike_share_daily$workingday) = c("No", "Yes")
levels(bike_share_daily$holiday) = c("No", "Yes")
bike_share_daily$mnth = as.factor(as.numeric(bike_share_daily$mnth))
levels(bike_share_daily$mnth) = c(month.abb)
levels(bike_share_daily$yr) = c(2011, 2012)


## Describing trends in non-temporal data

require(ggplot2)
require(quantreg)

png("biwr_05_loess.png", width=6, height=6, res=300, units="in")
ggplot(bike_share_daily, aes(x=atemp, y=casual)) +
    xlab("Daily Mean Normalized Air Temperature") +
    ylab("Number of Total Bike Uses") +
    geom_point(col="gray50") +
    geom_smooth(method="loess") +
    theme_bw()
dev.off()

png("biwr_05_quant.png", width=6, height=6, res=300, units="in")
ggplot(bike_share_daily, aes(x=temp, y=casual)) +
    xlab("Daily Mean Normalized Temperature") +
    ylab("Number of Casual Bike Uses") +
    geom_point(col="gray50") +
    stat_quantile(aes(colour = ..quantile..), quantiles = c(0.05, 0.1, 0.25,
         0.5, 0.75, 0.9, .95)) +
    scale_colour_gradient2(midpoint=0.5, low="steelblue", mid="brown", 
         high="steelblue ") +
    theme_bw()
dev.off()

png("biwr_05_linear.png", width=6, height=6, res=300, units="in")
ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
    xlab("Daily Mean Normalized Air Temperature") +
    ylab("Number of Total Bike Uses") +
    geom_point(col="gray50") +
    geom_smooth(method="lm") +
    theme_bw()
dev.off()


## Segmented regression

require(segmented)
bike_segment = segmented(lm(cnt~atemp, data=bike_share_daily), ~atemp, psi=0.6)
bike_segment$psi

psi = bike_segment$psi[2]

png("biwr_05_segment.png", width=6, height=6, res=300, units="in")
ggplot(bike_share_daily, aes(x=atemp, y=cnt, group = atemp > psi)) +
    xlab("Daily Mean Normalized Air Temperature") +
    ylab("Number of Total Bike Uses") +
    geom_point(col="gray50") +
    geom_smooth(method="lm") +
    theme_bw()
dev.off()


## Detecting autocorrelation



## Working with temporal data

require(lubridate)

india_natl_holidays = c("Jan 26, 2014", "Apr 13, 2014", "Aug 15, 2014", 
                        "Oct 02, 2014", "Dec 25, 2014")
india_natl_holidays = as.Date(india_natl_holidays, format="%b %d, %Y")
str(india_natl_holidays)

birthdays = c("20000101", "19991201", "19760704", "20140314")
ymd(birthdays)

age = ymd(today()) - ymd(birthdays)
round(age/eyears(1), 1)

age = ymd(20141201) - ymd(birthdays)
round(age/eyears(1), 0)

## Calculate a mean or correlation in circular time (clock time)

require(psych)
wake_time = c(7.0, 7.5, 6.5, 6.25, 7.0, 7.25, 7.25)
sleep_time = c(23.5, 0.5, 23.75, 0.25, 23.25, 23.5, 0.75)
wake_sleep = data.frame(wake_time, sleep_time)

circadian.mean(wake_sleep$sleep_time)
circadian.cor(wake_sleep)

## Plotting time-series data

UK_births = read.table("/home/db/RDSCB/archive/Chapter3/Data/UK_births.csv", 
                       header=T, sep=",", stringsAsFactors=F)

UK_births_ts = ts(UK_births$Value, start = c(2003, 1), frequency = 12)

png("biwr_05_ukbirths1.png", width=6, height=6, res=300, units="in")
plot(UK_births_ts, xlab="Date", ylab="Count", main="Monthly Births in the UK 
     (2003-2012)", col="darkblue")
dev.off()


## No day value but you want to plot it in ggplot?

require(ggplot2)

UK_births$Date = as.Date(paste("1", UK_births$MONTH, UK_births$TIME, sep=" "), 
                         format="%d %B %Y")

png("biwr_05_ukbirths2.png", width=6, height=6, res=300, units="in")
ggplot(UK_births, aes(x=Date, y=Value)) +
    geom_line(col="darkblue") +
    ylab("Count") +
    theme_minimal()
dev.off()

## Plotting monthly patterns

png("biwr_05_ukbirths3.png", width=6, height=6, res=300, units="in")
monthplot(UK_births_ts, main="Monthly Patterns in UK Births (2003-2012)", 
          xlab="Month", ylab="Count", col="darkblue", lwd=2, lty.base=2, lwd.base=1, 
          col.base="gray40")
dev.off()

require(forecast)

png("biwr_05_ukbirths4.png", width=6, height=6, res=300, units="in")
seasonplot(UK_births_ts, main="Seasonal Trends in UK Births (2003-2012)", 
           col=rainbow(10), year.labels=TRUE, year.labels.left=TRUE, cex=0.7, 
           cex.axis=0.8)
dev.off()

## Evaluating quality with control charts

require(qcc)
infections = c(6, 2, 5, 1, 3, 4, 2, 6, 3, 2, 4, 7, 1, 1, 4, 4, 1, 5, 2, 3, 5, 
               2, 3, 2, 4)
patient_days = c(985, 778, 1010, 834, 750, 729, 1002, 639, 985, 578, 976, 540, 
                 829, 723, 908, 1017, 1097, 1122, 1234, 1022, 1167, 1098, 1201, 1045, 1141)

png("biwr_05_qcc1", width=10, height=6, res=300, units="in")
infection_control = qcc(infections, sizes=patient_days, type="u",
                        labels=month.abb[c(1:12, 1:12, 1:1)], axes.las=2, xlab="Month", ylab="", 
                        title="Hospital Acquired Infections Rate\nu-chart for Jan 2012 - Jan 2014")
dev.off()

## Identifying possible breakpoints in a time series

require(strucchange)
us_co2 = read.table("/home/db/RDSCB/archive/Chapter3/Data/US_CO2_2001_2014.csv",
                    header=T, sep=",")
us_co2_ts = ts(us_co2[,2], freq=12, start=c(2001,2))

breakpoints(us_co2_ts ~ 1)

png("biwr_05_bps.png", width=10, height=6, res=300, units="in")
plot(us_co2_ts, main=expression(paste("Breakpoint in Monthly US"
        ~CO[2]~Emissions)), ylab="Million Metric Tons", col="darkblue", lwd=1.5)
lines(breakpoints(us_co2_ts ~ 1), col="darkgreen")
text(2008.9, 555, "Jan 2009", cex=0.75, col="darkgreen", pos=4, font=3)
dev.off()

## Decomposing time series into components

require(forecast)

png("biwr_05_decomp1", width=10, height=6, res=300, units="in")
plot(decompose(UK_births_ts), col="darkblue")
dev.off()


## Decomposing when the variance changes

data(AirPassengers)
plot(AirPassengers)
AirPassengers_lambda = BoxCox.lambda(AirPassengers)
png("biwr_05_decomp2", width=10, height=6, res=300, units="in")
plot(BoxCox(AirPassengers, AirPassengers_lambda))
dev.off()


## Using spectral analysis to find periodicity

data(sunspot.year)

sun_spec = spectrum(sunspot.year)

png("biwr_05_spectral", width=10, height=6, res=300, units="in")
plot(sun_spec)
dev.off()

## Fourier transforms, wavelets, and other decomposition


## Exploring relationships between time series: cross-correlation
