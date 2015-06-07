# Chapter 6: A Dog's Breakfast of Dataviz

require(ggplot2)
require(dplyr)

bike_share_grp = group_by(bike_share_daily, weekday, mnth)
bike_share_mean = summarise(bike_share_grp, mean=mean(casual))

ggplot(bike_share_mean, aes(weekday, mnth)) +
    geom_tile(aes(fill = mean)) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    xlab("Day of the Week") +
    ylab("Month") +
    ggtitle("Mean Daily Casual-Use Bike Sharing") +
    scale_y_discrete(limits = rev(levels(bike_share_mean$mnth))) +
    theme_bw() +
    theme(plot.title = element_text(vjust = 1))

bike_share_grp = group_by(bike_share_daily, weekday, mnth)
bike_share_sd = summarise(bike_share_grp, sd=sd(casual))

ggplot(bike_share_sd, aes(weekday, mnth)) +
    geom_tile(aes(fill = sd)) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    xlab("Day of the Week") +
    ylab("Month") +
    ggtitle("Standard Deviation of Daily Casual-Use Bike Sharing") +
    scale_y_discrete(limits = rev(levels(bike_share_sd$mnth))) +
    theme_bw() +
    theme(plot.title = element_text(vjust = 1))

### Creating calendar heatmaps

require(RColorBrewer)
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
new_color_ramp = brewer.pal(9, "Greens")
calendarHeat(bike_share_daily$dteday, bike_share_daily$casual, 
    varname="Casual Daily Bike Use", color="new_color_ramp")


### Parallel coordinates plots

require(GGally)
require(gdata)
quiebra_ download = read.xls("http://ciberconta.unizar.es/leccion/multivar/QUIEBRA.XLS", 
    sheet=1, header=TRUE, stringsAsFactors=FALSE)
quiebra = quiebra_download[,2:12]

ggparcoord(data=quiebra, columns=c(2:10), groupColumn=11, 
    scale="globalminmax", alphaLines=0.5)

ggparcoord(data=quiebra, columns=c(2:10), groupColumn=11, 
    scale="globalminmax", boxplot=TRUE, alphaLines=0.3)


### Peeking at multivariate data with dplyr and a bubblechart

require(dplyr)
require(ggplot2)

customer_survey = read.table("Data/customer_survey.csv", sep=",", header=T)
customer_survey %>%
    filter(customer_purchases >= 500) %>%
    group_by(state) %>%
    summarize(mean_purchases = mean(customer_purchases, na.rm=T),
        sd_purchases = sd(customer_purchases, na.rm=T),
        sum_est = sum(est)) %>%
    mutate(cv_purchases = round(sd_purchases / mean_purchases, 2)) %>%
    ggplot(aes(mean_purchases, sd_purchases, color=cv_purchases, size=sum_est)) +
        geom_point() +
        scale_size(range=c(2,6)) +
        theme_bw()


## Plotting a table

require(reshape2)
require(gridExtra)

bike_share_grp = group_by(bike_share_daily, yr, mnth)
bike_share_mean = summarise(bike_share_grp, mean=mean(casual))
bike_share_mean$mean = round(bike_share_mean$mean, 0)
bike_share_mean_wide = dcast(bike_share_mean, yr~mnth)
bike_share_mean_wide = rename(bike_share_mean_wide, Year = yr)

ggplot(bike_share_mean_wide, aes(Year, Jan)) +
    annotation_custom(tableGrob(bike_share_mean_wide, show.rownames = FALSE)) +
    ggtitle("Mean Daily Casual Bike Share Use, by Month (2011-2012)") +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
    axis.text = element_blank(), axis.title = element_blank())


## Making maps in R

### Basic point maps

require(maps)
map("county", xlim=c(min(ozone$x-0.5),max(ozone$x+0.5)), ylim=range(ozone$y),
    col="gray80")
map("state", xlim=c(min(ozone$x-0.5),max(ozone$x+0.5)), ylim=range(ozone$y),
    col="gray60", add=TRUE)
box(col="gray50")
text(ozone$x, ozone$y, ozone$median, cex=0.5)

map('world', xlim=c(-12,45), ylim=c(35,60), col="gray90", fill=T)
box()


### Chloropleth maps

require(choroplethr)
require(choroplethrMaps)
data(df_pop_county)
county_choropleth(df_pop_county)

county_choropleth(df_pop_county,
    legend="County\nPopulation",
    buckets=1,
    zoom=c("arizona", "colorado", "new mexico", "utah"))

require(zipcode)
data(df_pop_zip)
zip_map(df_pop_zip,
    legend="Zip Code\nPopulation",
    buckets=1,
    zoom=c("arizona", "colorado", "new mexico", "utah"))

### Chloropleth mapping with the American Community Survey

# http://www.census.gov/data/developers/about/terms-of-service.html
# http://factfinder2.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=survey&id=survey.en.ACS_ACS

require(acs)
api.key.install("YOUR KEY HERE")
require(choroplethr)
require(choroplethrMaps)
choroplethr_acs(tableId="B19113", map="county", buckets=4, endyear=2012)


### Example: Ozone pollution in the EU

download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/CNTR_2014_03M_SH.zip", 
    destfile="world2014.zip")
unzip("world2014.zip")
require(maptools)
worldmap2014 = readShapeLines("CNTR_2014_03M_SH/Data/CNTR_BN_03M_2014.shp", 
    proj4string = CRS("+proj=longlat +datum=ETRS89"))
plot(worldmap2014, xlim=c(7,16), ylim=c(35,60), col="gray60")
box(col="gray30")
# © EuroGeographics for the administrative boundaries

o3stations = read.table("Data/AirBase_v8_stations.csv", sep=",", header=T, 
    stringsAsFactors=F, quote="")
o3data = read.table("Data/AirBase_v8_statistics.csv", sep="\t", header=T, 
    stringsAsFactors=F, quote="")

require(sqldf)
eu_o3 = sqldf("SELECT
              a.station_european_code
              , a.component_caption
              , a.statistics_year
              , a.statistics_average_group
              , a.statistic_shortname
              , a.statistic_value
              , b.station_european_code
              , b.country_iso_code
              , b.country_name
              , b.station_city
              , b.station_longitude_deg as longitude
              , b.station_latitude_deg as latitude
              , b.station_altitude as altitude
              FROM o3data a
              INNER JOIN o3stations b
              ON a.station_european_code = b.station_european_code
              WHERE a.component_caption = 'O3'
              AND a.statistics_year = 2012
              AND a.statistics_average_group = 'day'
              AND a.statistic_shortname = 'P50'
              ")

plot(worldmap2014, xlim=c(2,12), ylim=c(35, 70), col="gray60")
box(col="gray30")
text(eu_o3$longitude, eu_o3$latitude, round(eu_o3$statistic_value,0), cex=0.5, 
    col="gray20")

require(RColorBrewer)
o3_colors = brewer.pal(5, "OrRd")
require(classInt)
o3_breaks = classIntervals(sort(eu_o3$statistic_value), n=5, style="quantile")
plot(worldmap2014, xlim=c(2,12), ylim=c(35, 70), col="gray60")
box(col="gray30")
points(eu_o3$longitude, eu_o3$latitude, pch=15, cex=0.5, 
    col=o3_colors[findInterval(eu_o3$statistic_value, o3_breaks$brks, 
    all.inside=TRUE)])
legend("topleft", title=expression(O[3]~Levels~(ppb)), inset=0.007, cex=0.8,
    legend=leglabs(round(o3_breaks$brks),0), fill=o3_colors, bg="white",
    border="gray60")


### ggmap example


## Interactive dataviz


