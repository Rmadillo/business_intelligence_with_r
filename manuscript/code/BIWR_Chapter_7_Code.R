#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 7 Code: Dataviz
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_7_Code.R
#
######################################################################

## Plotting multivariate distributions

### Heatmaps

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

green_color_ramp = brewer.pal(9, "Greens")

calendarHeat(bike_share_daily$dteday, bike_share_daily$casual, varname="Casual Daily Bike Use", color="green_color_ramp")


### Parallel coordinates plots

require(GGally)
require(gdata)

qb_url = "http://ciberconta.unizar.es/leccion/multivar/QUIEBRA.XLS"

quiebra_download = read.xls(qb_url, sheet=1, header=TRUE, stringsAsFactors=FALSE)

quiebra = quiebra_download[,2:12]

ggparcoord(data=quiebra, columns=c(2:10), groupColumn=11, scale="globalminmax", alphaLines=0.5)

ggparcoord(data=quiebra, columns=c(2:10), groupColumn=11, scale="globalminmax", boxplot=TRUE, alphaLines=0.3)


### Peeking at multivariate data with `dplyr` and a bubblechart

bike_share_daily %>%
  filter(workingday == "Yes" & weathersit != 1) %>%
  group_by(yr, mnth) %>%
  summarize(mean_casual = mean(casual, na.rm=T),
    sd_casual = sd(casual, na.rm=T),
    sum_casual = sum(casual)) %>%
  mutate(cv_casual = round(sd_casual / mean_casual, 2)) %>%
  ggplot(aes(mean_casual, sd_casual, color=cv_casual, size=sum_casual)) +
    geom_point() +
    scale_size(range=c(2,6)) +
    coord_equal() +
    theme_bw()


## Plotting a table

require(reshape2)
require(gridExtra)

bike_share_grp = group_by(bike_share_daily, yr, mnth)

bike_share_mean = summarise(bike_share_grp, mean=mean(casual))

bike_share_mean$mean = round(bike_share_mean$mean, 0)

bike_share_mean_wide = dcast(bike_share_mean, yr~mnth, value.var="mean")

bike_share_mean_wide = rename(bike_share_mean_wide, Year = yr)

ggplot(bike_share_mean_wide, aes(Year, Jan)) +
  annotation_custom(tableGrob(bike_share_mean_wide, rows=NULL)) +
  ggtitle("Mean Daily Casual Bike Share Use, by Month (2011-2012)") +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
    axis.text = element_blank(), axis.title = element_blank())


## Interactive dataviz

### Basic interactive plots

# devtools::install_github("hrbrmstr/taucharts")
require(taucharts)
require(dplyr)

bike_share_daily$weather = as.factor(ifelse(bike_share_daily$weathersit == 1, "Clear", ifelse(bike_share_daily$weathersit == 2, "Cloudy/Rainy", "Stormy")))

bike_share_count = bike_share_daily %>% 
  group_by(weather) %>%
  summarize(count=n())

bike_share_count_season = bike_share_daily %>% 
  group_by(weather, season) %>%
  summarize(count=n())
  
tauchart(bike_share_count) %>% 
  tau_bar("weather", "count") %>% 
  tau_tooltip()

tauchart(bike_share_count) %>% 
  tau_point("weather", "count") %>% 
  tau_tooltip()

tauchart(bike_share_count_season) %>% 
  tau_bar("count", "season", "weather", horizontal=T) %>% 
  tau_legend() %>%
  tau_tooltip()

tauchart(bike_share_daily) %>%
  tau_point("atemp", "casual") %>%
  tau_tooltip()

tauchart(bike_share_daily) %>% 
  tau_point(c('weather', 'atemp'), c('season', 'casual')) %>%
  tau_tooltip()


### Scatterplot matrix

require(pairsD3)

pairsD3(bike_share_daily[,11:14], group = bike_share_daily[,3], opacity = 0.7, tooltip = paste("Season: ", bike_share_daily$season, "<br/> Casual use count: ", bike_share_daily$casual), col = c("#0571b0", "#92c5de", "#ca0020", "#f4a582"))


### Motionchart: a moving bubblechart

# load the googleVis package

library(googleVis)

# load the data (an excerpt of data from gapminder.org)

rosling_data = read.csv("https://raw.githubusercontent.com/Rmadillo/UW_HCA/master/data/rosling_data.csv")

# create the motion plot using larger plot size than default
plot(gvisMotionChart(rosling_data, idvar = "Country", timevar = "Year", sizevar = "Population", options=list(width = 700, height = 600)))


### Interactive tables with `DT`

require(DT)

datatable(rosling_data, filter = "top")


## Making maps in R

# http://cran.r-project.org/web/views/Spatial.html

### Basic point maps

require(maps)

map("county", xlim=c(min(ozone$x-0.5),max(ozone$x+0.5)), ylim=range(ozone$y), col="gray80")

map("state", xlim=c(min(ozone$x-0.5),max(ozone$x+0.5)), ylim=range(ozone$y), col="gray60", add=TRUE)

box(col="gray50")

text(ozone$x, ozone$y, ozone$median, cex=0.5)
  
map('world', xlim=c(-12,45), ylim=c(35,60), col="gray90", fill=T)

box()


### Chloropleth maps

require(choroplethr)
require(choroplethrMaps)

data(df_pop_county)

county_choropleth(df_pop_county)

county_choropleth(df_pop_county, legend="County\nPopulation", num_colors=1, state_zoom=c("arizona", "colorado", "new mexico", "utah"))


### Chloropleth mapping with the *American Community Survey*

# http://www.census.gov/data/developers/about/terms-of-service.html
  
require(acs)

api.key.install("YOUR KEY HERE")

choroplethr_acs(tableId="B19113", map="county", buckets=4, endyear=2012)

# http://factfinder2.census.gov/faces/affhel/jsf/pages/metadata.xhtml?lang=en&type=survey&id=survey.en.ACS_ACS


### Using Shapefiles

download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/CNTR_2014_03M_SH.zip", destfile="world2014.zip")
# © EuroGeographics for the administrative boundaries

unzip("~/Downloads/world2014.zip", exdir="~/Downloads/world2014", junkpaths = TRUE)

require(maptools)

worldmap2014 = readShapeLines("~/Downloads/world2014/CNTR_BN_03M_2014.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

plot(worldmap2014, xlim=c(7,16), ylim=c(35,60), col="gray60")

box(col="gray30")

# http://www.eea.europa.eu/data-and-maps/data/airbase-the-european-air-quality-database-8

eu_o3 = read.table("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/eu_o3.csv", sep=",", header=T)

plot(worldmap2014, xlim=c(2,12), ylim=c(35, 70), col="gray60")

box(col="gray30")

text(eu_o3$longitude, eu_o3$latitude, round(eu_o3$statistic_value,0), cex=0.5, col="gray20")

require(RColorBrewer)

o3_colors = brewer.pal(5, "OrRd")

require(classInt)

o3_breaks = classIntervals(sort(eu_o3$statistic_value), n=5, style="quantile")

plot(worldmap2014, xlim=c(2,12), ylim=c(35, 70), col="gray60")

box(col="gray30")

points(eu_o3$longitude, eu_o3$latitude, pch=15, cex=0.5, col=o3_colors[findInterval(eu_o3$statistic_value, o3_breaks$brks, all.inside=TRUE)])

legend("topleft", title=expression(O[3]~Levels~(ppb)), inset=0.007, cex=0.8, legend=leglabs(round(o3_breaks$brks),0), fill=o3_colors, bg="white", border="gray60")


### Using `ggmap` for point data and heatmaps

require(ggmap)
require(dplyr) 

# SPD Police Reports
spd = read.csv("https://data.seattle.gov/api/views/7ais-f98f/rows.csv", header=T, strip.white = T)

# Filter to 2 years with full data
spd = filter(spd, Year >= 2014 & Year < 2016)

# Filter to bike thefts only
bike_theft = filter(spd, Summarized.Offense.Description == "BIKE THEFT")

## Set up base map
seattle_map = ggmap(get_map('Seattle, Washington', zoom=11, source='google', maptype='terrain'), base_layer = ggplot(aes(x = Longitude, y = Latitude), data = bike_theft))

# Point map of bike theft
seattle_map +  
  geom_point(alpha=0.5, size=0.5) +
  scale_alpha(guide = FALSE) +
  ggtitle('Seattle Bike Thefts (2014-2015)') +
  xlab('') +
  ylab('') +
  theme(axis.ticks=element_blank(), axis.text=element_blank(),
  legend.position='none')

# Density map of bike theft by year
seattle_map +  
  stat_density2d(aes(fill=..level..), 
  alpha=0.5,
  bins = 10,
  geom = 'polygon') +
  scale_alpha(guide = FALSE) +
  ggtitle('Seattle Bike Thefts (2014-2015)') +
  xlab('') +
  ylab('') +
  facet_wrap(~Year) +
  theme(axis.ticks=element_blank(), axis.text=element_blank(),
  legend.position='none')


### Interactive maps with `leaflet`

require(leaflet)

bike_theft_dec = filter(bike_theft, Year==2014 & Month==12)

bike_theft_jan = filter(bike_theft, Year==2015 & Month==1)

bike_theft_interactive = leaflet() %>% 
  addTiles() %>% 
  setView(-122.3397, 47.6144, zoom = 13) %>%
  addCircleMarkers(data = bike_theft_dec, 
    lat = ~ Latitude, 
    lng = ~ Longitude, 
    popup = bike_theft$Hundred.Block.Location, 
    group="December 2014", 
    fillOpacity = 0.25, 
    color="red", 
    radius=4) %>%
  addCircleMarkers(data = bike_theft_jan, 
    lat = ~ Latitude, 
    lng = ~ Longitude, 
    popup = bike_theft$Hundred.Block.Location, 
    group="January 2015", 
    fillOpacity = 0.25, 
    color="blue", 
    radius=4) %>%
  addLayersControl(overlayGroups = c("December 2014", "January 2015"), 
    options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("bottomright",  title = "Legend", 
    colors=c("red","blue"), labels=c("December 2014", "January 2015"))

bike_theft_interactive


##### End of File #####
