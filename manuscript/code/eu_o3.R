# Code used to set up Ozone Pollution in EU
# Chapter 7 Mapping section

# Net result is eu_o3.csv in this directory

# Data requires manual download from 
# http://www.eea.europa.eu/data-and-maps/data/airbase-the-european-air-quality-database-8
# Download both the stations and the statistics zip files

o3stations = read.table("~/Downloads/AirBase_v8_stations.csv", sep="\t", header=T, 
                        stringsAsFactors=F, quote="")
o3data = read.table("~/Downloads/AirBase_v8_statistics.csv", sep="\t", header=T, 
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
