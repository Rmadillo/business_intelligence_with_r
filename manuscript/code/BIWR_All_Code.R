##############################################################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 1 Code: Entire project, code only
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_All_Code.R
#
##############################################################################################################



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
power = read.table("Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)


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
calendarHeat(power_day_missing$Date, power_day_missing$Count_Missing, varname="Missing Data", color="w2b")

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

# Set the working directory back to normal
setwd("~/")


##### End of Chapter 1 #####



#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 2 Code: Getting data
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_2_Code.R
#
#####################################################################


## Working with files

### read.table

# read.table from Chapter 1
power = read.table("~/BIWR/Chapter1/Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

# Read in a csv from the internet with non-ASCII characters
pses2011 = read.table("http://www.tbs-sct.gc.ca/pses-saff/2011/data/2011_results-resultats.csv", sep=",", encoding="UTF-8")


### Reading big files with data.table
require(data.table)

# If you want to read in a csv directly, use fread, e.g., if you
# had the raw pses2011 in a local csv, you'd read it in this way:
power = fread("~/BIWR/Chapter1/Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

#If you *already* have a data frame and want to speed up reading 
# and accessing it, use the `data.table` function:
power = data.table("power")


### Unzipping files within R
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", "Bike-Data.zip")

bike_share_daily = read.table(unz("Bike-Data.zip", "day.csv"), header=T, quote="\"", sep=",")

unzip("Bike-Data.zip")

bike_share_daily = read.table("day.csv", header=T, sep=",")
bike_share_hourly = read.table("hour.csv", header=T, sep=",")
bike_share_readme = readLines("Readme.txt")


### Reading Excel files

# Load the readxl package
require(readxl)

# Load the Excel workbook from the readxl package
datasets = system.file("extdata/datasets.xlsx", package = "readxl")

# Read in the first worksheet
iris_xl = read_excel(datasets, sheet = 1)

# Load the XLConnect package
require(XLConnect)

# Load the Excel workbook
wb = loadWorkbook(system.file("extdata/datasets.xlsx", package = "readxl"))

# Read in the first tab
data_from_sheet = readWorksheet(wb, sheet = 1)

# Get a list of worksheets in the workbook
sheets = getSheets(wb)

# Invisibly return each sheet as its own data frame
invisible(  
    lapply(sheets,function(sheet) 
        assign(sheet,readWorksheet (wb, sheet = sheet ), pos = 1))
)

# Load gdata package
require(gdata)

# Download Excel file form web and read in the first sheet to a data frame
pses2011_header = read.xls("http://www.tbs-sct.gc.ca/pses-saff/2011/data/PSES2011_Documentation.xls", sheet="Layout-Format", nrows=22, header=FALSE, skip=1, col.names=c("Variables", "Size", "Type", "Description"), stringsAsFactors=FALSE)

# Assign the Variable column names from the Excel file to the raw data file
colnames(pses2011) = pses2011_header$Variable


### Creating a dataframe from the clipboard or direct entry

# Generic code for reading data from the clipboard
my_data_frame = read.table("clipboard", sep="\t", header=TRUE)

# Fake data in vectors
Survey_Response = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
Send_Email_Ad = c("Yes", "Yes", "No", "No", "No")

# Combine vectors into a data frame
marketing = data.frame(Survey_Response, Send_Email_Ad)

# An R take on SAS's datalines
marketing = read.table(textConnection
    ("Survey_Response, Send_Email_Ad                         
    Strongly Agree, Yes
    Agree, Yes
    Neutral, No
    Disagree, No
    Strongly Disagree, No"), 
    header=TRUE, sep=",") 


### Reading XML files

require(XML)

# Read in the webpage as-is
vha_pt_sat_raw = readLines("http://www1.va.gov/VETDATA/docs/Datagov/Data_Gov_VHA_2010_Dataset11_Patient_Satisfaction.xml")

# Convert xml to a data frame
VHA_Patient_Satisfaction = xmlToDataFrame(vha_pt_sat_raw)

# Read the file from the web
FHFA_HPI_raw = readLines("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.xml")

# Convert the XML to an R list 
FHFA_HPI_list = xmlToList(xmlParse(FHFA_HPI_raw))

# Turn the list into a data frame
FHFA_HPI = data.frame(do.call(rbind, FHFA_HPI_list), row.names = make.unique(names(FHFA_HPI_list)))


### Reading JSON files 

# Load the jsonlite package
require(jsonlite)

# Get JSON data of UFO sightings
ufos = fromJSON("http://metricsgraphicsjs.org/data/ufo-sightings.json")

# Acquire the JSON data from Yahoo finance
currency_list = fromJSON("http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json")

# This takes the piece of the list we need and brings it out as a data frame
currency_data = currency_list$list$resources$resource$fields

# Look at the data structure
str(currency_data)

# pseudo-JSON
# Download Enron emails to the working directory from jsonstudio
download.file("http://jsonstudio.com/wp-content/uploads/2014/02/enron.zip", "enron.zip")

unzip("enron.zip")

# Bring in the streaming json as a data frame
enron = stream_in(file("enron.json"))


## Working with databases

### Connecting to a database

require(RODBC)

# Hard-coded password entry
connection = odbcConnect("NAME OF DSN CONNECTION", uid = "YOUR USER ID", pwd = "YOUR PASSWORD")

# Interactive password entry in RStudio
connection = odbcConnect("NAME OF DSN CONNECTION", uid = "YOUR USER ID", pwd = .rs.askForPassword("Please enter your password"))

# Verify the connection is working:
odbcGetInfo(connection)

# To see all tables:
sqlTables(connection)

# To see all tables in a given schema, e.g. dbo:
sqlTables(connection, schema="dbo")

# Create an object and then view it within R:
connection_tables = sqlTables(connection)
connection_tables

### Creating data frames from a database

# Fetch a table
whole_table = sqlFetch(connection, "table_name")

# Make a SQL query and put the results into a dataframe

# Within the double quotes you can put a valid SQL query,
# using single quotes to name the table
query_1 = "SELECT * FROM 'table_name'"

# Then use sqlQuery to pull the table into R
query_1_dataframe = sqlQuery(connection, query_1)

# While not as clear for large queries, you can combine them into one step:
query_1_dataframe = sqlQuery(connection, "SELECT * FROM 'table_name'")

# Usual SQL syntax works
query_2 = "SELECT * FROM 'table_name$' 
WHERE variable_name = some_condition 
ORDER BY ordering_variable_name"

query_2_dataframe = sqlQuery(connection, query_2)


### Disconnecting from a database
odbcCloseAll()

vignette("RODBC", package="RODBC")


### Creating a SQLite database inside R

require(sqldf)

# Create empty database
sqldf("attach 'PSES_database.sqlite' as new")

# Create a connection
connect = dbConnect(SQLite(), dbname="PSES_database.sqlite")

# Write a data frame into a database table
# options are connection name, name of the table to write, and name of the dataframe
dbWriteTable(connect, "pses2011", pses2011)

# Download the PSES2011 csv file (instead of importing it, as above)
download.file("http://www.tbs-sct.gc.ca/pses-saff/2011/data/2011_results-resultats.csv", "pses2011.csv")

# Read the csv into the database as a table called pses2011_downloaded
read.csv.sql("pses2011.csv", sql = "CREATE TABLE pses2011_downloaded AS SELECT * FROM file", dbname = "PSES_database.sqlite")

# Populate database with Excel files using `XLConnect` 

# Download the Excel file
download.file("http://www.tbs-sct.gc.ca/pses-saff/2011/data/PSES2011_Documentation.xls", "PSES2011_Documentation.xls")

# Load the Excel file into R
pses2011_xls = loadWorkbook("PSES2011_Documentation.xls")

# Read each worksheet into separate dataframes within a list
pses2011_documentation = readWorksheet(pses2011_xls, sheet=getSheets(pses2011_xls))

# The sheet names have spaces and hyphens, which will cause
# trouble for SQLite; run names(pses2011_documentation) to see
# So, this changes the dataframe names inside the list
names(pses2011_documentation) = c("Questions", "Agency", "LEVELID15", "Demcode", "PosNegSpecs", "LayoutFormat")

# Add a new row to account for 0 values for LEVEL1ID in
# main pses2011 file
pses2011_documentation$Agency = rbind(pses2011_documentation$Agency, (0, NA, "Other", NA, "OTH"))

# Now each sheet can be loaded into the database as a separate table
with(pses2011_documentation, {
    dbWriteTable(conn=connect, name="Questions", value=Questions, row.names=FALSE)
    dbWriteTable(conn=connect, name="Agency", value=Agency, row.names=FALSE)
    dbWriteTable(conn=connect, name="LEVELID15", value=LEVELID15, row.names=FALSE)
    dbWriteTable(conn=connect, name="Demcode", value=Demcode, row.names=FALSE)
    dbWriteTable(conn=connect, name="PosNegSpecs", value=PosNegSpecs, row.names=FALSE)
    dbWriteTable(conn=connect, name="LayoutFormat", value=LayoutFormat, row.names=FALSE)
} )

# Remove the Excel objects
rm(pses2011_xls, pses2011_documentation)

# See table names
sqldf("SELECT * FROM sqlite_master", dbname = "PSES_database.sqlite")$tbl_name

# See table names and "DDL" statements
sqldf("SELECT * FROM sqlite_master", dbname = "PSES_database.sqlite")

# See names of columns in a particular table
# Note: PRAMA and TABLE_INFO are SQLite-specific statements
sqldf("PRAGMA TABLE_INFO(Questions)", dbname = "PSES_database.sqlite")$name

# Shortcut for seeing a list of tables and/or fields
# may not work on all systems 

dbListTables(connect)
dbListFields(connect, "Questions")  

# The equivalent of `head`
sqldf("SELECT * FROM Questions LIMIT 6", dbname = "PSES_database.sqlite")

# Close the connection
dbDisconnect(connect)

# List of the SQL statements available with SQLite
.SQL92Keywords


### Creating a dataframe from a SQLite database

require(sqldf)
connection = dbConnect(SQLite(), dbname="PSES_database.sqlite")

pses_acr = sqldf("SELECT Level1ID, Acronym_PSES_Acronyme_SAFF AS Acronym, DeptNameE as Department FROM Agency", dbname="PSES_database.sqlite")

pses2011_agency = sqldf("SELECT
    maintable.*
    , agencytable.DeptNameE as Agency
    , agencytable.Acronym_PSES_Acronyme_SAFF as Abbr
    FROM pses2011 maintable
    LEFT JOIN Agency agencytable
    ON maintable.LEVEL1ID = agencytable.Level1ID",
    dbname="PSES_db.sqlite")

agency_row_count = sqldf("SELECT
    Question
    , Agency
    , COUNT(ANSWER3) as Answer3_Count
    FROM pses2011_agency
    WHERE ANSWER3 > 50
    GROUP BY Question, Agency
    ORDER BY Answer3_Count desc")  

agency_row_count = sqldf("SELECT
    Question
    , SUM(CASE WHEN ANSWER1 > 51 THEN 1 ELSE 0 END) AS Strong_Agreement
    , SUM(CASE WHEN ANSWER5 > 51 THEN 1 ELSE 0 END) AS Strong_Disagreement
    FROM pses2011_agency
    GROUP BY Question, Agency
    ORDER BY Question desc")  

dbDisconnect(connection)


## Getting data from the web

### Working through a proxy

require(httr)

set_config(use_proxy(url="YOUR_PROXY_URL", port="YOUR_PORT", username="YOUR_USER_NAME", password="YOUR_PASSWORD"))

vignette("quickstart", package="httr")


### Scraping data from a web table

require(XML)

# Scrape the webpage
drug_use_2010 = readLines("http://archive.samhsa.gov/data/NSDUH/2k10nsduh/tabs/Sect1peTabs1to46.htm")

# Read the first HTML table on the page into a dataframe
drug_use_table1_1 = readHTMLTable(drug_use_2010, header=T, which=1, stringsAsFactors=FALSE)

# Read the 31st HTML table into a dataframe
drug_use_table1_17a = readHTMLTable(drug_use_2010, header=T, which=31, stringsAsFactors=FALSE)

# Read the 32nd HTML table into a dataframe
drug_use_table1_17b = readHTMLTable(drug_use_2010, header=T, which=32, stringsAsFactors=FALSE)


#### Scraping tables that cover multiple pages

allpages = paste("http://projects.propublica.org/nursing-homes/findings/search?page=", 1:524, "&search=&ss=ALL&state=TX", sep="")

tablelist = list()
for(i in seq_along(allpages)){
    page = allpages[i]
    page = readLines(page)
    homes = readHTMLTable(page, header=T, which=1, stringsAsFactors = FALSE)
    tablelist[[i]] = homes
}

nursing_home_deficiencies = do.call(rbind, lapply(tablelist, data.frame, stringsAsFactors=FALSE))


### Working with APIs

# http://www.census.gov/data/developers/about/terms-of-service.html

require(acs)
api.key.install(key="y0uR K3y H3rE")

tx_pops_acs = acs.fetch(geography=geo.make(state="TX", county="Harris"), table.number="B01003")

tx_county_pops = data.frame(tx_pops_acs@estimate)


## Creating fake data to test code

# Table A – customer data (Part 1)
set.seed(1235813)
customer_id = seq(1:10000)
customer_gender = sample(c("M","F", "Unknown"), 10000, replace=TRUE, prob=c(.45, .45, .10))
customer_age = round(runif(10000, 18, 88), 0)
make_NAs = which(customer_age %in% sample(customer_age, 25))
customer_age[make_NAs] = NA
customer_purchases = round(rlnorm(10000)*100, 2)

# Table B – city/state/zip and business density lookup table
download.file("ftp://ftp.census.gov/econ2012/CBP_CSV/zbp12totals.zip", "temp.zip", method="curl")
zipcodes = read.table(unz("temp.zip", "zbp12totals.txt"), header=T, quote="\"", sep=",", colClasses="character")
zipcodes = zipcodes[,c(1,11:12,10)]
# End of Table B

# Table A, continued (Part 2)
customer_zip = zipcodes[sample(1:nrow(zipcodes), 10000, replace=TRUE),]$zip
customers = data.frame(customer_id, customer_gender, customer_age, customer_purchases, customer_zip, stringsAsFactors=FALSE)

# TABLE C – results of a product interest survey
ones = seq(1, 1, length.out = 2000)
zeros = seq(0, 0, length.out = 2000)
strongly_agree = c(ones, zeros, zeros, zeros, zeros)
agree = c(zeros, ones, zeros, zeros, zeros)
neutral = c(zeros, zeros, ones, zeros, zeros)
disagree = c(zeros, zeros, zeros, ones, zeros)
strongly_disagree = c(zeros, zeros, zeros, zeros, ones)
survey = data.frame(customer_id, strongly_agree, agree, neutral, disagree, strongly_disagree)

# TABLE D – lookup table to match states to regions
state_regions = data.frame(datasets::state.abb, datasets::state.name, datasets::state.region, datasets::state.division, stringsAsFactors=FALSE)
colnames(state_regions) = c("abb", "state", "region", "division")


## Writing files to disk

# CSV
write.table(pses2011, "pses2011.csv", sep=",", row.names=FALSE)

# Split data frame into a list by a factor
pses2011_split = split(pses2011, pses2011$LEVEL1ID)

# Save each new data frame as an individual .csv file based on its name
lapply(1:length(pses2011_split), function(i) 
    write.csv(pses2011_split[[i]], 
        file = paste0("~/BIWR/", names(pses2011_split[i]), ".csv"), 
        row.names = FALSE))

# XML
kulife::write.xml(iris, file="iris.xml")

# JSON
iris_json = jsonlite::toJSON(iris, pretty=TRUE)
sink("iris_json.json")
iris_json
sink()


##### End of Chapter 2 #####



#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 3 Code: Cleaning and Preparing Data
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_3_Code.R
#
######################################################################

## Understanding your data

### Identifying data types and overall structure

# View the structure of the dataset, particularly data types
str(customers)

# Look at the first (or last) 6 lines of the data frame
head(customers)
tail(customers)

# Obtain basic summary stats
summary(customers)

# Look at the rows with missing data
missing_data = customers[!complete.cases(customers),]
missing_data


### Identifying (and ordering) factor levels

levels(customers$customer_gender)

customers$customer_gender = ordered(customers$customer_gender, c("Unknown", "F", "M"))

### Identifying unique values or duplicates

unique(customers$customer_age)

sort(unique(customers$customer_age))

customers[duplicated(customers),]

customers[10001,] = customers[2718,]

customers[duplicated(customers),]

customers = unique(customers)


### Sorting 

require(dplyr)

# Look at top 10 customers by purchases (descending)
head(arrange(customers, desc(customer_purchases)), 10)

# Look at top 10 customers by age (ascending) and purchases (descending)
head(arrange(customers, customer_age, desc(customer_purchases)), 10)

# NAs are placed last
tail(arrange(customers, customer_age, desc(customer_purchases)), 10)

# You can store the sorted data frame, of course
customers = arrange(customers, customer_age, desc(customer_purchases))


## Cleaning up

### Cleaning up a web-scraped table: Regular expressions and more

# Create cleaner column names
colnames(nursing_home_deficiencies) = c("Date", "Home", "City", "State", "Deficiency_Count", "Severity")

# Replace abbreviations and extraneous text
nursing_home_deficiencies$State = gsub("Tex.", "TX", nursing_home_deficiencies$State, fixed = TRUE)
nursing_home_deficiencies$Home = gsub(" (REPORT)  Home Info", "", nursing_home_deficiencies$Home, fixed = TRUE)

# Take the first letter (severity score) and remove the 
# duplicate and extraneous text
nursing_home_deficiencies$Severity = substr(nursing_home_deficiencies$Severity, 1, 1)

# Clean the date information and convert to date objcet
nursing_home_deficiencies$Date = gsub(".", "", nursing_home_deficiencies$Date, fixed = TRUE)

clean = function(col) {
    col = gsub('Jan', 'January', col, fixed = TRUE)
    col = gsub('Feb', 'February', col, fixed = TRUE)
    col = gsub('Aug', 'August', col, fixed = TRUE)
    col = gsub('Sept', 'September', col, fixed = TRUE)
    col = gsub('Oct', 'October', col, fixed = TRUE)
    col = gsub('Nov', 'November', col, fixed = TRUE)
    col = gsub('Dec', 'December', col, fixed = TRUE)
    return(col)
}

nursing_home_deficiencies$Date = clean(nursing_home_deficiencies$Date) 

nursing_home_deficiencies$Date = as.Date(nursing_home_deficiencies$Date, format="%B %d, %Y")

# colnames(my_data)[column_number] = "new_column_name"


### Missing data: Converting dummy NA values

# This will work 
# my_data$my_variable = as.numeric(gsub("9999", NA, my_data$my_variable))

# But this is better, albeit less transparent
# my_data$my_variable[my_data$my_variable==9999] = NA 

# And this will do it for the entire data frame
# my_data[my_data == 9999] = NA

# This won't work
mean(customers$customer_age)

# This will
mean(customers$customer_age, na.rm=TRUE)


### Converting between data types

# Character / String | as.character(OBJECT, ...)
# Factor / Category | as.factor(OBJECT, ...)
# Numeric / Double | as.numeric(OBJECT, ...)
# Integer | as.integer(OBJECT, ...)
# Date | as.Date(OBJECT, format="yyyy-mm-dd", ...)
# Datetime | as.POSIXct(OBJECT, tz="CURRENT TIME ZONE", ...)

customers_numeric = customers[sapply(customers,is.numeric)]


### Rounding

round(mean(customers$customer_age, na.rm=TRUE), 1)

signif(mean(customers$customer_age, na.rm=TRUE), 1)

signif(mean(customers$customer_age, na.rm=TRUE), 3)


### Concatenation

# my_data$date = as.Date(paste(mydata$yyyy_mm, "01", sep="-"))

# my_data$date = as.Date(paste0(mydata$yyyy_mm, "-01"))

plot(customers$customer_age, customers$customer_purchase, main=paste("Age vs Purchase Amount for", length(customers$customer_age), "Customers"))

# my_data$my_variable = as.numeric(paste(gsub(",", "", my_data$my_variable)))


## Merging dataframes

### Joins

require(dplyr)
require(sqldf)

customer_locations = left_join(customers, zipcodes, by=c("customer_zip" = "zip"))

customer_regions = left_join(customer_locations, state_regions, by=c("stabbr" = "abb"))

customer_survey = left_join(customer_regions, survey, by=c("customer_id" = "customer_id"))

customer_locations = merge(customers, zipcodes, by.x = "customer_zip", by.y = "zip", all.x = TRUE)

customer_regions = merge(customer_locations, state_regions, by.x = "stabbr", by.y = "abb", all.x = TRUE)

customer_survey = merge(customer_regions, survey, by.x="customer_id", by.y="customer_id", all.x=TRUE)

customer_survey = sqldf("
    SELECT
    customers.*
    , zipcodes.city
    , zipcodes.stabbr
    , state_regions.state
    , state_regions.region
    , state_regions.division
    , survey.strongly_agree
    , survey.agree
    , survey.neutral
    , survey.disagree
    , survey.strongly_disagree
    FROM customers
    LEFT JOIN zipcodes
        ON customers.customer_zip = zipcodes.zip
    LEFT JOIN state_regions
        ON zipcodes.stabbr = state_regions.abb
    LEFT JOIN survey
        ON customers.customer_id = survey.customer_id"
    , stringsAsFactors=FALSE)


# Left | left_join(A, B, by=c("A.key" ="B.key")) | merge(A, B, by.x="A.key", by.y="B.key", all.x=TRUE) | SELECT * FROM A LEFT JOIN B ON A.key = B.key 
# Inner | inner_join(A, B, by=c("A.key" ="B.key")) | merge(A, B, by.x="A.key", by.y="B.key", all=FALSE) | SELECT * FROM A INNER JOIN B ON A.key = B.key
# Full (Outer) | merge(A, B, by.x="A.key", by.y="B.key", all=TRUE)
# Semi | semi_join(A, B, by=c("A.key" ="B.key")) | SELECT * FROM A LEFT JOIN B ON A.key = B.key WHERE B.key iS NULL | 
# Anti | anti_join(A, B, by=c("A.key" ="B.key"))
# Right | left_join(B, A, by=c("B.key" ="A.key")) | merge(A, B, by.x="A.key", by.y="B.key", all.y=TRUE) | SELECT * FROM B LEFT JOIN A ON B.key = A.key
# Right Anti | semi_join(B, A, by=c("B.key" ="A.key")) | SELECT * FROM B LEFT JOIN A ON B.key = A.key WHERE A.key iS NULL

# Using data.tables | datatable_AB = datatable_A[datatable_B] 


### Unions and bindings

customers_first_half = customer_survey[1:5000,]

customers_second_half = customer_survey[5001:10000,]

customers_whole = sqldf(
    "SELECT * FROM customers_first_half
    UNION
    SELECT * FROM customers_second_half")

customers_whole = rbind(customers_first_half, customers_second_half)

# Create a data frame with the original data
# and space for the forecast details
use_df = data.frame(Total_Use = power_monthly$Total_Use_kWh, Forecast = NA, Upper_80 = NA, Lower_80 = NA, Upper_95 = NA, Lower_95 = NA)

# Create a data frame for the forecast details
# with a column for the original data
use_fc = data.frame(Total_Use = NA, Forecast = total_use_fc$mean, Upper_80 = total_use_fc$upper[,1], Lower_80 = total_use_fc$lower[,1], Upper_95 = total_use_fc$upper[,2], Lower_95 = total_use_fc$lower[,2])

# "Union" the two data frames into one
use_ts_fc = rbind(use_df, use_fc)

customers_left_half = customer_survey[,1:7]
customers_right_half = customer_survey[,8:14]

customers_back_together = cbind(customers_left_half, customers_right_half)
customers_back_together = data.frame(customers_left_half, customers_right_half)

# Keep the ID in the second table
customers_right_half = customer_survey[,c(1,8:14)]

# Use sqldf to make a natural join
customers_back_together = sqldf("
    SELECT *
    FROM customers_left_half A
    JOIN customers_right_half B
        ON A.customer_id = B.customer_id")

# Union | rbind(A, B) | SELECT * FROM A UNION SELECT * FROM B
# Concatenate | data.frame(A, B) | SELECT * FROM A JOIN B ON A.key = B.key | cbind(A, B)


## Subsetting: filtering and selecting from a dataframe

require(dplyr)

# Subset the data to only those customers who agree or strongly agree 
# with the survey question
customers_marketing = filter(customer_survey, strongly_agree == 1 | agree == 1)

# Subset the data to customers who are Californians over age 64 who have spent 
# at least $200 OR Oregonians of any age who have spent more than $500
customers_CA_65_200_OR_500 = filter(customer_survey, state == "California" & customer_age >= 65 & customer_purchases >= 200 | state == "Oregon" & customer_purchases > 500)

customers_in_lake_cities = filter(customer_survey, grepl('lake', tolower(city)))

# Subset the data to only columns with names that start with "customer"
customers_only = select(customer_survey, starts_with("customer"))

# Subset the data to only ID and columns with names that contain "agree"
customers_only_agreement = select(customer_survey, customer_id, matches("agree"))

# Subset the data to only columns with names that DO NOT start with "customer"
customers_excluded = select(customer_survey, -starts_with("customer"))

vignette("introduction", package="dplyr")

# http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


## Creating a derived column

customer_survey$Location = paste(customer_survey$city, customer_survey$stabbr, sep=", ")
# vs
customer_survey = mutate(customer_survey, Location=paste(city, stabbr, sep=", "))

customer_survey$proportion = customer_survey$customer_purchases / sum(customer_survey$customer_purchases)
# vs
customer_survey = mutate(customer_survey, proportion2 = customer_purchases / sum(customer_purchases))


## Peeking at the outcome with dplyr

head(select(customer_survey, contains("customer", ignore.case=TRUE)))

head(arrange(customer_survey, region, desc(customer_age), desc(customer_purchases)))

head(mutate(customer_survey, Location = paste(city, state, sep=", "), purchase_proportion=customer_purchases/sum(customer_purchases)))


## Reshaping a dataframe between wide and long

### Reshaping: melt and cast

require(reshape2)

surgery_outcomes = read.table("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/surgery_outcomes.tsv", header=T, sep="\t")

head(surgery_outcomes, 4)

surgery_outcomes_melted = melt(surgery_outcomes, id.vars=c(1:4), measure.vars=c(5:6), variable.name="Test_type", value.name="IQ_score")

head(surgery_outcomes_melted, 4)

surgery_outcomes_original = dcast(surgery_outcomes_melted, ID + Side + Phase ~ Test_type,  value.var="IQ_score")

head(surgery_outcomes_original, 4)


### Reshaping: Crossing variables with ~ and +

head(dcast(surgery_outcomes_melted, ID ~ Test_type + Phase, value.var='IQ_score'), 4)

head(dcast(surgery_outcomes_melted, ID + Side ~ Test_type + Phase, value.var='IQ_score'), 4)

head(dcast(surgery_outcomes_melted, ID + Phase ~ Test_type, value.var='IQ_score'), 4)


### Summarizing while reshaping

dcast(surgery_outcomes_melted, Phase ~ Test_type, value.var='IQ_score', mean, na.rm=T)

dcast(surgery_outcomes_melted, Side ~ Test_type + Phase, value.var='IQ_score', mean, na.rm=T)

dcast(surgery_outcomes_melted, Test_type + Side + Phase ~ ., value.var='IQ_score', sd, na.rm=T)


## Piping with %>%: Stringing it together in dplyr

# For customers who've purchased more than $500, get count, mean, 
# sd, and sum for each state, and calculate the coeffient of
# variation, rounded to 2 decimal places
customer_500_piped = customer_survey %>%  
    filter(customer_purchases >= 500) %>%   
    group_by(state) %>%  
    summarize(count_purchases = n(),  
        mean_purchases = mean(customer_purchases, na.rm=T),  
        sd_purchases = sd(customer_purchases, na.rm=T),  
        sum_purchases = sum(customer_purchases)) %>%
    mutate(cv_purchases = round(sd_purchases / mean_purchases, 2))  

head(customer_500_piped, 4)


##### End of Chapter 3 #####



#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 4 Code: Know Thy Data (EDA)
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_4_Code.R
#
######################################################################

require(ggplot2)
require(scales)

# Useful ggplot2 websites
# http://www.rstudio.com/wp-content/uploads/2015/12/ggplot2-cheatsheet-2.0.pdf
# http://www.cookbook-r.com/Graphs/
# http://www.computerworld.com/article/2935394/business-intelligence/my-ggplot2-cheat-sheet-search-by-task.html
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/


# Acquire the bike share data and set factor levels
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", "Bike-Sharing-Dataset.zip")

bike_share_daily = read.table(unz("Bike-Sharing-Dataset.zip", "day.csv"), colClasses=c("character", "Date", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric", "integer", "integer", "integer"), sep=",", header=TRUE)

levels(bike_share_daily$season) = c("Winter", "Spring", "Summer", "Fall")

levels(bike_share_daily$workingday) = c("No", "Yes")

levels(bike_share_daily$holiday) = c("No", "Yes")

bike_share_daily$mnth = ordered(bike_share_daily$mnth, 1:12)

levels(bike_share_daily$mnth) = c(month.abb)

levels(bike_share_daily$yr) = c(2011, 2012)


## Creating summary plots

### Everything at once: ggpairs

require(GGally)

ggpairs(data=bike_share_daily, columns=c(14:15, 10, 13, 3, 7), title="Daily Bike Sharing Data", axisLabels="show", mapping=aes(color = season, alpha=0.3))


### Create histograms of all numeric variables in one plot

require(psych)

multi.hist(bike_share_daily[,sapply(bike_share_daily, is.numeric)])


### A better "pairs" plot

pairs.panels(bike_share_daily[,sapply(bike_share_daily, is.numeric)], ellipses=FALSE, pch=".", las=2, cex.axis=0.7, method="kendall")


### Mosaic plots: "Scatterplots" for categorical data

require(vcd)

pairs(Titanic, highlighting=2)


## Plotting univariate distributions

### Histograms and density plots

ggplot(bike_share_daily, aes(casual)) +
    geom_density(col="blue", fill="blue", alpha=0.3) +
    xlab("Casual Use") +
    theme_bw()

ggplot(bike_share_daily, aes(casual)) +
    geom_histogram(col="blue", fill="blue", alpha=0.3) +
    xlab("Casual Use") +
    theme_bw()

ggplot(bike_share_daily, aes(casual)) +
    ylab("density and count") +
    xlab("Casual Use") +
    geom_histogram(aes(y=..density..), col="blue", fill="blue", alpha=0.3) +
    geom_density(col="blue", fill="blue", alpha=0.2) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


### Bar and dot plots

ggplot(bike_share_daily, aes(weathersit)) +
    geom_bar(col="blue", fill="blue", alpha=0.3) +
    xlab("Weather Pattern") +
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", "Stormy")) +
    theme_bw()

ggplot(bike_share_daily, aes(x=weathersit, y=..count..)) +
    geom_bar(stat="count", width=0.01) +
    geom_point(stat = "count", size=4, pch=21, fill="darkblue") +
    xlab("Weather Pattern") +
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", "Stormy")) +
    coord_flip() +
    theme_bw()


### Plotting multiple univariate distributions with faceting

ggplot(bike_share_daily, aes(casual, fill=season)) +
    geom_histogram(aes(y = ..density..), alpha=0.2, color="gray50") +
    geom_density(alpha=0.5, size=0.5) +
    facet_wrap(~season) +
    theme_light() +
    xlab("Daily Bike Use Count") +
    ylab("") +
    theme(legend.position="none") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          axis.title = element_text(size=9, face=2, color="gray30"),
          axis.title.x = element_text(vjust=-0.5))

ggplot(bike_share_daily, aes(weathersit, fill=season)) +
    geom_bar(alpha=0.5) +
    xlab("") +
    ylab("Number of Days") +
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", "Stormy")) +
    coord_flip() +
    facet_wrap(~season, ncol=1) +
    theme_light()


## Plotting bivariate and comparative distributions

require(ggExtra)
require(vcd)
require(beanplot)


### Double density plots

ggplot(bike_share_daily, aes(casual, fill=workingday, color=workingday)) +
    geom_density(alpha=0.4) +
    theme_minimal() +
    xlab("Daily Casual Bike Use Count") +
    ylab("") +
    scale_fill_discrete(name="Work Day?") +
    scale_color_discrete(name="Work Day?") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
          legend.position="top")


### Boxplots

ggplot(bike_share_daily, aes(mnth, casual, fill=workingday)) +
    xlab("Month") +
    ylab("Daily Casual Bike Use Count") +
    geom_boxplot() +
    theme_minimal() +
    scale_fill_discrete(name="Work Day?") +
    scale_color_discrete(name="Work Day?") +
    theme(legend.position="bottom")


### Beanplots

beanplot(casual ~ mnth, data = bike_share_daily, side="first", overallline="median", what=c(1,1,1,0), col=c("gray70", "transparent", "transparent", "blue"), xlab = "Month", ylab = "Daily Casual Bike Use Count")

beanplot(casual ~ mnth, data = bike_share_daily, side="second", overallline="median", what=c(1,1,1,0), col=c("gray70", "transparent", "transparent", "blue"), ylab = "Month", xlab = "Daily Casual Bike Use Count", horizontal=TRUE)


### Scatterplots and marginal distributions

bike_air_temp = ggplot(bike_share_daily, aes(x=atemp, y=casual)) +
    xlab("Daily Mean Normalized Air Temperature") +
    ylab("Number of Total Casual Bike Uses") +
    geom_point(col="gray50") +
    theme_bw()

bike_air_temp_mh = ggMarginal(bike_air_temp, type="histogram")

bike_air_temp_mh

bike_air_temp_md = ggMarginal(bike_air_temp, type="density")

bike_air_temp_md

bike_air_temp_mb = ggMarginal(bike_air_temp, type="boxplot")

bike_air_temp_mb


### Mosaic plots

mosaic(~ weathersit + season, data=bike_share_daily, shade=T, legend=F, labeling_args = list(set_varnames = c(season = "Season", weathersit = "Primary Weather Pattern"), set_labels = list(weathersit = c("Clear", "Cloudy/Rainy",  "Stormy"))))


### Multiple bivariate comparisons with faceting

ggplot(bike_share_daily, aes(casual, fill=workingday, color=workingday)) +
    geom_density(alpha=0.4) +
    theme_minimal() +
    xlab("Daily Casual Bike Use Count") +
    ylab("") +
    scale_fill_discrete(name="Work Day?") +
    scale_color_discrete(name="Work Day?") +
    facet_wrap(~season, ncol=2) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
          legend.position="top")


## Pareto charts

library(qcc)

Readmits = c(148, 685, 16, 679, 192, 8, 1601, 37, 269, 48)
Dx = c('Septicemia', 'Cancer', 'Diabetes', 'Heart disease', 'Stroke', 'Aortic aneurysm', 'Pneumonia', 'Chronic liver disease', 'Nephritis/nephrosis', 'Injury/poisoning') 

pareto.chart(xtabs(Readmits ~ Dx), main='Pareto Chart for Unplanned Readmissions')


## Plotting survey data

require(likert)

# Create a likert object
mathiness = likert(mass[2:15])

# Plot the likert object
plot(mathiness)

# Create likert object with a grouping factor
gender_math = likert(items=mass[,c(4,6,15), drop=FALSE], grouping=mass$Gender)

# Grouped plot
plot(gender_math, include.histogram=TRUE)


## Obtaining summary and conditional statistics

require(psych)

describe(bike_share_daily[10:16])

describeBy(bike_share_daily[10:16], bike_share_daily$holiday)

table(bike_share_daily$holiday)

prop.table(table(bike_share_daily$holiday))

describeBy(bike_share_daily[14:16], bike_share_daily$season == "Winter")

describeBy(bike_share_daily[10:13], bike_share_daily$casual <= 1000)

describeBy(bike_share_daily$casual, bike_share_daily$windspeed > mean(bike_share_daily$windspeed))

sum(bike_share_daily$cnt > 500, na.rm=T)

sum(bike_share_daily$workingday == 'Yes')

table(bike_share_daily[c(3,6:7)]) 

addmargins(table(bike_share_daily[c(3,6:7)]))

prop.table(table(bike_share_daily[c(3,9)]))

require(dplyr)
summarize(group_by(bike_share_daily, season, holiday, weekday), count=n())


## Finding local maxima/minima
bike_share_daily[which.min(bike_share_daily[,14]),]

# Calculate density of casual bike use
casual_dens = data.frame(casual = density(bike_share_daily$casual)$x, density_value = density(bike_share_daily$casual)$y)

# Mode / maximum density
casual_dens[which.max(casual_dens[,2]),]

# Plot histogram and density with mode as a line
ggplot(bike_share_daily, aes(casual)) +
    ylab("density and count") +
    xlab("Casual Use") +
    geom_histogram(aes(y=..density..), col="blue", fill="blue", alpha=0.3) +
    geom_density(col="blue", fill="blue", alpha=0.2) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_vline(xintercept = dens[which.max(dens[,2]),1], color = "yellow")


## Inference on summary statistics

### Confidence intervals

# Median | asbio::ci.median(x) 
# Mean | t.test(x)$conf.int 
# Proportion | binom.test(x, n)$conf.int
# Count | poisson.test(x)$conf.int 
# Rate | poisson.test(x, n)$conf.int

# 95% CI for median daily casual bike use
asbio::ci.median(bike_share_daily$casual) 

# 95% CI for mean daily casual bike use
t.test(bike_share_daily$casual)$conf.int

# 95% CI for proportion of casual bike use of all rentals
binom.test(sum(bike_share_daily$casual), sum(bike_share_daily$cnt))$conf.int

# Subset data to winter only
bike_share_winter = filter(bike_share_daily, season == "Winter")

# 95% CI for count of winter casual bike use
poisson.test(sum(bike_share_winter$casual))$conf.int

# 95% CI for count of winter casual bike use per 1000 rentals
poisson.test(sum(bike_share_winter$casual), sum(bike_share_winter$cnt)/1000)$conf.int


## Bootstrapping

require(boot)

# Create the boot function
sd_boot_function = function(x,i){sd(x[i])}

# Run the bootstrapping
sd_boot = boot(PlantGrowth$weight, sd_boot_function, R=10000)

# Bootstrapped sd
sd(PlantGrowth$weight)

# 95% CI for bootstrapped sd 
boot.ci(sd_boot, type="bca")$bca[4:5]

# Example for 75th percentile
q75_function = function(x,i){quantile(x[i], probs=0.75)}

q75_boot = boot(PlantGrowth$weight, q75_function, R=10000)

quantile(PlantGrowth$weight, 0.75)

boot.ci(q75_boot, type="bca")$bca[4:5]

# Mean | Bootstrapped CI | (fun.data = mean_cl_boot, fun.args = list(conf.int = 0.95), ...)
# Mean | Normal CI | (fun.data = mean_cl_normal, fun.args = list(conf.int = 0.95), ...)
# Mean | Standard deviation | (fun.data = mean_sdl, fun.args = list(mult = 2), ...)
# Median | Quantile | (fun.data = median_hilow, fun.args = list(conf.int = 0.5), ...)

require(gridExtra)

p1 = ggplot(PlantGrowth, aes(group, weight)) +
    ggtitle("Bootstrapped") +
    stat_summary(fun.data = mean_cl_boot, fun.args=list(conf.int = 0.95)) 

p2 = ggplot(PlantGrowth, aes(group, weight)) +
    ggtitle("Normal") +
    stat_summary(fun.data = mean_cl_normal, fun.args=list(conf.int = 0.95)) 

p3 = ggplot(PlantGrowth, aes(group, weight)) +
    ggtitle("2 SDs") +
    stat_summary(fun.data = mean_sdl, fun.args=list(mult=2)) 

p4 = ggplot(PlantGrowth, aes(group, weight)) +
    ggtitle("Median+IQR") +
    stat_summary(fun.data = median_hilow, fun.args=list(conf.int = 0.5)) 

grid.arrange(p1, p2, p3, p4, nrow=2)


### Tolerance intervals

require(tolerance)

commute_time = c(68, 42, 40, 69, 46, 37, 68, 68, 69, 38, 51, 36, 50, 37, 41, 68, 59, 65, 67, 42, 67, 62, 48, 52, 52, 44, 65, 65, 46, 67, 62, 66, 43, 58, 45, 65, 60, 55, 48, 46)

commute_time_npti = nptol.int(commute_time, alpha=0.05, P=0.75, side=2)

commute_time_npti

plottol(commute_time_npti, commute_time, side="two", plot.type="both")

par(mfrow=c(1,1))

# Percent | bintol.int(x, n, m, …)
# Count or Rate | poistol.int(x, n, m, side, …)
# Nonparametric | nptol.int(x, …)
# Continuous | normtol.int(x, side, …)
# Continuous | uniftol.int(x, …) 
# Lifetime/survival | exptol.int(x, type.2, …) 
# Score | laptol.int(x, …) 
# Indicies | gamtol.int(x, …) 
# Reliability, extreme values | extol.int(x, dist, …) 


## Dealing with missing data

### Visualizing missing data

require(VIM)

data(tao)

# Rename the Sea.Surface.Temp column to make label fit on plot
colnames(tao)[4] = "Sea.Temp"

# Look at the data, esp. NAs
summary(tao)

# Plot missings (you can ignore the error message)
matrixplot(tao)

# Missing value summary values and plot
tao_aggr = aggr(tao)

tao_aggr

# Two-variable missingness comparisons
histMiss(tao[5:6])

histMiss(tao[c(6,5)])

marginplot(tao[5:6])

marginmatrix(tao[4:6])


### Imputation for missing values

# Perform k Nearest Neighbors imputation
# Result is new dataframe with imputed values
tao_knn = kNN(tao)

marginplot(tao_knn[c(5:6, 13:14)], delimiter="_imp")

marginmatrix(tao_knn[c(4:6, 12:14)], delimiter="_imp")

# Perform standard Iterative Robust Model-based Imputation
tao_irmi = irmi(tao)

# Perform robust Iterative Robust Model-based Imputation
tao_irmi_robust = irmi(tao, robust=TRUE)

# Create a mean-imputed air temp variable
tao$tao_airtemp_mean = ifelse(is.na(tao$Air.Temp), mean(tao$Air.Temp, na.rm=TRUE), tao$Air.Temp)

# Make a data frame of each air temp result
tao_compare_airtemp = data.frame(tao=tao[,5], tao_knn=tao_knn[,5], tao_irmi=tao_irmi[,5], tao_irmi_robust=tao_irmi_robust[,5], mean=tao[,9])

# Melt the various air temp results into a long data frame
require(reshape2)
tao_compare_melt = melt(tao_compare_airtemp, value.name="Air.Temp")

# Plot density histograms of each option and 
# add black dotted line to emphasize the original data
ggplot(tao_compare_melt, aes(Air.Temp, color=variable)) +
    geom_density(lwd=1.25) + 
    geom_density(data=subset(tao_compare_melt, variable=="tao"), 
        aes(Air.Temp), lty=3, lwd=1.5, color="black") +
    theme_minimal()


# More imputation in R: other packages
# http://cran.r-project.org/web/views/OfficialStatistics.html


##### End of Chapter 4 #####



#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 5 Code: Effect Sizes
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_5_Code.R
#
######################################################################


# install JAGS from http://mcmc-jags.sourceforge.net/ 

# install.packages(rjags)
# devtools::install_github("rasmusab/bayesian_first_aid")


## Overview

### Differences in tendency 

# Difference in means 
# t.test(...)$estimate
# t.test(...)$conf.int 
# bootES
# bootES with effect.type="unstandardized" 
# BayesianFirstAid
# bayes.t.test  

# Difference in medians
# simpleboottwo.boot(..., median)$t0  
# boot.ci of two.boot object 

# Difference in quantiles
# simpleboot
# two.boot(..., quantile, probs=0.75)$t0
# boot.ci of two.boot object 

# Standardized mean difference
# Cohen's *d*
# bootES
# bootES with effect.type="cohens.d"
# robust Cohen's *d*
# bootES
# bootES with effect.type="akp.robust.d"
# Hedge's *g*
# bootES
# bootES with effect.type="hedges.g"

# Difference between proportions
# prop.test(...)$estimate
# prop.test(...)$conf.int 
# BayesianFirstAid
# bayes.prop.test

# Difference between counts or rates
# poisson.test(...)$estimate
# poisson.test(...)$conf.int
# BayesianFirstAid
# bayes.prop.test

# Standardized group differences
# Cliff's Delta
# orrdom
# dmes.boot with theta.es="dc"
# Vargha-Delaney's *A*
# orrdom
# dmes.boot with theta.es="Ac"


### Differences in variability

# Variance
# var.test(...)$estimate
# var(...).test$conf.int

# Difference between variances
# asympTestasymp.test(...)$estimate
# asymp(...).test$conf.int with parameter="dVar"


### Relationships and Associations

# Correlation
# Pearson's *r*
# cor 
# cor.test(...)$conf.int 
# BayesianFirstAid
# bayes.cor.test
# bootES
# bootES with effect.type="r"
# Spearman's rho
# pysch
# cor.ci with method="spearman"
# boot (function in recipe below)
# Kendall's tau
# pysch
# cor.ci with method="kendall"
# boot (function in recipe below)

# Partial correlation
# psych
# corr.test()
# partial.r()
# Polychoric correlation 
# psych
# polychoric()
# Polyserial correlation 
# psych
# polyserial()

# Odds ratio
# psych
# oddsratio()
# Standardized odds ratio / Yule's Q
# psych
# Yule()

# Comparisons of agreement
# Cohen's kappa
# psych
# cohen.kappa()

# Regression coefficient
# lm()
# confint


## Effect sizes: Measuring *differences* between groups

# Load libraries
require(simpleboot)
require(bootES)
require(orddom)
require(asympTest)
require(BayesianFirstAid)
require(reshape2)
require(dplyr)

# Reshape the data
casual_workingday_use = dcast(bike_share_daily, yr~workingday, value.var="casual", sum)

casual_workingday_use$sum = casual_workingday_use$Yes + casual_workingday_use$No

# Filter the data into subsets
casual_notworkingday = filter(bike_share_daily, workingday == "No" & season == "Spring" | workingday == "No" & season == "Fall")

casual_notworking_Spring = filter(casual_notworkingday, season == "Spring")

casual_notworking_Fall = filter(casual_notworkingday, season == "Fall")


### Basic differences

workday_diff = prop.test(casual_workingday_use$Yes, casual_workingday_use$sum)

round(workday_diff$estimate[1] - workday_diff$estimate[2], 2)

round(workday_diff$conf.int, 2)

casual_notworkingday_mean = t.test(casual~season, data=casual_notworkingday)

abs(casual_notworkingday_mean$estimate[1] - casual_notworkingday_mean$estimate[2])

casual_notworkingday_mean$conf.int

bootES(casual_notworkingday, data.col="casual", group.col="season", contrast=c("Fall", "Spring"), effect.type="unstandardized")

diff_medians = two.boot(casual_notworking_Spring$casual, casual_notworking_Fall$casual, median, R=2000)

diff_medians_ci = boot.ci(diff_medians, conf=0.95, type='bca')

diff_medians$t0

diff_medians_ci

diff_75 = two.boot(casual_notworking_Spring$casual, casual_notworking_Fall$casual, quantile, probs=0.75, R=2000)

diff_75_ci = boot.ci(diff_medians, conf=0.95, type='bca')

diff_75$t0

diff_75_ci

median_diff = wilcox.test(casual~season, data=casual_notworkingday, conf.int=TRUE)

median_diff$estimate

median_diff$conf.int

var.test(casual_notworkingday$casual ~ casual_notworkingday$season)$estimate

var.test(casual_notworkingday$casual ~ casual_notworkingday$season)$conf.int

asymp.test(casual_notworkingday$casual ~ casual_notworkingday$season, parameter = "dVar")$estimate

asymp.test(casual_notworkingday$casual ~ casual_notworkingday$season, parameter = "dVar")$conf.int


### Standardized differences

bootES(casual_notworkingday, data.col="casual", group.col="season", contrast=c("Fall", "Spring"), effect.type="hedges.g")

bootES(casual_notworkingday, data.col="casual", group.col="season", contrast=c("Fall", "Spring"), effect.type="akp.robust.d")

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual, theta.es="dc")$theta

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual, theta.es="dc")$theta.bci.lo

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual, theta.es="dc")$theta.bci.up

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual, theta.es="Ac")$theta

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual, theta.es="Ac")$theta.bci.lo

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual, theta.es="Ac")$theta.bci.up

delta_gr(casual_notworking_Fall$casual, casual_notworking_Spring$casual, x.name="Fall", y.name="Spring")


### Determining the probability of a difference

workday_diff_bayes = bayes.prop.test(casual_workingday_use$Yes, casual_workingday_use$sum)

workday_diff_bayes

plot(workday_diff_bayes)

casual_notworkingday_mean_bayes = bayes.t.test(casual~season, data=casual_notworkingday)

casual_notworkingday_mean_bayes

plot(casual_notworkingday_mean_bayes)

summary(casual_notworkingday_mean_bayes)

diagnostics(casual_notworkingday_mean_bayes)


## Effect sizes: Measuring *similarities* between groups

### Associations between numeric variables (correlation)

require(psych)
require(bootES)

# Use count and air temp variables from bike share data
bike_use_atemp = data.frame(air_temp = bike_share_daily$atemp, count = bike_share_daily$cnt)

# Pearson's
cor(bike_use_atemp$air_temp, bike_use_atemp$count)

# Normal CI for Pearson's
cor.test(bike_use_atemp$air_temp, bike_use_atemp$count)$conf.int

# Bootstrapped CI for Pearson's
bootES(c(bike_use_atemp$air_temp, bike_use_atemp$count), effect.type="r")

# Prefer Kendall's to Spearman's in most cases
# Kendall's with CI
cor.ci(bike_use_atemp, method="kendall", n.iter = 10000, plot=FALSE)

# Spearman's with CI
cor.ci(bike_use_atemp, method="spearman", n.iter = 10000, plot=FALSE)


### Bootstrapping BCa CIs for non-parametric correlation

# Boot function for Kendall's 
rt_function = function(x,i){cor(x[i,1], x[i,2], method="kendall")}

# Run Kendall's boot function
rt_boot = boot(bike_use_atemp, rt_function, R=10000)

# Kendall's BCa CI
boot.ci(rt_boot, type="bca")$bca[4:5]

# Boot function for Spearman's
rs_function = function(x,i){cor(x[i,1], x[i,2], method="spearman")}

rs_boot = boot(bike_use_atemp, rs_function, R=10000)

boot.ci(rs_boot, type="bca")$bca[4:5]


### Determining the probability of a correlation

require(BayesianFirstAid)

atemp_bike_cor_bayes = bayes.cor.test(bike_use_atemp$air_temp, bike_use_atemp$count)

atemp_bike_cor_bayes

plot(atemp_bike_cor_bayes)


### Partial correlations

# Subset bike share data data
bike_use_atemp_wind = data.frame(temp = bike_share_daily$temp, cnt = bike_share_daily$cnt, windspeed = bike_share_daily$windspeed )

# Acquire correlation matrix  
atemp_wind_count = corr.test(bike_use_atemp_wind, method="kendall")

# Review matrix
atemp_wind_count$r

# Obtain partial r
partial.r(as.matrix(atemp_wind_count$r), c(1:2), 3)


### Polychoric and polyserial correlation for ordinal data

## Polychoric

# Get math attitudes data
data(mass, package="likert")

# Subset and convert to numeric
poly_math = data.frame(as.numeric(mass[,7]), as.numeric(mass[,14]))

# Name columns
colnames(poly_math) = c("worry", "enjoy")

# Obtain polychoric correlation
polychoric(poly_math)

## Polyserial

# Made up math scores
math_score = c(755, 642, 626, 671, 578, 539, 769, 614, 550, 615, 749, 676, 753, 509, 798, 783, 508, 767, 738, 660)

# Obtain polyserial correlation using the polycor package
polycor::polyserial(math_score, poly_math$enjoy)


### Associations between categorical variables

require(epitools)
require(psych)
data(Aspirin, package="abd")

# Obtain the odds ratio and CI
oddsratio(table(Aspirin))$measure

# Obtain Yule's Q
Yule(table(Aspirin))


### Cohen’s kappa for comparisons of agreement

# Doctor ratings
doctor = c("yes", "no", "yes", "unsure", "yes", "no", "unsure", "no", "no", "yes", "no", "yes", "yes")
# Model ratings
model = c("yes", "yes", "unsure", "yes", "no", "no", "unsure", "no", "unsure", "no", "yes", "yes", "unsure")

# Obtain Cohen's kappa
cohen.kappa(x=cbind(doctor, model))

# Category proportion of agreement
cohen.kappa(x=cbind(doctor, model))$agree


### Regression coefficient

data(tao, package="VIM")

# run the linear model
effect_air_on_sea = lm(Sea.Surface.Temp ~ Air.Temp, data=tao)

# review model coefficients
effect_air_on_sea


# get 95% confidence interval
confint(effect_air_on_sea)


### R^2: Proportion of variance explained 

library(boot)

# R-squared boot function
rsq = function(formula, data, indices) {
    d = data[indices,] # allows boot to select sample
    fit = lm(formula, data=d)
    return(summary(fit)$r.square)
}

# bootstrap R^2 with 10k replications
air_temp_R2 = boot(data=tao, statistic=rsq, R=10000, formula=Sea.Surface.Temp ~ Air.Temp)

# view bootstrapped R2 results
air_temp_R2

# get 95% confidence interval
boot.ci(air_temp_R2, type="bca")

# plot results
plot(air_temp_R2)

# Example of getting relatively high R^2 from pure noise
set.seed(123)
y = rnorm(10)
x = sapply(rep(10,8), rnorm)
noise = lm(y ~ x)
summary(noise)$r.squared

##### End of Chapter 5 #####



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
require(mgcv)

### Smoothed trends

ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
    xlab("Daily Mean Normalized Air Temperature") +
    ylab("Number of Total Bike Uses") +
    geom_point(col="gray50") +
    geom_smooth(method="loess") +
    theme_bw()

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


## Plotting regression results

require(dotwhisker)

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

birthdays = c("20000101", "19991201", "19760704", "20140314")

ymd(birthdays)

age = ymd(today()) - ymd(birthdays)

round(age/dyears(1), 1) # dyears is a lubridate function, not a typo!

age = ymd(20141201) - ymd(birthdays)

round(age/dyears(1), 0)

?strptime 


## Calculate a mean or correlation in circular time (clock time)

require(psych)

wake_time = c(7.0, 7.5, 6.5, 6.25, 7.0, 7.25, 7.25)

sleep_time = c(23.5, 0.5, 23.75, 0.25, 23.25, 23.5, 0.75)

wake_sleep = data.frame(wake_time, sleep_time)

circadian.mean(wake_sleep$sleep_time)

circadian.cor(wake_sleep)


## Plotting time-series data

require(eurostat)

demo_fmonth = get_eurostat('demo_fmonth')

demo_fmonth$date = as.Date(paste(substr(demo_fmonth$time, 1, 4), substr(demo_fmonth$month, 2, 3), "01", sep="-"))

require(dplyr)

UK_births = filter(demo_fmonth, geo == "UK" & month != 'TOTAL' & month != 'UNK' & date >= '2003-01-01' & date <= '2012-12-01')

UK_births = arrange(UK_births, date)

UK_births_ts = ts(UK_births$values, start=c(2003, 1), frequency=12)

plot(UK_births_ts, xlab="Date", ylab="Count", main="Monthly Births in the UK (2003-2012)", col="darkblue")

require(ggplot2)

ggplot(UK_births, aes(x=date, y=values)) +
    geom_line(col="darkblue") +
    ylab("Count") +
    theme_minimal()


## Detecting autocorrelation

acf(UK_births_ts)


## Plotting monthly patterns

# Monthplot to plot by month
monthplot(UK_births_ts, main="Monthly Patterns in UK Births (2003-2012)", xlab="Month", ylab="Count", col="darkblue", lwd=2, lty.base=2, lwd.base=1, col.base="gray40")

# Seasonplot to plot by year
require(forecast)

seasonplot(UK_births_ts, main="Seasonal Trends in UK Births (2003-2012)", col=rainbow(10), year.labels=TRUE, year.labels.left=TRUE, cex=0.7, cex.axis=0.8)

## Plotting seasonal adjustment on the fly

require(ggseas)

# Convert the time series object to a dataframe for stat_seas
UK_births_ts_df = tsdf(UK_births_ts)

# Plot seasonally-adjusted data
ggplot(UK_births_ts_df, aes(x=x, y=y)) +
    ggtitle("Seasonally-Adjusted Monthly Births in the UK (2003-2012)") + 
    geom_line(color="gray70") +
    stat_seas(color="darkblue", size=1) +
    scale_x_continuous(breaks=seq(2003, 2013, 1)) + 
    ylab("Count") +
    xlab("") +
    theme_minimal()


## Decomposing time series into components

### Additive decomposition

plot(decompose(UK_births_ts), col="darkblue")


### Decomposing when the variance changes

data(AirPassengers)

plot(AirPassengers)

require(forecast)

# Calculate the Box-Cox transformation
AirPassengers_lambda = BoxCox.lambda(AirPassengers)

# Plot the Box-Cox transformation results
plot(BoxCox(AirPassengers, AirPassengers_lambda))

# Plot the transformed decomposition
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

# These are just labels for qcc, not real dates
month_name = month.abb[c(1:12, 1:12, 1:1)]

infection_control = qcc(infections, sizes=patient_days/1000, type="u", labels=month_name, axes.las=2, xlab="Month", ylab="", digits=2, title="Hospital Acquired Infections Rate per 1,000 Patient Days\n u-chart for Jan 2012 - Jan 2014")

# Create a real date and move limits and rate into a data frame
ic_qcc = data.frame(Month = seq(as.Date("2012-01-01"), as.Date("2014-01-01"), "months"), infection_control$limits, Rate = (infections / patient_days)*1000)

# Create a factor for the "special causes" points
ic_qcc$Violations = factor(ifelse(row.names(ic_qcc) == infection_control$violations$beyond.limits, "Violation", NA))

# Plot a cleaner control chart
ggplot(ic_qcc, aes(x=Month, y=Rate)) +
    geom_line() +
    geom_point(color="darkblue") +
    geom_point(data=filter(infection_control_qcc, 
        Violations=="Violation"), color="red", size=3) +
    geom_line(aes(y=LCL), linetype="dashed", color="gray50") +
    geom_line(aes(y=UCL), linetype="dashed", color="gray50") +
    xlab("Month") +
    ylab("Rate") +
    ggtitle("Hospital Acquired Infections Rate per 1,000 Patient Days\nu-chart for Jan 2012 - Jan 2014") +
    theme_minimal()


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
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip", destfile = "~/BIWR/Chapter1/Data/household_power_consumption.zip")

unzip("~/BIWR/Chapter1/Data/household_power_consumption.zip", exdir="Data")

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


##### End of Chapter 6 #####



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

banks = read.csv("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/quiebra.csv", encoding = "UTF-8", stringsAsFactors = FALSE, header = TRUE)

ggparcoord(data=banks, columns=c(3:11), groupColumn=12, scale="globalminmax", alphaLines=0.5)

ggparcoord(data=banks, columns=c(3:11), groupColumn=12, scale="globalminmax", boxplot=TRUE, alphaLines=0.3)


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

rosling_data = read.csv("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/rosling_data.csv")

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
    stat_density2d(aes(fill=..level..), alpha=0.5, bins = 10,
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
    addCircleMarkers(data = bike_theft_dec, lat = ~ Latitude, 
        lng = ~ Longitude, popup = bike_theft$Hundred.Block.Location, 
        group="December 2014", fillOpacity = 0.25, color="red", radius=4) %>%
    addCircleMarkers(data = bike_theft_jan, lat = ~ Latitude, 
        lng = ~ Longitude, popup = bike_theft$Hundred.Block.Location, 
        group="January 2015", fillOpacity = 0.25, color="blue", radius=4) %>%
    addLayersControl(overlayGroups = c("December 2014", "January 2015"), 
        options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("bottomright",  title = "Legend", colors=c("red","blue"), 
        labels=c("December 2014", "January 2015"))

bike_theft_interactive


##### End of Chapter 7 #####



#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 8 Code: Patterns and Outliers
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_8_Code.R
#
######################################################################


## Mapping multivariate relationships

### Non-metric multidimensional scaling (nMDS)

require(vegan)

banks = read.csv("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/quiebra.csv", encoding = "UTF-8", stringsAsFactors = FALSE, header = TRUE)

banks_mds = metaMDS(banks[,3:11], distance="euclidean", autotransform=FALSE, noshare=FALSE, wascores=FALSE)

# You may need to mess with par for the margins. Note: Clearing the 
# plots in RStudio (broom icon) returns par to defaults, which is
# par(mar=c(1.5,0,1,0))

# Save the stress value for the legend
banks.stress = round(banks_mds$stress, digits=2)

# Set up color scheme
groups = ifelse(banks$Solvente == 0, "darkred", "blue")

# Create an empty plot
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", display="sites")

# Add banks by number and color (solvency)
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

# Add legend
legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred","blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)

# Zoom in 
ordiplot(banks_mds, type="n", xlab=" ", ylab=" ", display="sites", xlim=c(0,0.07), ylim=c(-0.1, 0.095))

orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred", "blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)

### Euromap nMDS example

# par(mar=c(1.5,4,1,4))

euromap = metaMDS(eurodist, distance="euclidean", autotransform=FALSE, noshare=FALSE, wascores=FALSE)

euromap$points[,1] = -euromap$points[,1]

ordiplot(euromap, display="sites", xaxt="n", yaxt="n", xlab=" ", ylab=" ", type="t")

### Diagnostics for nMDS results

banks_stress = round(banks_mds$stress, digits=2)

banks_stress

stressplot(banks_mds, xaxt="n", yaxt="n")

### Vector mapping influential variables over the nMDS plot

banks_vectors = envfit(banks_mds, banks[,3:11])

ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", display="sites")

orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

plot(banks_vectors, col="gray40", cex=0.75)

legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred","blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)

### Contour mapping influential variables over the nMDS plot

# par(mar=c(1.5,0,1,0))

# Empty plot
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", display="sites")

# Add contour of variable R8 (Cost of Sales:Sales ratio)
# Putting it in tmp suppresses console output
tmp = ordisurf(banks_mds, banks$R8, add=TRUE, col="gray30")

# Add contour of variable R4 (Reserves:Loans ratio)
tmp = ordisurf(banks_mds, banks$R4, add=TRUE, col="gray70")

# Plot nMDS solution
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred","blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)


### Principal Components Analysis (PCA)

banks_pca = princomp(banks[,3:11], cor=TRUE)

summary(banks_pca, loadings=TRUE)

# devtools::install_github("vqv/ggbiplot")

require(ggbiplot)

ggbiplot(banks_pca, labels=rownames(banks), ellipse=T, 
        groups= as.factor(banks$Solvente)) + 
    theme_bw()

# Get variances
vars = banks_pca$sdev^2

# Get proportion of variance
var_exp = vars/sum(vars)

# Get cumulative variance and make into data frame
variances = data.frame(vars = vars, var_exp = var_exp, var_cumsum = cumsum(var_exp), PCs = 1:length(banks_pca$sdev))

# Plot variance explained and cumulative variance
ggplot(variances, aes(PCs, var_cumsum)) +
    scale_x_discrete(limits=c(1:9)) +
    ylab("Var. Explained (bars) and Cumulative (line) by PC") +
    geom_bar(aes(y=var_exp), stat="identity", fill="gray", alpha=.5) +
    geom_line() +
    theme_bw()


### nMDS for Categories: Correspondence Analysis

require(ca)

smoke_ca = ca(smoke)

summary(smoke_ca)

plot(smoke_ca, arrows=c("F","T"), mass = c(TRUE, TRUE))


## Grouping observations with hierarchical clustering

banks_dist = dist(banks[,3:11])

banks_hclust = hclust(banks_dist, method="ward.D")

plot(banks_hclust, hang=-1, labels=paste(banks$BANCO, banks$NUMERO, sep="-"))


### Plotting a cluster dendrogram with ggplot

require(NeatMap)

ggplot() +
    draw.dendrogram(banks_hclust) +
    scale_color_manual(name="Status: ", values=c("darkred", "blue"),
        labels=c("Bankrupt ", "Solvent ")) +
    geom_text(aes(x=0.75, y=banks_hclust$order, label=banks$NUMERO, 
        color=factor(banks$Output)), size=4) +
    ggtitle("Cluster Dendrogram (Ward's) - Spanish Banking Crisis") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position="top")

banks_scaled = scale(banks[,3:11])

banks_dist_sc = dist(banks_scaled)

banks_hclust_sc = hclust(banks_dist, method="ward.D")

# Now rerun the ggplot commands as above
ggplot() +
    draw.dendrogram(banks_hclust_sc) +
    scale_color_manual(name="Status: ", values=c("darkred", "blue"),
        labels=c("Bankrupt ", "Solvent ")) +
    geom_text(aes(x=0.75, y=banks_hclust$order, label=banks$NUMERO, 
        color=factor(banks$Output)), size=4) +
    ggtitle("Cluster Dendrogram (Ward's) - Spanish Banking Crisis") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position="top")


### Exploring hierarchical clustering of nMDS results

require(NeatMap)

# nMDS results from above
banks_pos = banks_mds$points 

draw.dendrogram3d(banks_hclust, banks_pos, labels=banks$NUMERO, label.colors=banks$Solvente+3, label.size=1)


## How to partition the results of a hierarchical cluster analysis

# Run cutree for 2 groups
bank_clusters = cutree(banks_hclust, k=2)

# View results
table(bank_clusters)

# Show variable medians for each group
aggregate(banks[,3:11], by=list(cluster=bank_clusters), median)

# Plot dendrogram
plot(banks_hclust, hang=-1, labels=paste(banks$BANCO, banks$NUMERO, sep="-"), cex=0.9)

# Add cluster boxes to dendrogram
rect.hclust(banks_hclust, k=2)


## Identifying and describing group membership with kMeans and PAM

require(cluster)

banks_pam = pam(banks[,3:11], k=2)

banks_pam

banks_kmeans = kmeans(banks[,3:11], centers=2, nstart=50)

banks_kmeans

banks$pam2 = banks_pam$clustering

banks$km2 = banks_kmeans$cluster

table(banks$Output, banks$pam2)

table(banks$Output, banks$km2)

plot(banks_pam, which.plot=1, xlab="", ylab="")

require(useful)

plot(banks_kmeans, data=banks, class="Output", xlab="", ylab="")


## How to choose an optimal number of clusters with bootstrapping

banks_gap = clusGap(banks[,3:11], FUNcluster=pam, K.max=10, B=500)

banks_gap_df = as.data.frame(banks_gap$Tab)

ggplot(banks_gap_df, aes(x=1:nrow(banks_gap_df))) +
    scale_x_discrete(limits=c(1:10)) +
    xlab("Clusters") +
    geom_point(aes(y=gap), size=3) +
    geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim))


## Determining optimal numbers of clusters with model-based clustering

require(mclust)
data("diabetes")

diabetes_mclust = Mclust(diabetes[,2:4])

summary(diabetes_mclust)

diabetes_mclust$BIC

# Model comparison plot
plot(diabetes_mclust, what="BIC")

# Cluster results 
plot(diabetes_mclust, what="classification")

# Bivariate density (basic contour plot)
plot(diabetes_mclust, what="density")

# Bivariate density (perspective plot)
plot(diabetes_mclust, what = "density", type = "persp", border = adjustcolor(grey(0.01), alpha.f = 0.15))

# Cluster uncertainty plot
plot(diabetes_mclust, what="uncertainty")

# Add classification and uncertainty to data set
diabetes$mclust_cluster = diabetes_mclust$classification

diabetes$mclust_uncertainty = diabetes_mclust$uncertainty

# Plot the uncertainty as a histogram and table
require(googleVis)

diabetes_histogram = gvisHistogram(data.frame(diabetes$mclust_uncertainty), options=list(width=800, height=300, legend="none", title="Distribution of Mclust Uncertainty Values"))

diabetes_table = gvisTable(diabetes, options=list(width=800, height=300))

HT = gvisMerge(diabetes_histogram, diabetes_table) 

plot(HT)


## Identifying group membership with irregular clusters

data("multishapes", package="factoextra")

ggplot(multishapes, aes(x, y)) +
    geom_point(aes(shape=as.factor(shape), 
        color=as.factor(shape))) +
    scale_shape_discrete(name="Known\nGrouping", labels = 
        levels(as.factor(multishapes$shape))) +
    scale_color_discrete(name="Known\nGrouping", labels = 
        levels(as.factor(multishapes$shape))) +
    theme_bw()

require(dbscan)

# Estimate eps with kNN distance plot
kNNdistplot(multishapes[,1:2], k=3)

# Try an eps value of 0.15
abline(h=0.15, col="blue", lty=3)

multishapes_dbscan = dbscan(multishapes[,1:2], eps = 0.15, minPts = 3)

multishapes_dbscan

multishapes$dbscan = multishapes_dbscan$cluster

table(multishapes$shape, multishapes$dbscan)

ggplot(multishapes, aes(x, y)) +
    geom_point(aes(color=as.factor(dbscan), shape=as.factor(shape))) +
    labs(color="DBSCAN\nGrouping", shape="Known\nGrouping") +
    theme_bw()


## Variable selection in cluster analysis

require(clustvarsel)

# Download the Wisconsin Breast Cancer data
breast_cancer = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",", na.strings = c("NA", "?"), header=F, col.names=c("Sample", "Clump_Thickness", "Uniformity_Cell_Size", "Uniformity_Cell_Shape", "Marginal_Adhesion", "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class"))

# Label the outcomes
breast_cancer$Diagnosis = ifelse(breast_cancer$Class==2, "benign", ifelse(breast_cancer$Class==4, "malignant", "unk"))

# Remove incomplete rows, then create data-only object
bc_wisc = na.omit(breast_cancer[,c(2:10, 12)])

bc_wisc_data = bc_wisc[,1:8]

# Perform variable selection (with parallel processing)
varsel_fwd = clustvarsel(bc_wisc_data, parallel = TRUE)

varsel_fwd

# Create data frame of the date with only the chosen variables
bc_fwd = bc_wisc_data[,varsel_fwd$subset]

# Run Mclust on subsetted data frame
bc_mclust_fwd = Mclust(bc_fwd)

# Add clustering result to data
bc_wisc$mclust_fwd_cluster = bc_mclust_fwd$classification

# Review Mclust results
summary(bc_mclust_fwd)


## Error checking cluster results with known outcomes 

classError(bc_wisc$Diagnosis, bc_wisc$mclust_fwd_cluster)

adjustedRandIndex(bc_wisc$Diagnosis, bc_wisc$mclust_fwd_cluster)

# Run Mclust on the full variable set of the Wisconsin Breast Cancer data
bc_mclust = Mclust(bc_wisc_data)

bc_wisc$mclust_all = bc_mclust$classification

classError(bc_wisc$Diagnosis, bc_wisc$mclust_all)

adjustedRandIndex(bc_wisc$Diagnosis, bc_wisc$mclust_all)


## Exploring outliers

### Identifying outliers with distance functions

require(mvoutlier)

quiebra_outliers = uni.plot(banks[,3:11], symb = T)

# Assign outlier grouping to main data
banks$mv_outlier = quiebra_outliers$outliers

# Add Mahalanobis distance results to main data
banks$mv_md = quiebra_outliers$md

# Add Euclidean distance results to main data
banks$mv_euclidean = quiebra_outliers$euclidean

require(ggplot2)
require(plotly)

p1 = ggplot(banks, aes(mv_md, text = paste("Bank: ", 
        banks$NUMERO, "<br>Solvency: ", Output))) +
    xlab("Mahalanobis Distance") + 
    geom_histogram() + 
    facet_wrap(~mv_outlier, ncol=1, scales="free_y") 

ggplotly(p1)

uni.plot(bike_share_daily[,10:13], pch=as.numeric(bike_share_daily$weathersit))


### Identifying outliers with the local outlier factor

require(DMwR)

# Calculate the local outlier factor values
banks$lof = lofactor(banks[,3:11], k=4)

# Plot the lof results interactively
p_lof = ggplot(banks, aes(lof, text = paste("Bank: ",
        banks$NUMERO, "<br>Solvency: ", Output))) +
    xlab("Local Outlier Factor") + 
    geom_histogram() 

ggplotly(p_lof)


## Anomaly detection

# devtools::install_github("twitter/AnomalyDetection")

require(AnomalyDetection)
data(raw_data)

# Run the algorithm, include expected values and plotting features
raw_data_anomalies = AnomalyDetectionTs(raw_data, direction='both', e_value=TRUE, plot=TRUE)

# Plot the time series and the anomalies
raw_data_anomalies$plot

# Extract data and expected values
raw_data_anomalies_df = raw_data_anomalies$anoms 


## Extreme value analysis

require(dplyr) 

data("damage", package="extRemes")

hurricane_cost = damage %>% group_by(Year) %>% summarise(total_damage = sum(Dam))

yearz = data.frame(Year = seq(1926, 1995, 1))

hurricane_cost = full_join(yearz, hurricane_cost)

hurricane_cost$total_damage[is.na(hurricane_cost$total_damage)] = 0

require(extremeStat)

hurr_fit = distLfit(hurricane_cost$total_damage)

distLplot(hurr_fit, cdf=TRUE, main="Total Annual Hurricane Damages (in billion USD)", xlab="Damages ($1B USD)")

distLprint(hurr_fit)

hurr_fit$gof

# Calculate quantiles for different models
hurr_quant = distLquantile(hurricane_cost$total_damage, probs=c(0.80, 0.90, 0.95, 0.99), returnlist=TRUE)

# Look at the top five models only
hurr_quant$quant[1:5,]

# Plot the cdf of top five models with their quantiles
distLplot(hurr_quant, cdf=T, main="Total Annual Hurricane Damages (in billion USD)", xlab="Damages ($1B USD)", qlines=TRUE)

# Calcluate return intervals. 
hurr_returns = distLextreme(hurricane_cost$total_damage, RPs = c(2,10,50,100))

# View top five model results
hurr_returns$returnlev[1:5,]

# Plot return intervals
distLextremePlot(hurr_returns, main="Total Annual Hurricane Damages (in billion USD)", ylab="Damages ($1B USD)")


## Finding associations in shopping carts

require(arules)
require(arulesViz)

# Download data and convert to a transactions object
basket_url = "http://dmg.org/pmml/pmml_examples/baskets1ntrans.csv"

basket_trans = read.transactions(basket_url, format = "single", sep = ",", cols = c("cardid", "Product"), rm.duplicates=TRUE)

summary(basket_trans)

itemFrequencyPlot(basket_trans, topN=11, type="absolute") 

basket_rules_custom = apriori(basket_trans, parameter = list(support = 0.01, confidence = 0.01, target="rules"))

inspect(head(sort(basket_rules_custom, by="lift"), 10))

plot(basket_rules_custom, interactive = T)

beer_rules = apriori(data=basket_trans, parameter=list(supp=0.01, conf = 0.01, minlen=2), appearance = list(default="rhs", lhs="beer"))

inspect(head(sort(beer_rules, by="lift"), 10))

plot(beer_rules, method="graph", interactive=T)

write(basket_rules_custom, file = "basket_rules_custom.csv", sep = ",", row.names=FALSE)


##### End of Chapter 8 #####



# There is no non-.Rmd code in Chapter 9 #



##### End of File #####
