#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 2 Code
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_2_Code.R
#
#####################################################################


## Working with files

### read.table

# read.table from Chapter 1
power = read.table("Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

# Read in a csv from the internet with non-ASCII characters
pses2011 = read.table("http://www.tbs-sct.gc.ca/pses-saff/2011/data/2011_results-resultats.csv", sep=",", encoding="UTF-8")


### Reading big files with data.table
require(data.table)

# If you want to read in a csv directly, use fread, e.g., if you
# had the raw pses2011 in a local csv, you'd read it in this way:
power = fread("Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

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
Strongly Disagree, No"), header=TRUE, sep=",") 


### Reading XML files

require(XML)

# Read in the webpage as-is
vha_pt_sat_raw = readLines("http://www1.va.gov/VETDATA/docs/Datagov/Data_Gov_VHA_2010_Dataset11_Patient_Satisfaction.xml")

# Convert xml to a data frame
VHA_Patient_Satisfaction = xmlToDataFrame(vha_pt_sat_raw)
    
# From list first and then into a data frame 
FHFA_HPI_raw = readLines("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.xml")

FHFA_HPI_list = xmlToList(xmlParse(FHFA_HPI_raw))

FHFA_HPI = data.frame(do.call(rbind, FHFA_HPI_list)) 


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

connection = odbcConnect("NAME OF DSN CONNECTION", uid="YOUR USER ID",pwd="YOUR PASSWORD")

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

query_1_dataframe = sqlQuery(connection, query_1)

# While not as clear for large queries, you can combine them into one step:

query_1_dataframe = sqlQuery(connection, "SELECT * FROM 'table_name'")

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

# Populate database with csv
read.csv.sql("pses2011.csv", sql = "CREATE TABLE pses2011 AS SELECT * FROM file", dbname = "PSES_database.sqlite")

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

drug_use_2010_table = readLines("http://oas.samhsa.gov/NSDUH/2k10NSDUH/tabs/Sect1peTabs1to46.htm")

drug_use_table1_1 = readHTMLTable(druguse, header=T, which=1, stringsAsFactors=FALSE)

drug_use_table1_17a = readHTMLTable(druguse, header=T, which=31, 
stringsAsFactors=FALSE)

drug_use_table1_17b = readHTMLTable(druguse, header=T, which=32, 
stringsAsFactors=FALSE)


#### Scraping tables that cover multiple pages

allpages = paste("http://projects.propublica.org/nursing-homes/findings/search?page=", 1:189, "&search=&ss=ALL&state=TX", sep="")

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
customers = data.frame(customer_id, customer_gender, customer_age, customer_purchases, customer_zip)

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
state_regions = data.frame(datasets::state.abb, datasets::state.name, datasets::state.region, datasets::state.division)
colnames(state_regions) = c("abb", "state", "region", "division")


## Writing files to disk

# csv
write.table(pses2011, "pses2011.csv", sep=",", row.names=FALSE)

# json
iris_json = jsonlite::toJSON(iris, pretty=TRUE)
sink("iris_json.json")
iris_json
sink()
