# Chapter 2: Getting Data

### Reading flat files from disk or the web

vanco=read.table(

pses2011 = read.table("http://www.tbs-sct.gc.ca/pses-saff/2011/data/2011_results-resultats.csv", 
    sep=",", encoding="UTF-8", header=FALSE)


### Reading Excel files

require(readxl)
bob = read_xl(

require(gdata)
pses2011_header = read.xls("http://www.tbs-sct.gc.ca/pses-saff/2011/data/PSES2011_Documentation.xls", 
    sheet="Layout-Format", nrows=22, header=FALSE, skip=1, col.names=
    c("Variables", "Size", "Type", "Description"), stringsAsFactors=FALSE)

colnames(pses2011) = pses2011_header$Variable


### Creating a dataframe from the clipboard

my_data_frame = read.table("clipboard", sep="\t", header=TRUE)


### Another way to to read SAS files

require(Hmisc)
sas.get(libraryName="directoryofsasfile", member="sasfile", 
    sasprog="full/path/to/sas.exe")

marketing = read.table(textConnection
("Survey_Response, Send_Email_Ad
Strongly_Agree, Yes
Agree, Yes
Neutral, No
Disagree, No
Strongly_Disagree, No"),
header=TRUE, sep=",")


### Reading big files with data.table

require(data.table)

# If you want to read in a csv directly, use fread
pses2011 = fread("pses2011.csv", sep=",")

# If you already have a dataframe and want to speed up
# reading and accessing it, use data.table
pses2011 = data.table("pses2011")


### Unzipping files into R

### SIMPLIFY THIS

tf = tempfile()
download.file("ftp://ftp.census.gov/econ2012/CBP_CSV/zbp12totals.zip", tf)
zipcodes = read.table(unz(tf, "zbp12totals.txt"), header=T, quote="\"", 
    sep=",", colClasses="character")
file.remove(tf)

td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", 
    tf)
unzip(tf, exdir=td)
bike_share_daily = read.table(unz(tf, "day.csv"), header=T, sep=",")
bike_share_hourly = read.table(unz(tf, "hour.csv"), header=T, sep=",")
bike_share_readme = readLines(unz(tf, "Readme.txt"))
file.remove(tf)


## Writing files to disk

write.table(pses2011, "pses2011.csv", sep=",", row.names=FALSE)


## Interfacing with a database

require(RODBC)

connect = odbcConnect("name_of_dsn_connection", uid="your_user_id", 
    pwd="your_password")

odbcGetInfo(connect)

sqlTables(connect)

# If there are a lot of tables or you want to make a record of
# their names, create an object and then view it within R:
connect_tables = sqlTables(connect)
connect_tables

whole_table = sqlFetch(connect, "table_name$")

# Within the double quotes you can put a valid SQL query
# Using single quotes to name the table
query_1 = "SELECT * FROM 'table_name$'"
query_1_dataframe = sqlQuery(connect, query_1)

query_2 = "SELECT * FROM 'table_name$' 
  WHERE variable_name = some_condition 
  ORDER BY ordering_variable_name"
query_2_dataframe = sqlQuery(connect, query_2)

odbcCloseAll()


## Creating a SQLite database inside R

require(RODBC)
require(sqldf)

sqldf("attach 'PSES_database.sqlite' as new")

connect = dbConnect(SQLite(), dbname="PSES_database.sqlite ")

read.csv.sql("pses2011.csv", sql="CREATE TABLE pses2011 AS SELECT * FROM file",
  dbname = "PSES_database.sqlite")

# This loads the Excel file into R
require(XLConnect)
pses2011_xls = loadWorkbook("PSES2011_Documentation.xls")
  
# This reads each worksheet into separate dataframes within a list
pses2011_documentation = readWorksheet(pses2011_xls, 
  sheet=getSheets(pses2011_xls))
  
# This file was downloaded to the working directory from
# http://www.tbs-sct.gc.ca/pses-saff/2011/data/PSES2011_Documentation.xls
# The sheet names have spaces and hyphens, which will cause
# trouble for SQLite; run names(pses2011_documentation) to see
# So, this changes the dataframe names inside the list
names(pses2011_documentation) = c("Questions", "Agency", "LEVELID15", "Demcode",
    "PosNegSpecs", "LayoutFormat")
  
# Add a new row to account for 0 values for LEVEL1ID in
# main pses2011 file
pses2011_documentation$Agency = rbind(pses2011_documentation$Agency,
    c(0,NA,"Other",NA,"OTH"))
  
# Now each sheet can be loaded into the database as a separate table
with(pses2011_documentation, {
    dbWriteTable(conn=connect, name="Questions", value=Questions, 
        row.names=FALSE)
    dbWriteTable(conn=connect, name="Agency", value=Agency, row.names=FALSE)
    dbWriteTable(conn=connect, name="LEVELID15", value=LEVELID15, 
        row.names=FALSE)
    dbWriteTable(conn=connect, name="Demcode", value=Demcode, row.names=FALSE)
    dbWriteTable(conn=connect, name="PosNegSpecs", value=PosNegSpecs, 
        row.names=FALSE)
    dbWriteTable(conn=connect, name="LayoutFormat", value=LayoutFormat, 
        row.names=FALSE)   
} )
  
# Remove the Excel objects
rm(pses2011_xls, pses2011_documentation)

sqldf("SELECT * FROM sqlite_master", dbname = "PSES_database.sqlite")$tbl_name

sqldf("SELECT * FROM sqlite_master", dbname = "PSES_database.sqlite")

# Note: PRAMA and TABLE_INFO are SQLite-specific statements
sqldf("PRAGMA TABLE_INFO(Questions)", dbname = "PSES_database.sqlite")$name       

dbListTables(connect)
dbListFields(connect, "Questions")  

sqldf("SELECT * FROM Questions LIMIT 6", dbname = "PSES_database.sqlite")
    
dbDisconnect(connect)


## Creating a dataframe from a SQLite database

require(sqldf)
connect = dbConnect(SQLite(), dbname="PSES_database.sqlite ")

pses_acr = sqldf("SELECT 
    Level1ID
    , Acronym_PSES_Acronyme_SAFF AS Acronym
    , DeptNameE as Department 
    FROM Agency", 
    dbname="PSES_database.sqlite")

pses2011_agency = sqldf("SELECT
    maintable.*
    , agencytable.DeptNameE as Agency
    , agencytable.Acronym_PSES_Acronyme_SAFF as Abbr
    FROM pses2011 maintable
    LEFT JOIN Agency agencytable
        ON maintable.LEVEL1ID = agencytable.Level1ID",
    dbname="PSES_db.sqlite")

dbDisconnect(connect)

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


## Working through a proxy

require(httr)

set_config(use_proxy(url="YOUR_PROXY_URL", port="YOUR_PORT", 
    username="YOUR_USER_NAME", password="YOUR_PASSWORD")


## Scraping data from a web table

require(XML)

drug_use_2010_table = readLines("http://oas.samhsa.gov/NSDUH/2k10NSDUH/tabs/Sect1peTabs1to46.htm")

drug_use_table1_1 = readHTMLTable(druguse, header=T, which=1, 
    stringsAsFactors=FALSE)

drug_use_table1_17a = readHTMLTable(druguse, header=T, which=31, 
    stringsAsFactors=FALSE)
drug_use_table1_17b = readHTMLTable(druguse, header=T, which=32, 
    stringsAsFactors=FALSE)


### Scraping tables that cover multiple pages

allpages = paste("http://projects.propublica.org/nursing-homes/findings/search?page=", 
    1:189, "&search=&ss=ALL&state=TX", sep="")

tablelist = list()
for(i in seq_along(allpages)){
  page = allpages[i]
  page = readLines(page)
  homes = readHTMLTable(page, header=T, which=1, stringsAsFactors = FALSE)
  tablelist[[i]] = homes
  }

nursing_home_deficiencies = do.call(rbind, lapply(tablelist, data.frame, 
    stringsAsFactors=FALSE))


### Cleaning up the scrape

colnames(nursing_home_deficiencies) = c("Date", "Home", "City", "State", 
    "Deficiency_Count", "Severity")
nursing_home_deficiencies$State = gsub("Tex.", "TX", 
    nursing_home_deficiencies$State, fixed = TRUE)
nursing_home_deficiencies$Home = gsub(" (REPORT)  Home Info", "", 
    nursing_home_deficiencies$Home, fixed = TRUE)
nursing_home_deficiencies$Severity = substr(nursing_home_deficiencies$Severity, 
    1, 1)
nursing_home_deficiencies$Date = gsub(".", "", nursing_home_deficiencies$Date, 
    fixed = TRUE)
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
nursing_home_deficiencies$Date = as.Date(nursing_home_deficiencies$Date,
    format="%B %d, %Y")


## Working with APIs

STUFF HERE



## Creating fake data to test code

# Table A – customer data
customer_id = seq(1:10000)
customer_gender = sample(c("M","F", "Unknown"), 10000, replace=TRUE,
    prob=c(.45, .45, .10))
set.seed(1235813)
customer_age = round(runif(10000, 18, 88), 0)
customer_purchases = round(rlnorm(10000)*100, 2)
  
# Table B – city/state/zip and business density lookup table
temp = tempfile()
download.file("ftp://ftp.census.gov/econ2012/CBP_CSV/zbp12totals.zip", temp)
# https://www.census.gov/econ/cbp/download/noise_layout/ZIP_Totals_Layout10.txt
zipcodes = read.table(unz(temp, "zbp12totals.txt"), header=T, quote="\"",
    sep=",", colClasses="character")
unlink(temp)
zipcodes = zipcodes[,c(1,11:12,10)]
# End of Table B
  
# Table A, continued
customer_zip = zipcodes[sample(1:nrow(zipcodes), 10000, replace=TRUE),]$zip
customers = data.frame(customer_id, customer_gender, customer_age,
    customer_purchases, customer_zip)
  
# TABLE C – results of a product interest survey
ones = seq(1, 1, length.out = 2000)
zeros = seq(0, 0, length.out = 2000)
strongly_agree = c(ones, zeros, zeros, zeros, zeros)
agree = c(zeros, ones, zeros, zeros, zeros)
neutral = c(zeros, zeros, ones, zeros, zeros)
disagree = c(zeros, zeros, zeros, ones, zeros)
strongly_disagree = c(zeros, zeros, zeros, zeros, ones)
survey = data.frame(customer_id, strongly_agree, agree, neutral, disagree,
    strongly_disagree)
  
# TABLE D – lookup table to match states to regions
state_regions = data.frame(datasets::state.abb, datasets::state.name, 
    datasets::state.region, datasets::state.division, colnames=c("abb", "state", 
    "region", "division"))


