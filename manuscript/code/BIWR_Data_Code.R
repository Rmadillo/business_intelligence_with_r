#####################################################################
# Business Intelligence with R
# Dwight Barry
# Data Acquisition Code
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Data_Code.R
#
#####################################################################



#####################################################################
##### Power Consumption #####
# Chapters 1, 2, and 6, and Appendices 2 and 4

download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip", destfile="~/BIWR/Chapter1/Data/household_power_consumption.zip")

unzip("~/BIWR/Chapter1/Data/household_power_consumption.zip", exdir="Data")

power = read.table("~/BIWR/Chapter1/Data/household_power_consumption.txt", sep=";", header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

power$Date = as.Date(power$Date, format="%d/%m/%Y")

power$Month = format(power$Date,"%Y-%m")

power$Month = as.Date(paste0(power$Month, "-01"))

power$Global_active_power_locf = na.locf(power$Global_active_power)

#####################################################################



#####################################################################
##### PSES 2011 #####
# Chapter 2

require(gdata)

pses2011 = read.table("http://www.tbs-sct.gc.ca/pses-saff/2011/data/2011_results-resultats.csv", sep=",", encoding="UTF-8")

pses2011_header = read.xls("http://www.tbs-sct.gc.ca/pses-saff/2011/data/PSES2011_Documentation.xls", sheet="Layout-Format", nrows=22, header=FALSE, skip=1, col.names=c("Variables", "Size", "Type", "Description"), stringsAsFactors=FALSE)

colnames(pses2011) = pses2011_header$Variable

require(XLConnect)

download.file("http://www.tbs-sct.gc.ca/pses-saff/2011/data/PSES2011_Documentation.xls", "PSES2011_Documentation.xls")

pses2011_xls = loadWorkbook("PSES2011_Documentation.xls")

pses2011_documentation = readWorksheet(pses2011_xls, sheet=getSheets(pses2011_xls))

names(pses2011_documentation) = c("Questions", "Agency", "LEVELID15", "Demcode", "PosNegSpecs", "LayoutFormat")

pses2011_documentation$Agency = rbind(pses2011_documentation$Agency, (0, NA, "Other", NA, "OTH"))

#####################################################################



#####################################################################
##### XML and JSON downloads and scrapes #####
# Chapter 2

require(XML)

vha_pt_sat_raw = readLines("http://www1.va.gov/VETDATA/docs/Datagov/Data_Gov_VHA_2010_Dataset11_Patient_Satisfaction.xml")

FHFA_HPI_raw = readLines("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.xml")

drug_use_2010 = readLines("http://archive.samhsa.gov/data/NSDUH/2k10nsduh/tabs/Sect1peTabs1to46.htm")

require(jsonlite)

ufos = fromJSON("http://metricsgraphicsjs.org/data/ufo-sightings.json")

currency_list = fromJSON("http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json")

download.file("http://jsonstudio.com/wp-content/uploads/2014/02/enron.zip", "enron.zip")

unzip("enron.zip")

enron = stream_in(file("enron.json"))

#####################################################################



#####################################################################
##### Bike Share #####
# Chapters 2, 4, 5, 6, 7, and 8 

download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", "Bike-Data.zip")

bike_share_daily = read.table(unz("Bike-Sharing-Dataset.zip", "day.csv"), colClasses=c("character", "Date", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric", "integer", "integer", "integer"), sep=",", header=TRUE)

levels(bike_share_daily$season) = c("Winter", "Spring", "Summer", "Fall")

levels(bike_share_daily$workingday) = c("No", "Yes")

levels(bike_share_daily$holiday) = c("No", "Yes")

bike_share_daily$mnth = ordered(bike_share_daily$mnth, 1:12)

levels(bike_share_daily$mnth) = c(month.abb)

levels(bike_share_daily$yr) = c(2011, 2012)

#####################################################################



#####################################################################
##### Nursing Home Deficiencies #####
# Chapters 2 and 3

require(XML)

allpages = paste("http://projects.propublica.org/nursing-homes/findings/search?page=", 1:524, "&search=&ss=ALL&state=TX", sep="")

tablelist = list()
for(i in seq_along(allpages)){
    page = allpages[i]
    page = readLines(page)
    homes = readHTMLTable(page, header=T, which=1, stringsAsFactors = FALSE)
    tablelist[[i]] = homes
}

nursing_home_deficiencies = do.call(rbind, lapply(tablelist, data.frame, stringsAsFactors=FALSE))

colnames(nursing_home_deficiencies) = c("Date", "Home", "City", "State", "Deficiency_Count", "Severity")

nursing_home_deficiencies$State = gsub("Tex.", "TX", nursing_home_deficiencies$State, fixed = TRUE)

nursing_home_deficiencies$Home = gsub(" (REPORT)  Home Info", "", nursing_home_deficiencies$Home, fixed = TRUE)

nursing_home_deficiencies$Severity = substr(nursing_home_deficiencies$Severity, 1, 1)

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

#####################################################################



#####################################################################
##### Creating Fake Customer Data #####
# Chapters 2 and 3

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

#####################################################################



#####################################################################
##### Surgery Outcomes #####
# Chapter 3

surgery_outcomes = read.table("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/surgery_outcomes.tsv", header=T, sep="\t")

#####################################################################



#####################################################################
##### UK births #####
# Chapter 6

require(eurostat)

demo_fmonth = get_eurostat('demo_fmonth')

demo_fmonth$date = as.Date(paste(substr(demo_fmonth$time, 1, 4), substr(demo_fmonth$month, 2, 3), "01", sep="-"))

UK_births = filter(demo_fmonth, geo == "UK" & month != 'TOTAL' & month != 'UNK' & date >= '2003-01-01' & date <= '2012-12-01')

#####################################################################



#####################################################################
##### CO2 levels #####
# Chapter 6
# http://www.eia.gov/beta/MER/index.cfm?tbl=T12.01#/?f=M&start=200101&end=201506&charted=0-1-13    

US_co2 = read.table("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T12.01", sep=",", header=T)

#####################################################################



#####################################################################
##### CPI time series #####
# Chapter 6

CPI_xport = read.table("http://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation", header=T, sep="\t", strip.white=T)

#####################################################################



#####################################################################
##### Spanish Banking Crisis #####
# Chapters 7 and 8

banks = read.csv("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/quiebra.csv", encoding = "UTF-8", stringsAsFactors = FALSE, header = TRUE)

#####################################################################



#####################################################################
##### Motionchart #####
# Chapter 7

rosling_data = read.csv("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/rosling_data.csv")

#####################################################################



#####################################################################
##### Mapping #####
# Chapter 7

# © EuroGeographics for the administrative boundaries
download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/CNTR_2014_03M_SH.zip", destfile="world2014.zip")

unzip("~/Downloads/world2014.zip", exdir="~/Downloads/world2014", junkpaths = TRUE)

# http://www.eea.europa.eu/data-and-maps/data/airbase-the-european-air-quality-database-8
eu_o3 = read.table("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/eu_o3.csv", sep=",", header=T)

spd = read.csv("https://data.seattle.gov/api/views/7ais-f98f/rows.csv", header=T, strip.white = T)

#####################################################################



#####################################################################
##### Wisconsin Breast Cancer #####
# Chapter 8

breast_cancer = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",", na.strings = c("NA", "?"), header=F, col.names=c("Sample", "Clump_Thickness", "Uniformity_Cell_Size", "Uniformity_Cell_Shape", "Marginal_Adhesion", "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class"))

#####################################################################



#####################################################################
##### Shopping Basket #####
# Chapter 8

require(arules)

basket_url = "http://dmg.org/pmml/pmml_examples/baskets1ntrans.csv"

basket_trans = read.transactions(basket_url, format = "single", sep = ",", cols = c("cardid", "Product"), rm.duplicates=TRUE)

#####################################################################



##### End of File #####
