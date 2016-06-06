#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 3 Code: Cleaning and Preparing Data
#
# 
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

customers$customer_gender = ordered(customers$customer_gender, 


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
nursing_home_deficiencies$State = gsub("Tex.", "TX", 
nursing_home_deficiencies$State, fixed = TRUE)
nursing_home_deficiencies$Home = gsub(" (REPORT)  Home Info", "", 
nursing_home_deficiencies$Home, fixed = TRUE)

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
my_data$my_variable = as.numeric(gsub("9999", NA, my_data$my_variable))

# But this is better
my_data$my_variable[my_data$my_variable==9999] = NA 

# This won't work
mean(customers$customer_age)

# This will
mean(customers$customer_age, na.rm=TRUE)
[1] 53.42279


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

my_data$date = as.Date(paste(mydata$yyyy_mm, "01", sep="-"))

my_data$date = as.Date(paste0(mydata$yyyy_mm, "-01"))

plot(customers$customer_age, customers$customer_purchase, main=paste("Age vs Purchase Amount for", length(customers$customer_age), "Customers"))

my_data$my_variable = as.numeric(paste(gsub(",", "", my_data$my_variable)))


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

customer_survey = sqldf("SELECT
                        customers.*
                        , zipcodes.city
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
  JOIN 
  customers_right_half B
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

vignette("introduction", package="dplyr"))

# http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


## Creating a derived column

customer_survey$Location = paste(city, stabbr, sep=", ")
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

surgery_outcomes = read.table("surgery_outcomes.csv", header=T, sep=",")

head(surgery_outcomes, 4)

surgery_outcomes_melted = melt(surgery_outcomes, id.vars=c(1:3), measure.vars=c(4:5), variable.name="Test_type", value.name="IQ_score")

head(surgery_outcomes_melted, 4)

surgery_outcomes_original = dcast(surgery_outcomes_melted, ID + Side + Phase ~ Test_type)

head(surgery_outcomes_original, 4)


### Reshaping: Crossing variables with ~ and +
  
head(dcast(surgery_outcomes_melted, ID ~ Test_type + Phase), 4)
  
head(dcast(surgery_outcomes_melted, ID + Side ~ Test_type + Phase), 4)
  
head(dcast(surgery_outcomes_melted, ID + Phase ~ Test_type), 4)


### Summarizing while reshaping

dcast(surgery_outcomes_melted, Phase ~ Test_type, mean, na.rm=T)
  
dcast(surgery_outcomes_melted, Side ~ Test_type + Phase, mean, na.rm=T)
  
dcast(surgery_outcomes_melted, Test_type + Side + Phase ~ ., sd, na.rm=T)


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
            sum_est = sum(est)) %>%  
  mutate(cv_purchases = round(sd_purchases / mean_purchases, 2))  


##### End of File #####