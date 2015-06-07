# Chapter 3: Cleaning and Preparing Data

tf = tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", 
    tf)
bike_share_daily_raw = read.table(unz(tf, "day.csv"), header=TRUE, sep=",")
file.remove(tf)
write.table(bike_share_daily_raw, “bike_share_daily.csv”, row.names=FALSE)
rm(bike_share_daily_raw)

bike_share_daily=read.table("bike_share_daily.csv", sep=",", header=T, 
    colClasses=c("character", "Date", "factor", "factor", "factor", "factor", 
    "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric",
    "integer", "integer", "integer"))
levels(bike_share_daily$season) = c("Winter", "Spring", "Summer", "Fall")
levels(bike_share_daily$workingday) = c("No", "Yes")
levels(bike_share_daily$holiday) = c("No", "Yes")
bike_share_daily$mnth = as.factor(as.numeric(bike_share_daily$mnth))
levels(bike_share_daily$mnth) = c(month.abb)
levels(bike_share_daily$yr) = c(2011, 2012)

str(bike_share_daily)
head(bike_share_daily)
summary(bike_share_daily)
bike_share_daily[!complete.cases(bike_share_daily),]


## Dummy NA values and dropping the NAs

my_data$my_variable[my_data$my_variable==9999] = NA 

my_data = na.omit(my_data)

mean(my_data$my_variable, na.rm=TRUE)


## Dropping duplicates

deduped_df = unique(df)


## DOUBLE CHECK ON REDUCING TO ONLY NUMERIC


## Merging dataframes

require(dplyr)
require(sqldf)

# dplyr
customer_locations = left_join(customers, zipcodes, by = 
  c("customer_zip" = "zip")
customer_regions = left_join(customer_locations, state_regions, by= 
  c("stabbr" = "abb")
customer_survey = left_join(customer_regions, survey, by = 
  c("customer_id" = "customer_id")

# merge
customer_locations = merge(customers, zipcodes, by.x = "customer_zip", 
  by.y = "zip", all.x = TRUE)
customer_regions = merge(customer_locations, state_regions, by.x = "stabbr", 
  by.y = "abb", all.x = TRUE)
customer_survey = merge(customer_regions, survey, by.x="customer_id", 
  by.y="customer_id", all.x=TRUE)

# sqldf
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

# left join
left_join(A, B, by=c("A.key" ="B.key")) 
merge(A, B, by.x="A.key", by.y="B.key", all.x=TRUE)
sqldf("SELECT * FROM A LEFT JOIN B ON A.key = B.key", stringsAsFactors=FALSE)
# inner_join
inner_join(A, B, by=c("A.key" ="B.key"))
merge(A, B, by.x="A.key", by.y="B.key", all=FALSE)
SELECT * FROM A INNER JOIN B ON A.key = B.key", stringsAsFactors=FALSE)
# n/a 
merge(A, B, by.x="A.key", by.y="B.key", all=TRUE)
SELECT * FROM A FULL OUTER JOIN B ON A.key = B.key", stringsAsFactors=FALSE)
# 
semi_join(A, B, by=c("A.key" ="B.key"))
sqldf("SELECT * FROM A LEFT JOIN B ON A.key = B.key WHERE B.key iS NULL", 
    stringsAsFactors=FALSE) | 
# 
anti_join(A, B, by=c("A.key" ="B.key"))
sqldf("SELECT * FROM A FULL OUTER JOIN B ON A.key = B.key WHERE A.key is NULL 
    OR B.key iS NULL", stringsAsFactors=FALSE) | 
# 
left_join(B, A, by=c("B.key" ="A.key")) 
merge(A, B, by.x="A.key", by.y="B.key", all.y=TRUE) 
sqldf("SELECT * FROM A RIGHT JOIN B ON A.key = B.key", stringsAsFactors=FALSE)
# 
semi_join(B, A, by=c("B.key" ="A.key"))
sqldf("SELECT * FROM A RIGHT JOIN B ON A.key = B.key WHERE A.key IS NULL", 
    stringsAsFactors=FALSE)


## Filtering, selecting, and arranging a dataframe

require(dplyr)

customers_marketing = filter(customer_survey, strongly_agree == 1 | agree == 1)
customers_CA_65_200_OR_500 = filter(customer_survey, state == "California" &
    customer_age >= 65 & customer_purchases >= 200 | state == "Oregon" &
    customer_purchases > 500)

customers_only = select(customer_survey, starts_with("customer"))
customers_only_agreement = select(customer_survey, customer_id, 
    matches("agree"))

customers_excluded = select(customer_survey, -starts_with("customer"))

customers_sorted = arrange(customers_CA_65_200_OR_500, state, customer_age, 
    desc(customer_purchases))


## Stringing it together in dplyr

customer_500_dplyred = customer_survey %>%
    filter(customer_purchases >= 500) %>%
    group_by(state) %>%
    summarize(count_purchases = n(),
              mean_purchases = mean(customer_purchases, na.rm=T),
              sd_purchases = sd(customer_purchases, na.rm=T),
              sum_est = sum(est)) %>%
    mutate(cv_purchases = round(sd_purchases / mean_purchases, 2))

customer_survey = mutate(customer_survey, Location = paste(city, stabbr, 
    sep=", "))
# vs
customer_survey$Location = paste(city, stabbr, sep=", ")


## Peeking at the outcome

head(select(customer_survey, contains("customer", ignore.case=TRUE)))
head(arrange(customer_survey, region, desc(customer_age), 
    desc(customer_purchases)))
head(mutate(customer_survey, Location = paste(city, stabbr, sep=", "), 
    purchase_proportion=round(customer_purchases/length(customer_purchases),2)))


## Transforming a dataframe from wide to long and back again

require(reshape2)
surgery_outcomes = read.table("surgery_outcomes.csv", header=T, sep=",")
head(surgery_outcomes, 4)

surgery_outcomes_melted = melt(surgery_outcomes, id.vars=c(1:3),
    measure.vars=c(4:5), variable.name="Test_type", value.name="IQ_score")
head(surgery_outcomes_melted, 4)

surgery_outcomes_original = dcast(surgery_outcomes_melted, ID + Side + 
    Phase ~ Test_type)
head(surgery_outcomes_original, 4)


### Reshaping by crossing variables with ~ and +

head(dcast(surgery_outcomes_melted, ID ~ Test_type + Phase), 4)

head(dcast(surgery_outcomes_melted, ID + Side ~ Test_type + Phase), 4)

head(dcast(surgery_outcomes_melted, ID + Phase ~ Test_type), 4)


### Summarizing while reshaping

dcast(surgery_outcomes_melted, Phase ~ Test_type, mean, na.rm=T)

dcast(surgery_outcomes_melted, Side ~ Test_type + Phase, mean, na.rm=T)

dcast(surgery_outcomes_melted, Test_type + Side + Phase ~ ., sd, na.rm=T)


