# Chapter 4: Know Thy Data

tf = tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", 
    tf)
bike_share_daily_raw = read.table(unz(tf, "day.csv"), header=TRUE, sep=",")
file.remove(tf)
write.table(bike_share_daily_raw, “bike_share_daily.csv”, row.names=FALSE)
rm(bike_share_daily_raw)


## Creating summary plots

require(GGally)
ggpairs(data=bike_share_daily, columns=c(14:15, 10, 13, 3, 7), 
    title="Daily Bike Sharing Data", axisLabels="show", color="season")


## Create histograms of all numeric variables in one plot

require(psych)
multi.hist(bike_share_daily[,sapply(bike_share_daily, is.numeric)])


## A better "pairs" plot

require(psych)
pairs.panels(bike_share_daily[,sapply(bike_share_daily, is.numeric)], 
    ellipses=FALSE, pch=".", las=2, cex.axis=0.7, method="kendall")


## Mosaic plots—"Scatterplots" for categorical data

require(vcd)
pairs(table(bike_share_daily[c(3, 9)])) # not shown

pairs(Titanic, highlighting=2)


## Obtaining summary statistics

require(psych)
bike_share_daily=read.table("bike_share_daily.csv", sep=",", header=T)

describe(bike_share_daily[10:16]) # not shown
describeBy(bike_share_daily[10:16], bike_share_daily$holiday)

table(bike_share_daily$holiday)
prop.table(table(bike_share_daily$holiday))

describeBy(bike_share_daily[14:16], bike_share_daily$season == "Winter") # not shown
describeBy(bike_share_daily[14:16], bike_share_daily$temp > 0.5) # not shown
describeBy(bike_share_daily$casual, bike_share_daily$windspeed > 
    mean(bike_share_daily$windspeed))

table(bike_share_daily[c(3,6:7)]) # not shown
addmargins(table(bike_share_daily[c(3,6:7)])) # not shown
prop.table(table(bike_share_daily[c(3,9)]))

require(plyr)
count(bike_share_daily[c(3,6:7)])


## Inference on summary statistics: confidence intervals

# Median, non-parametric
wilcox.test(x, conf.int=TRUE)$conf.int
# Mean, Normal
t.test(x)$conf.int
# Percent/Proportion, Binomial
binom.test(x, n)$conf.int
# Count, Poisson
poisson.test(x)$conf.int
# Rate, Poisson
poisson.test(x, n)$conf.int

require(boot)
sd_boot_function = function(x,i){sd(x[i])
sd_boot = boot(CO2$conc, sd_boot_function, R=10000)
sd(CO2$conc)
boot.ci(sd_boot, type="bca")$bca[4:5]

q75_function = function(x,i){quantile(x[i], probs=0.75)
q75_boot = boot(CO2$conc, q75_function, R=10000)
quantile(CO2$conc,0.75)
boot.ci(q75_boot, type="bca")$bca[4:5]


## Inference on summary statistics: tolerance intervals

require(tolerance)
commute_time = c(68, 42, 40, 69, 46, 37, 68, 68, 69, 38, 51, 36, 50, 37, 41, 
    68, 59, 65, 67, 42, 67, 62, 48, 52, 52, 44, 65, 65, 46, 67, 62, 66, 43, 58, 
    45, 65, 60, 55, 48, 46)
commute_time_npti = nptol.int(commute_time, alpha=0.05, P=0.75, side=2)
commute_time_npti

plottol(commute_time_npti, commute_time, side="two", plot.type="both")

par(mfrow=c(1,1))

# Percent, Binomial
bintol.int(x, n, m, …)
# Count/Rate, Poisson
poistol.int(x, n, m, side, …)
# Non parametric
nptol.int(x, …)
# Continuous, Normal
normtol.int(x, side, …)
# Continuous, Uniform
uniftol.int(x, …)
# Lifetime/survival, Exponential
exptol.int(x, type.2, …)
# Score, Laplace
laptol.int(x, …)
#Indicies, Gamma
gamtol.int(x, …)
# Reliability/extreme values, Weibull/Gumbel
extol.int(x, dist, …)


## Plotting univariate distributions

require(ggplot2)
require(scales)

# density plot
ggplot(bike_share_daily, aes(casual)) +
    geom_density(col="blue", fill="blue", alpha=0.3) +
    xlab("Casual Use") +
    theme_bw()

# histogram
ggplot(bike_share_daily, aes(casual)) +
    geom_histogram(col="blue", fill="blue", alpha=0.3) +
    xlab("Casual Use") +
    theme_bw()

# density and histogram
ggplot(bike_share_daily, aes(casual)) +
    ylab("density and count") +
    xlab("Casual Use") +
    geom_histogram(aes(y=..density..), col="blue", fill="blue", alpha=0.3) +
    geom_density(col="blue", fill="blue", alpha=0.2) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

# bar plot
ggplot(bike_share_daily, aes(weathersit)) +
    geom_bar(col="blue", fill="blue", alpha=0.3) +
    xlab("Weather Pattern") +
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", 
    "Stormy")) +
    theme_bw()
    
# dot plot 
ggplot(bike_share_daily, aes(x=weathersit, y=..count.. )) +
    geom_point(stat = "bin", size = 3, pch=15, col="blue") +
    xlab("Weather Pattern") +
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", 
    "Stormy")) +
    theme_bw()

# horizontal bar chart
ggplot(bike_share_daily, aes(weathersit)) +
    geom_bar(col="blue", fill="blue", alpha=0.3) +
    xlab("") +
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", 
    "Stormy")) +
    coord_flip() +
    theme_bw()


## Plotting multiple univariate distributions with faceting

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
    scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", 
    "Stormy")) +
    coord_flip() +
    facet_wrap(~season, ncol=1) +
    theme_light()

## Plotting bivariate and comparative distributions

require(ggplot2)
require(scales)
require(vcd)

# double density plot
ggplot(bike_share_daily, aes(casual, fill=workingday, color=workingday)) +
    geom_density(alpha=0.4) +
    theme_minimal() +
    xlab("Daily Casual Bike Use Count") +
    ylab("") +
    scale_fill_discrete(name="Work Day?") +
    scale_color_discrete(name="Work Day?") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
    legend.position="top")

# mosaic plot
mosaic(~ weathersit + season, data=bike_share_daily, shade=T, 
    legend=F, labeling_args = list(set_varnames = c(season = "Season", 
    weathersit = "Primary Weather Pattern"), set_labels = list(weathersit = 
    c("Clear", "Cloudy/Rainy",  "Stormy"))))

table(bike_share_daily$weathersit, bike_share_daily$season)


## Multiple bivariate comparisons with faceting

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


## Plotting survey data

require(likert)
mathiness = likert(mass[2:15])
plot(mathiness)

require(reshape)
gender_math = likert(items=mass[,c(4,6,15), drop=FALSE], grouping=mass$Gender)
plot(gender_math, include.histogram=TRUE)

detach("package:likert", unload=TRUE)
detach("package:reshape", unload=TRUE)


## Effect sizes: Measuring differences between groups

require(bootES)
require(orddom)
require(asympTest)
require(reshape2)

casual_workingday_use = dcast(bike_share_daily, yr~workingday, 
    value.var="casual", sum)
casual_workingday_use$sum = casual_workingday_use$Yes + casual_workingday_use$No
require(dplyr)
casual_notworkingday = filter(bike_share_daily, workingday == "No" & 
    season == "Spring" | workingday == "No" & season == "Fall")
casual_notworking_Spring =  filter(notworkingday, season == "Spring")
casual_notworking_Fall =  filter(notworkingday, season == "Fall")

workday_diff = prop.test(casual_workingday_use$Yes, casual_workingday_use$sum)
round(workday_diff$estimate[1] - workday_diff$estimate[2], 2)
round(workday_diff$conf.int, 2)

casual_notworkingday_mean = t.test(casual~season, data=casual_notworkingday)
abs(casual_notworkingday_mean$estimate[1] - 
    casual_notworkingday_mean$estimate[2]); casual_notworkingday_mean$conf.int

bootES(casual_notworkingday, data.col="casual", group.col="season", 
    contrast=c("Fall", "Spring"), effect.type="unstandardized")

casual_notworkingday_median = wilcox.test(casual~season, 
  data=casual_notworkingday, conf.int=TRUE)
casual_notworkingday_median$estimate
casual_notworkingday_median$conf.int

bootES(casual_notworkingday, data.col="casual", group.col="season", 
    contrast=c("Fall", "Spring"), effect.type="hedges.g")

bootES(casual_notworkingday, data.col="casual", group.col="season", 
    contrast=c("Fall", "Spring"), effect.type="akp.robust.d")

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual,
    theta.es="dc")$theta
dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual,
    theta.es="dc")$theta.bci.lo
dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual,
    theta.es="dc")$theta.bci.up

dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual,
    theta.es="Ac")$theta
dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual,
    theta.es="Ac")$theta.bci.lo
dmes.boot(casual_notworking_Fall$casual, casual_notworking_Spring$casual,
    theta.es="Ac")$theta.bci.up

delta_gr(casual_notworking_Fall$casual, casual_notworking_Spring$casual, 
    x.name="Fall", y.name="Spring")

var.test(casual_notworkingday$casual ~ casual_notworkingday$season)$estimate

var.test(casual_notworkingday$casual ~ casual_notworkingday$season)$conf.int

require(asympTest)
asymp.test(casual_notworkingday$casual ~ casual_notworkingday$season,
    parameter = "dVar")$estimate

asymp.test(casual_notworkingday$casual ~ casual_notworkingday$season,
    parameter = "dVar")$conf.int


## What are the chances? Determining the probability of a difference

# require(devtools)
# devtools::install_github("rasmusab/bayesian_first_aid")

require(BayesianFirstAid)

workday_diff_bayes = bayes.prop.test(casual_workingday_use$Yes, 
    casual_workingday_use$sum)
workday_diff_bayes
plot(workday_diff_bayes)

casual_notworkingday_mean_bayes = bayes.t.test(casual~season, 
    data=casual_notworkingday)
casual_notworkingday_mean_bayes
plot(casual_notworkingday_mean_bayes)

summary(casual_notworkingday_mean_bayes)

# Mean, Continuous, Normal, t, “normal enough”
bayes.t.test(x)
# Proportion/Percent, Binomial
bayes.binom.test(x, n)
# Count, Poisson
bayes.poisson.test(x)
# Rate, Poisson
bayes.poisson.test(x, T)

diagnostics(casual_notworkingday_mean_bayes) # not shown


## Effect sizes: Measuring relationships between groups

require(bootES)
require(psych)
bike_use_atemp = data.frame(bike_share_daily$atemp, bike_share_daily$cnt)
colnames(bike_use_atemp) = c("air_temp", "count")

cor(bike_use_atemp$air_temp, bike_use_atemp$count)
cor.test(bike_use_atemp$air_temp, bike_use_atemp$count)$conf.int

bootES(c(bike_use_atemp$air_temp, bike_use_atemp$count), effect.type="r")

cor.ci(bike_use_atemp, method="spearman", n.iter = 10000, plot=FALSE)

cor.ci(bike_use_atemp, method="kendall", n.iter = 10000, plot=FALSE)


### Associations between categorical variables

require(psych)
data(Aspirin, package="abd")

oddsratio(table(Aspirin))$measure
Yule(table(Aspirin))


### Bootstrapping BCa CIs for non-parametric correlation

require(boot)
rs_function = function(x,i){cor(x[i,1], x[i,2], method="spearman")}
rs_boot = boot(bike_use_atemp, rs_function, R=10000)
boot.ci(rs_boot, type="bca")$bca[4:5]

rt_function = function(x,i){cor(x[i,1], x[i,2], method="kendall")}
rt_boot = boot(bike_use_atemp, rt_function, R=10000)
boot.ci(rt_boot, type="bca")$bca[4:5]


### Partial correlations

require(psych)

bike_use_atemp_wind = data.frame(bike_share_daily$temp, bike_share_daily$cnt, 
    bike_share_daily$windspeed )
atemp_wind_count = corr.test(bike_use_atemp_wind, method="kendall")
atemp_wind_count$ci[1:3]

partial.r(as.matrix(atemp_wind_count$r), c(1:2), 3)


### Polychoric and polyserial correlation for ordinal data

require(psych)

data(mass, package="likert")
poly_math = data.frame(as.numeric(mass[,7]), as.numeric(mass[,14]))
colnames(poly_math) = c("worry", "enjoy")
polychoric(poly_math)$rho

math_score = c(755, 642, 626, 671, 578, 539, 769, 614, 550, 615, 749, 676, 753, 
    509, 798, 783, 508, 767, 738, 660)
polyserial(math_score, poly_math$enjoy)


### Cohen’s kappa for comparisons of agreement

require(psych)

doctor = c(1,2,3,4,1,2,3,4,1,2,3,4)
model = c(1,3,1,3,1,4,4,4,4,2,3,1)
cohen.kappa(x=cbind(doctor,model))

doctor = c("yes", "no", "yes", "unsure", "yes", "no", "unsure", "no", "no", 
    "yes", "no", "yes", "yes")
model = c("yes", "yes", "unsure", "yes", "no", "no", "unsure", "no", "unsure", 
    "no", "yes", "yes", "unsure")
doctor_vs_model = data.frame(doctor, model)
cohen.kappa(doctor_vs_model)

cohen.kappa(doctor_vs_model)$agree

  
### Bayesian correlation

require(BayesianFirstAid)
bayes.cor.test(bike_use_atemp$air_temp, bike_use_atemp$count)

plot(atemp_bike_cor_bayes)

