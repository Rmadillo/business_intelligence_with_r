#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 5 Code: Effect Sizes
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_5_Code.R
#
######################################################################


# install JAGS from http://mcmc-jags.sourceforge.net/ 

# install.packages("rjags")
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

##### End of File #####
