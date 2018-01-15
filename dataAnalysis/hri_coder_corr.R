######### HRI CLASS PROJECT ###############################
######### Coder Agreement Calc ############################
######### 12/3/2017 #######################################

### Load libraries
library(reshape)
library(plyr)
library(ggplot2)
library(corrgram)
library(car)
library(zoo)
library(ez)
library(sciplot)
library (nlme)
library(Hmisc)
library(mice)
library(lme4)
library(languageR)
#library(psy)
library(irr)

rm(list = ls(all = TRUE))


## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\Objective Data")

### Read data
data = read.csv('coders_corr.csv', header = T)

str(data)

#http://www.statisticshowto.com/intraclass-correlation/
icc(data, model="oneway", type="agreement")


###
summary(data)

### Summarize variables: Name, Type, Number missing, Unique values, Min, and Max
ezPrecis(data) # From ez library
