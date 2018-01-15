######### HRI CLASS PROJECT ###############################
######### Survey Data Analysis: Final Questionnaire #######
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

rm(list = ls(all = TRUE))


## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\Survey Data")

### Read data
data = read.csv('Final.csv', header = T)

str(data)

data$SID = as.factor(data$SID)

### subset of first question
revdata = data[, c(1:2)]



### Exclude SID#11
subdata = subset(revdata, SID != 11)



### Check design for balance: Missing conditions, cases, or values
## Identifies the number of participants per group, 
ddply(subdata,
      .(subdata$programs),
      function(subdata) length(unique(subdata$SID)))



### Chi-square test
subdata2 = table (subdata$programs)
subdata2
chisq.test(subdata2)


