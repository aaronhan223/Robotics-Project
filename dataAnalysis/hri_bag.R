######### HRI CLASS PROJECT ###############################
######### Bag Data  #######################################
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
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\Objective Data")

### Read data
data = read.csv('bag.csv', header = T)

str(data)

data$SID = as.factor(data$SID)
data$Cup = as.factor(data$Cup)


### Check design for balance: Missing conditions, cases, or values
## Identifies the number of participants per group, 
ddply(data,
      .(data$Condition),
      function(data) length(unique(data$SID)))

###
summary(data)

### Summarize variables: Name, Type, Number missing, Unique values, Min, and Max
ezPrecis(data) # From ez library


### Define IVs (create new columns)
# A=Baseline
# B=IR
# C=MT
# D=Both

data$MT = "Legible"
data$IR = "IR Absent"


data[data$Condition == "a", 10] = "Predictable"
data[data$Condition == "b", 10] = "Predictable"

data[data$Condition == "b", 11] = "IR Present"
data[data$Condition == "d", 11] = "IR Present"

data$MT = as.factor(data$MT)
data$IR = as.factor(data$IR)

str(data)


### Re-arrange columns
data = data[, c(1, 2, 10, 11, 3:9)]


### Calc avg for cup1 and cup2
# export data
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\R_Exports")

avgT <- function(data)
{
  data$dt_hms_rir_avg = 0
  for (i in 1:(nrow(data)-1)){
    data[i, ]$dt_hms_rir_avg = (data[i+1, ]$dt_hms_rir + data[i,]$dt_hms_rir)/2
  }
  data
}

data_rev1 = ddply(data, c("SID", "Condition"), avgT)
write.table(data_rev1,file="data_rev1.csv",sep = ",",row.names=F,quote=F)


avgT2 <- function(data)
{
  data$dt_rir_rms_avg = 0
  
  for (i in 1:(nrow(data)-1)){
    data[i, ]$dt_rir_rms_avg = (data[i+1, ]$dt_rir_rms + data[i,]$dt_rir_rms)/2
  }
  data
}

data_rev2 = ddply(data_rev1, c("SID", "Condition"), avgT2)
write.table(data_rev2,file="data_rev2.csv",sep = ",",row.names=F,quote=F)


avgT3 <- function(data)
{
  data$dt_hms_rms_avg = 0
  
  for (i in 1:(nrow(data)-1)){
    data[i, ]$dt_hms_rms_avg = (data[i+1, ]$dt_hms_rms + data[i,]$dt_hms_rms)/2
  }
  data
}

data_rev3 = ddply(data_rev2, c("SID", "Condition"), avgT3)
write.table(data_rev3,file="data_rev3.csv",sep = ",",row.names=F,quote=F)


### subset
subdata = subset(data_rev3, Cup == "1")
write.table(subdata,file="subdata.csv",sep = ",",row.names=F,quote=F)


############# anova
### reference is: http://www.personality-project.org/r/r.guide.html#withinone

Alldata_aov = aov(dt_hms_rir_avg  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(dt_rir_rms_avg  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(dt_hms_rms_avg  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)


############ plots
## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\R_Exports")

jpeg("bar_dt_hms_rir_avg.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=dt_hms_rir_avg , fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Avg delta time: human motion start to robot IR (sec)") 
)
dev.off()


jpeg("bar_dt_rir_rms_avg.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=dt_rir_rms_avg, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Avg delta time: robot IR to robot motion starts (sec)") 
)
dev.off()


jpeg("bar_dt_hms_rms_avg.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=dt_hms_rms_avg, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Avg delta time: human motion starts to robot motion starts (sec)") 
)
dev.off()




