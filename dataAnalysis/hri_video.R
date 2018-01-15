######### HRI CLASS PROJECT ###############################
######### Video Data  ############################
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
data = read.csv('video_all.csv', header = T)

str(data)

data$SID = as.factor(data$SID)


### Check design for balance: Missing conditions, cases, or values
## Identifies the number of participants per group, 
ddply(data,
      .(data$Condition),
      function(data) length(unique(data$SID)))


### replace NAs with 0
data[is.na(data$concurrent_cup_c1), c("concurrent_cup_c1")] = 0
data[is.na(data$concurrent_cup_c2), c("concurrent_cup_c2")] = 0



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


### Calc total time
data$human_predicts_tot = data$human_predicts_c1 + data$human_predicts_c2

data$concurrent_cup_tot = data$concurrent_cup_c1 + data$concurrent_cup_c2

data$concurrent_bin_tot = data$conconcurrent_bin_c1 + data$conconcurrent_bin_c2


############# anova
### reference is: http://www.personality-project.org/r/r.guide.html#withinone

Alldata_aov = aov(task_duration  ~ (MT * IR) + Error(SID/ (MT * IR)),data = data)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(human_predicts_tot  ~ (MT * IR) + Error(SID/ (MT * IR)),data = data)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(concurrent_cup_tot  ~ (MT * IR) + Error(SID/ (MT * IR)),data = data)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(concurrent_bin_tot  ~ (MT * IR) + Error(SID/ (MT * IR)),data = data)
summary(Alldata_aov, intercept = T)


############ plots
## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\R_Exports")


### Mean 
zscore_mean = function(var)
{
  mean(var)
}

# apply the zscore_mean transform to create new variables
mean_MT_IR = ddply(data,c("MT", "IR"),transform,task_duration_mean=zscore_mean(task_duration), human_predicts_tot_mean=zscore_mean(human_predicts_tot),
                   concurrent_cup_tot_mean=zscore_mean(concurrent_cup_tot),concurrent_bin_tot_mean=zscore_mean(concurrent_bin_tot))
write.table(mean_MT_IR,file="mean_MT_IR.csv",sep = ",",row.names=F,quote=F)


### Std dev 
zscore_se = function(var)
{
  sd(var)/sqrt(length(var))
}

# apply the zscore_mean transform to create new variables
mean_se_MT_IR = ddply(mean_MT_IR,c("MT", "IR"),transform,task_duration_se=zscore_se(task_duration), human_predicts_tot_se=zscore_se(human_predicts_tot),
                      concurrent_cup_tot_se=zscore_se(concurrent_cup_tot),concurrent_bin_tot_se=zscore_se(concurrent_bin_tot))
write.table(mean_se_MT_IR,file="mean_se_MT_IR.csv",sep = ",",row.names=F,quote=F)


### outdata
orgDat <- function (data){
  dataNew = data[1,]
  dataNew
}

outdata = ddply(mean_se_MT_IR, c("Condition"), orgDat)
write.table(outdata,file="outdata.csv",sep = ",",row.names=F,quote=F)


################## plotdata
plotdata = outdata[, c("MT","IR", "human_predicts_tot_mean", "concurrent_cup_tot_mean", "concurrent_bin_tot_mean")]


meltdata <- melt(plotdata, id=c("MT","IR"))

levels(meltdata$variable)
new_variable = c("Human Prediction Time", "Concurrent Time (Cups)", "Concurrent Time (Bins)")
levels(meltdata$variable) = new_variable

### rename column
colnames(meltdata)[3] <- "Dependent_Var"
colnames(meltdata)[4] <- "Mean"


### std error
plotdata_se = outdata[, c("MT","IR", "human_predicts_tot_se", "concurrent_cup_tot_se", "concurrent_bin_tot_se")]


meltdata_se <- melt(plotdata_se, id=c("MT","IR"))

levels(meltdata_se$variable)
new_variable_se = c("Human Prediction Time", "Concurrent Time (Cups)", "Concurrent Time (Bins)")
levels(meltdata_se$variable) = new_variable_se

### rename column
colnames(meltdata_se)[3] <- "Dependent_Var"
colnames(meltdata_se)[4] <- "SE"


######## merge data

merge_data = merge(meltdata, meltdata_se, by = c("MT", "IR", "Dependent_Var"), all = TRUE)
write.table(merge_data,file="merge_data.csv",sep = ",",row.names=F,quote=F)
head(merge_data)




jpeg("final_humanPred_concurrent.jpeg", quality=100, res=200, width=1200, height=900)
print(
  ggplot(data=merge_data, aes(x=MT, y=Mean, fill=Dependent_Var)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Mean-SE, ymax=Mean+SE)) +
    scale_fill_manual("Dependent Var", values = c("Human Prediction Time" = "gray40", "Concurrent Time (Cups)" = "gray60", "Concurrent Time (Bins)" = "gray80")) +
    #scale_fill_manual("Dependent Var", values = c("Team Fluency" = "gray30", "Robot Contribution" = "gray40", "Capability" = "gray50", "Legibility" = "gray60", "Intent Recognition" = "gray70")) +
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    #theme(axis.text=element_text(size=10)) +
    #opts(axis.text.x=theme_text(size=10), axis.title.x = theme_text(size=10, angle=0))+
    #opts(axis.text.y=theme_text(size=10), axis.title.y = theme_text(size=10, angle=90))+
    xlab("Motion Type") +
    ylab("Time (sec)")
)
dev.off()

####################################### total task time
plotdata2 = outdata[, c("MT","IR", "task_duration_mean")]


meltdata2 <- melt(plotdata2, id=c("MT","IR"))

levels(meltdata2$variable)
new_variable2 = c("Total Task Time")
levels(meltdata2$variable) = new_variable2

### rename column
colnames(meltdata2)[3] <- "Dependent_Var"
colnames(meltdata2)[4] <- "Mean"


### std error
plotdata_se2 = outdata[, c("MT","IR", "task_duration_se")]


meltdata_se2 <- melt(plotdata_se2, id=c("MT","IR"))

levels(meltdata_se2$variable)
new_variable_se2 = c("Total Task Time")
levels(meltdata_se2$variable) = new_variable_se2

### rename column
colnames(meltdata_se2)[3] <- "Dependent_Var"
colnames(meltdata_se2)[4] <- "SE"


######## merge data
merge_data2 = merge(meltdata2, meltdata_se2, by = c("MT", "IR", "Dependent_Var"), all = TRUE)
write.table(merge_data2,file="merge_data2.csv",sep = ",",row.names=F,quote=F)
head(merge_data2)


jpeg("final_taskTime.jpeg", quality=100, res=200, width=600, height=900)
print(
  ggplot(data=merge_data2, aes(x=MT, y=Mean, fill=Dependent_Var)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.5)+
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Mean-SE, ymax=Mean+SE)) +
    scale_fill_manual("Dependent Var", values = c("Total Task Time" = "gray50")) +
    guides(fill=FALSE)+
    #scale_fill_manual(values = c("Total Task Time" = "gray50")) +
    #scale_fill_manual("Dependent Var", values = c("Team Fluency" = "gray30", "Robot Contribution" = "gray40", "Capability" = "gray50", "Legibility" = "gray60", "Intent Recognition" = "gray70")) +
    #ylim(0, 7) +
    theme_bw()+
    scale_size(guide="none") +
    facet_grid(IR~.) + 
    #theme(axis.text=element_text(size=10)) +
    #opts(axis.text.x=theme_text(size=10), axis.title.x = theme_text(size=10, angle=0))+
    #opts(axis.text.y=theme_text(size=10), axis.title.y = theme_text(size=10, angle=90))+
    xlab("Motion Type") +
    ylab("Total Task Time (sec)")
)
dev.off()



######################################

jpeg("bar_total task time.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=data, aes(x=MT, y=task_duration, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Total Task Time (sec)") 
)
dev.off()


jpeg("bar_human_predicts.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=data, aes(x=MT, y=human_predicts_tot, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Human Predicts (sec)") 
)
dev.off()


jpeg("bar_concurrent_cup.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=data, aes(x=MT, y=concurrent_cup_tot, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Concurrent Motion for Cups (sec)") 
)
dev.off()


jpeg("bar_concurrent_bin.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=data, aes(x=MT, y=concurrent_bin_tot, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    #ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Concurrent Motion for Bins (sec)") 
)
dev.off()




