######### HRI CLASS PROJECT ###############################
######### Survey Data Analysis ############################
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
data = read.csv('Conditions.csv', header = T)

str(data)

data$SID = as.factor(data$SID)

### Rename levels
levels(data$fluency_team)
new_fluency_team = c("2", "3", "4", "5", "6", "7")
levels(data$fluency_team) = new_fluency_team

levels(data$fluency_robot)
new_fluency_robot = c("2", "3", "4", "5", "6", "7")
levels(data$fluency_robot) = new_fluency_robot

levels(data$contribution_human)
new_contribution_human = c("1", "2", "3", "4", "5", "6", "7")
levels(data$contribution_human) = new_contribution_human

levels(data$contribution_robot)
new_contribution_robot = c("2", "3", "4", "5", "6", "7")
levels(data$contribution_robot) = new_contribution_robot

levels(data$contribution_robot_perf)
new_contribution_robot_perf = c("3", "4", "5", "6", "7")
levels(data$contribution_robot_perf) = new_contribution_robot_perf

levels(data$cap_robot)
new_cap_robot = c("2", "3", "4", "5", "6", "7")
levels(data$cap_robot) = new_cap_robot

levels(data$cap_intelligent)
new_cap_intelligent = c("1", "2", "3", "4", "5", "6", "7")
levels(data$cap_intelligent) = new_cap_intelligent

levels(data$leg_reason)
new_leg_reason = c("2", "3", "4", "5", "6", "7")
levels(data$leg_reason) = new_leg_reason

levels(data$leg_easy_pred)
new_leg_easy_pred = c("2", "3", "4", "5", "6", "7")
levels(data$leg_easy_pred) = new_leg_easy_pred

levels(data$leg_move_intent)
new_leg_move_intent = c("2", "3", "4", "5", "6", "7")
levels(data$leg_move_intent) = new_leg_move_intent

levels(data$leg_move_reach)
new_leg_move_reach = c("1", "2", "3", "4", "5", "6", "7")
levels(data$leg_move_reach) = new_leg_move_reach

levels(data$ir_reason)
new_ir_reason = c("1", "2", "3", "4", "5", "6", "7")
levels(data$ir_reason) = new_ir_reason

levels(data$ir_confident)
new_ir_confident = c("1", "2", "3", "4", "5", "6", "7")
levels(data$ir_confident) = new_ir_confident

levels(data$ir_move)
new_ir_move  = c("1", "2", "3", "4", "5", "6", "7")
levels(data$ir_move ) = new_ir_move 

### reverse scale
# contribution_human
data$contribution_human = as.integer(data$contribution_human)
data$contribution_human = 8 -  data$contribution_human
data$contribution_human = as.factor(data$contribution_human)

levels(data$contribution_human)

### make ratings integers
data$fluency_team = as.integer(data$fluency_team)

data$fluency_robot = as.integer(data$fluency_robot)

data$contribution_human = as.integer(data$contribution_human)

data$contribution_robot = as.integer(data$contribution_robot)

data$contribution_robot_perf = as.integer(data$contribution_robot_perf)

data$cap_robot = as.integer(data$cap_robot)

data$cap_intelligent = as.integer(data$cap_intelligent)

data$leg_reason = as.integer(data$leg_reason)

data$leg_easy_pred = as.integer(data$leg_easy_pred)

data$leg_move_intent = as.integer(data$leg_move_intent)

data$leg_move_reach = as.integer(data$leg_move_reach)

data$ir_reason = as.integer(data$ir_reason)

data$ir_confident = as.integer(data$ir_confident)

data$ir_move = as.integer(data$ir_move)


### Check design for balance: Missing conditions, cases, or values
## Identifies the number of participants per group, 
ddply(data,
      .(data$Program),
      function(data) length(unique(data$SID)))

### Exclude SID#11
subdata = subset(data, SID != 11)

ddply(subdata,
      .(subdata$Program),
      function(subdata) length(unique(subdata$SID)))

### Define IVs (create new columns)
# A=Baseline
# B=IR
# C=MT
# D=Both

subdata$MT = "Legible"
subdata$IR = "IR Absent"


subdata[subdata$Program == "A", 17] = "Predictable"
subdata[subdata$Program == "B", 17] = "Predictable"

subdata[subdata$Program == "B", 18] = "IR Present"
subdata[subdata$Program == "D", 18] = "IR Present"

subdata$MT = as.factor(subdata$MT)
subdata$IR = as.factor(subdata$IR)

str(subdata)

### Add gender data
subdata$Gender = "Male"

subdata[subdata$SID == "2" | subdata$SID == "3" | subdata$SID == "6" | subdata$SID == "8" | subdata$SID == "12", 19] = "Female"

### Re-arrange columns
subdata = subdata[, c(1, 19, 2, 17, 18, 3:16)]

### Calculate scales by averaging the items 
subdata$fluency = (subdata$fluency_team + subdata$fluency_robot) / 2

subdata$contribution = (subdata$contribution_human + subdata$contribution_robot +  subdata$contribution_robot_perf) / 3

subdata$cap = (subdata$cap_robot + subdata$cap_intelligent) / 2

subdata$leg = (subdata$leg_reason + subdata$leg_easy_pred + subdata$leg_move_intent + subdata$leg_move_reach) / 4

subdata$ir = (subdata$ir_reason + subdata$ir_confident + subdata$ir_move) / 3



### Cronbach's alpha for scales
# fluency_scale = subdata[, c("fluency_team", "fluency_robot")]
# head(fluency_scale)
# alpha(fluency_scale, na.rm = TRUE)

# cronbach(data.frame(dat$item.1, dat$item.2, dat$item.3, dat$item.4))



### plots for each scale


ggplot(data=subdata, aes(x=MT, y=fluency, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 


jpeg("bar_fluency.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=fluency, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Fluency") 
)
dev.off()


jpeg("bar_contribution.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=contribution, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Robot Contribution") 
)
dev.off()


jpeg("bar_cap.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=cap, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Capability") 
)
dev.off()


jpeg("bar_leg.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=leg, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Legibility") 
)
dev.off()


jpeg("bar_ir.jpeg", quality=100, res=200, width=1200, height=700)
print(
  ggplot(data=subdata, aes(x=MT, y=ir, fill=SID)) +
    geom_bar(stat="identity", position=position_dodge())+
    ylim(0, 7) +
    theme_bw()+
    facet_grid(IR~.) + 
    xlab("Motion Type") +
    ylab("Intent_Recognition") 
)
dev.off()


ggplot(data=subdata, aes(x=MT, y=cap, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 

ggplot(data=subdata, aes(x=MT, y=leg, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 

ggplot(data=subdata, aes(x=MT, y=ir, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 


################################### Descriptive Statistics
summary(subdata)


### Summarize variables: Name, Type, Number missing, Unique values, Min, and Max
ezPrecis(subdata) # From ez library


### export data
## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\R_Exports")

write.table(subdata,file="subdata.csv",sep = ",",row.names=F,quote=F)



############# anova
### reference is: http://www.personality-project.org/r/r.guide.html#withinone

Alldata_aov = aov(fluency  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(contribution  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(cap  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(leg  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

Alldata_aov = aov(ir  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)




### plots for each question
ggplot(subdata, aes(x=Program, y= fluency_team, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=fluency_team, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=fluency_team, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y = fluency_robot, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y = fluency_robot, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=fluency_robot, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= contribution_human, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=contribution_human, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=contribution_human, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= contribution_robot, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=contribution_robot, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=contribution_robot, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 




ggplot(subdata, aes(x=Program, y= contribution_robot_perf, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=contribution_robot_perf, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=contribution_robot_perf, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= cap_robot, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=cap_robot, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=cap_robot, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= cap_intelligent, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=cap_intelligent, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=cap_intelligent, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 





ggplot(subdata, aes(x=Program, y= leg_reason, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=leg_reason, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=leg_reason, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= leg_easy_pred, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=leg_easy_pred, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=leg_easy_pred, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= leg_move_intent, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=leg_move_intent, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=leg_move_intent, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= leg_move_reach, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=leg_move_reach, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=leg_move_reach, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= ir_reason , color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=ir_reason , fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=ir_reason, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= ir_confident, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=ir_confident, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=ir_confident, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 



ggplot(subdata, aes(x=Program, y= ir_move, color = SID)) +
  geom_point(alpha = .7, position=position_dodge(width=.5))+
  theme_bw()

ggplot(data=subdata, aes(x=Program, y=ir_move, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()

ggplot(data=subdata, aes(x=MT, y=ir_move, fill=SID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  facet_grid(IR~.) 






# jpeg("bar_all.jpeg", quality=100, res=200, width=1200, height=700)
# print(
#   ggplot(subdata, aes(Program, ir_move))+
#     geom_bar(alpha = .8)
#     #theme_bw()+
#     #geom_text(aes(label=count, size=0.1), vjust=0) +
#     #opts(axis.text.x=theme_text(size=10), axis.title.x = theme_text(size=10, angle=0))+
#     #opts(axis.text.y=theme_text(size=10), axis.title.y = theme_text(size=10, angle=90))
#     #facet_grid(.~IR) +
#     #scale_size(guide="none") 
#     #xlab("Capability") +
#     #ylab("Number of manual paths") 
# )
# dev.off()
# 
# 
# 
# 
# jpeg("bar_all.jpeg", quality=100, res=200, width=1500, height=1000)
# print(
#   ggplot(data, aes(capability, searchTask_mean_mean, size = 1)) +
#     geom_line(aes(group=transparency,linetype = transparency), size = 1) +
#     geom_errorbar(limits, width=0.2, alpha = 0.3, size = 1) +
#     geom_point(aes(shape=transparency)) +
#     scale_shape_manual(values=c(1,16)) + 
#     scale_linetype_manual(values=c("solid", "dotdash")) +
#     scale_size(guide="none") +
#     facet_grid(.~Program) +
#     theme_bw()+
#     opts(axis.text.x=theme_text(size=10), axis.title.x = theme_text(size=10, angle=0))+
#     opts(axis.text.y=theme_text(size=10), axis.title.y = theme_text(size=10, angle=90))+
#     xlab("Capability") +
#     ylab("Search Task Score")
# )
# dev.off()