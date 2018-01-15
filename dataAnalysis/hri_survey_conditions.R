######### HRI CLASS PROJECT ###############################
######### Survey Data Analysis ############################
######### Mai Lee Chang  ##################################
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
library(effsize)
library(lsr)
library(heplots)
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
## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\R_Exports")

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

### fluency
Alldata_aov = aov(fluency  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)


#etasq(Alldata_aov, anova = TRUE)

m1 <- ezANOVA(data = subdata, dv = fluency, wid = SID, within = .(MT, IR), type = 3, detailed = TRUE)
m1$ANOVA

#etaSquared(Alldata_aov)

ttest = aov(formula = fluency ~ MT * IR,data = subdata)
TukeyHSD(ttest)
plot(TukeyHSD(ttest, conf.level=.95))

### contribution
Alldata_aov = aov(contribution  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

m2 <- ezANOVA(data = subdata, dv = contribution, wid = SID, within = .(MT, IR), type = 3, detailed = TRUE)
m2$ANOVA

ttest = aov(formula = contribution ~ MT * IR,data = subdata)
TukeyHSD(ttest)


### capability
Alldata_aov = aov(cap  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

m3 <- ezANOVA(data = subdata, dv = cap, wid = SID, within = .(MT, IR), type = 3, detailed = TRUE)
m3$ANOVA

cohen.d(subdata$cap,subdata$IR,pooled=TRUE,paired=TRUE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

cohen.d(subdata$cap,subdata$MT,pooled=TRUE,paired=TRUE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

ttest = aov(formula = cap ~ MT * IR,data = subdata)
TukeyHSD(ttest)


### legibility
Alldata_aov = aov(leg  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)

m4 <- ezANOVA(data = subdata, dv = leg, wid = SID, within = .(MT, IR), type = 3, detailed = TRUE)
m4$ANOVA

ttest = aov(formula = leg ~ MT * IR,data = subdata)
TukeyHSD(ttest)


Alldata_aov = aov(ir  ~ (MT * IR) + Error(SID/ (MT * IR)),data = subdata)
summary(Alldata_aov, intercept = T)



###### Effect Size

cohen.d(subdata$leg,subdata$MT,pooled=TRUE,paired=TRUE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)


cohen.d(subdata$leg,subdata$IR,pooled=TRUE,paired=TRUE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

cohen.d(subdata$fluency,subdata$IR,pooled=TRUE,paired=TRUE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)


### Mean 
zscore_mean = function(var)
{
  mean(var)
}

# apply the zscore_mean transform to create new variables
mean_MT_IR = ddply(subdata,c("MT", "IR"),transform,fluency_mean=zscore_mean(fluency), contribution_mean=zscore_mean(contribution),
                        cap_mean=zscore_mean(cap),leg_mean=zscore_mean(leg),
                        ir_mean=zscore_mean(ir))
write.table(mean_MT_IR,file="mean_MT_IR.csv",sep = ",",row.names=F,quote=F)


### Std dev 
zscore_se = function(var)
{
  sd(var)/sqrt(length(var))
}

# apply the zscore_mean transform to create new variables
mean_se_MT_IR = ddply(mean_MT_IR,c("MT", "IR"),transform,fluency_se=zscore_se(fluency), contribution_se=zscore_se(contribution),
                   cap_se=zscore_se(cap),leg_se=zscore_se(leg),
                   ir_se=zscore_se(ir))
write.table(mean_se_MT_IR,file="mean_se_MT_IR.csv",sep = ",",row.names=F,quote=F)


### outdata
orgDat <- function (data){
  dataNew = data[1,]
  dataNew
}

outdata = ddply(mean_se_MT_IR, c("Program"), orgDat)
write.table(outdata,file="outdata.csv",sep = ",",row.names=F,quote=F)


################## plotdata
plotdata = outdata[, c("MT","IR", "fluency_mean", "contribution_mean", "cap_mean", "leg_mean", "ir_mean")]


meltdata <- melt(plotdata, id=c("MT","IR"))

levels(meltdata$variable)
new_variable = c("Team Fluency", "Robot Contribution", "Capability", "Legibility", "Intent Recognition")
levels(meltdata$variable) = new_variable

### rename column
colnames(meltdata)[3] <- "Dependent_Var"
colnames(meltdata)[4] <- "Mean"



### std error
plotdata_se = outdata[, c("MT","IR", "fluency_se", "contribution_se", "cap_se", "leg_se", "ir_se")]


meltdata_se <- melt(plotdata_se, id=c("MT","IR"))

levels(meltdata_se$variable)
new_variable_se = c("Team Fluency", "Robot Contribution", "Capability", "Legibility", "Intent Recognition")
levels(meltdata_se$variable) = new_variable_se

### rename column
colnames(meltdata_se)[3] <- "Dependent_Var"
colnames(meltdata_se)[4] <- "SE"


######## merge data

merge_data = merge(meltdata, meltdata_se, by = c("MT", "IR", "Dependent_Var"), all = TRUE)
write.table(merge_data,file="merge_data.csv",sep = ",",row.names=F,quote=F)
head(merge_data)




jpeg("final_survey.jpeg", quality=100, res=200, width=1200, height=900)
print(
ggplot(data=merge_data, aes(x=MT, y=Mean, fill=Dependent_Var)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Mean-SE, ymax=Mean+SE)) +
  scale_fill_manual("Dependent Var", values = c("Team Fluency" = "gray40", "Robot Contribution" = "gray65", "Capability" = "gray80", "Legibility" = "indianred1", "Intent Recognition" = "skyblue1")) +
  #scale_fill_manual("Dependent Var", values = c("Team Fluency" = "gray30", "Robot Contribution" = "gray40", "Capability" = "gray50", "Legibility" = "gray60", "Intent Recognition" = "gray70")) +
  ylim(0, 7) +
  theme_bw()+
  facet_grid(IR~.) + 
  #theme(axis.text=element_text(size=10)) +
  #opts(axis.text.x=theme_text(size=10), axis.title.x = theme_text(size=10, angle=0))+
  #opts(axis.text.y=theme_text(size=10), axis.title.y = theme_text(size=10, angle=90))+
  xlab("Motion Type") +
  ylab("Likert Rating")
)
dev.off()




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



