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
data = read.csv('better_program.csv', header = T)

str(data)


### export data
## Set working directory
setwd("C:\\Users\\Owner\\Desktop\\Fall 2017 Classes\\HRI\\Project\\Data\\R_Exports")


jpeg("final_betterProgram.jpeg", quality=100, res=200, width=600, height=600)
print(
  ggplot(data=data, aes(x=MT, y=better)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.5)+
    #scale_fill_manual("Dependent Var", values = c("better" = "gray50")) +
    guides(fill=FALSE)+
    #scale_fill_manual(values = c("Total Task Time" = "gray50")) +
    #scale_fill_manual("Dependent Var", values = c("Team Fluency" = "gray30", "Robot Contribution" = "gray40", "Capability" = "gray50", "Legibility" = "gray60", "Intent Recognition" = "gray70")) +
    #ylim(0, 7) +
    theme_bw()+
    scale_size(guide="none") +
    scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6))+
    facet_grid(IR~.) + 
    #theme(axis.text=element_text(size=10)) +
    xlab("Motion Type") +
    ylab("Significantly Better Program (Count)")
)
dev.off()
