##########################################
######Math Fluency Analysis Script########
######Author: Stephanie###################
######Updated: Oct 04 2017################
##########################################

#Install needed packages
install.packages("psych")
install.packages("dplyr")
install.packages("sem")
install.packages("data.table")
install.packages("xlsx")
install.packages("magrittr")
install.packages("normtest")
install.packages("Hmisc")
install.packages("plyr")

#Make packages active
library(psych)
library(dplyr)
library(sem)
library(data.table)
library(xlsx)
library(magrittr)
library(normtest)
library(Hmisc)
library(plyr)

#Find my file and read from it
setwd("/Volumes/PEGASUS/Projects/Admin/NSF_SLCN/MathFluency")   #Other users will need to change this to their directory
my.data <- read.csv(file = "mathFluency.csv",header=TRUE)

#How many participants?
length(unique(my.data$UserID))

#####################################
######Cleaning things up a bit#######
#####################################

######Calculate Raw Score Using Accuracy######
#Time 1
t1scores <- my.data %>%
  filter(Wave==1) %>%
  group_by(UserID,CurrentDate,CurrentYear,Semester,Grade,Age,
           Handedness,Gender,SEAGroup,iPadNumber) %>% 
  dplyr::summarise(endtime = max(FeedbackOnset),rawscoret1 = sum(Accuracy)) 
#find out also time they discontinued
#Time 2
t2scores <- my.data %>%
  filter(Wave==2) %>%
  group_by(UserID,CurrentDate,CurrentYear,Wave,Semester,Grade,Age,
           Handedness,Gender,SEAGroup,iPadNumber) %>%
  dplyr::summarise(endtime2 = max(FeedbackOnset),rawscoret2 = sum(Accuracy)) 
#find out also time they discontinued

######Check for trials where users ended early######
#Filter out those who meet criteria, leaving those who ended early
##Defining early here as ending 15 seconds (15,000 ms) before 3 min
##This means discard trials with max time of 165,000 ms or less
t1scores <- t1scores %>% filter(endtime > 165000)
t2scores <- t2scores %>% filter(endtime2 > 165000)

######Rename Time 1 Labels######
t1scores <- plyr::rename(t1scores,c("CurrentDate" = "CurrentDatet1", "CurrentYear" = "CurrentYeart1",
                                    "Semester" = "Semestert1", "Grade" = "Gradet1", "Age" = "Aget1",
                                    "iPadNumber" = "iPadNumbert1","endtime"="endtimet1",
                                    "Gender" = "Gendert1", "Handedness" = "Handednesst1"))

######Rename Time 2 Labels######
t2scores <- plyr::rename(t2scores,c("CurrentDate" = "CurrentDatet2", "CurrentYear" = "CurrentYeart2",
                                    "Semester" = "Semestert2", "Grade" = "Gradet2", "Age" = "Aget2",
                                    "iPadNumber" = "iPadNumbert2","Gender" = "Gendert2", 
                                    "Handedness" = "Handednesst2"))

#####################################
######Merging the two timepoints#####
#####################################

#merge them by UserID
master.data <- merge(t1scores,t2scores,by="UserID",all = TRUE) 
#using ALL here returns NA for values that don't match

#check for mismatches
master.data$match <- ifelse((master.data$Gendert1 != master.data$Gendert2),"mismatch_gender",
                            ifelse((master.data$Gradet1 != master.data$Gradet2), "mismatch_grade","match"))

#####################################
######Export Resulting CSV File######
#####################################

write.csv(master.data,file = "MathFluency_ForAnalysis.csv",na="")








