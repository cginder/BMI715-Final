#Libraries
library(tidyverse)
library(ggplot2)
library(tableone)

#SetWD
setwd("~/Documents/GitHub/BMI715Final")

#Read Data
rawdata <- read.csv("nhanes_13_14_subset.csv")

#Candidate Variables
candidates <- c("X", #Respondent ID
                "LBXAPB", #Apo B, dependent variable
                "BPXSY1", #SBP1
                "BPXSY2", #SBP2
                "BPXSY3", #SBP3
                "BPXSY4", #SBP4
                "BPXDI1", #DBP1
                "BPXDI2", #DBP2
                "BPXDI3", #DBP3
                "BPXDI4", #DBP4
                "MCQ370A", #Controlling or losing weight?
                "MCQ370B", #Increasing exercise?
                "BMXBMI", #BMI
                "SMQ020", #Tobacco Use
                "LBXTC", #Total Cholesterol
                "LBXGH", #A1c
                "LBXSTR", #Triglycerides
                "LBXSCR", #Creatinine
                "HUQ051", #Number of times received healthcare over last year
                "HUQ010", #Health Quality 1-5
                "BPQ100D", #Currently taking cholesterol med?
                "BPQ020", #Told high blood pressure?
                "BPQ030", #Told high blood pressure more than once?
                "BPQ040A", #Taking a prescription for HTN
                "BPXPLS", #Pulse
                "BPQ090D", #Cholesterol med?
                "HUQ071", #Number of admissions in last year?
                "MCQ160E", #Prior heart attack?
                "MCQ160F", #Prior stroke?
                "MCQ370C", #Are you reducing salt in diet?
                "SMQ040") #Currently smoke cigarettes?
 

#Filter to candidate indepent variables and include only those with an ApoB Measurement
data <- rawdata %>% select(any_of(candidates)) %>% filter(!is.na(LBXAPB))

#Assess Respondent Coverage by Variable
#Categorical Variables
cat_vars <- c("MCQ370A", #Controlling or losing weight?
             "MCQ370B", #Increasing exercise?
             "SMQ020", #Tobacco Use
             "HUQ051", #Number of times received healthcare over last year
             "HUQ010", #Health Quality 1-5
             "BPQ100D", #Currently taking cholesterol med?
             "BPQ020", #Told high blood pressure?
             "BPQ030", #Told high blood pressure more than once?
             "BPQ040A", #Taking a prescription for HTN
             "BPQ090D", #Cholesterol med?
             "HUQ071", #Number of admissions in last year?
             "MCQ160E", #Prior heart attack?
             "MCQ160F", #Prior stroke?
             "MCQ370C", #Are you reducing salt in diet?
             "SMQ040") #Currently smoke cigarettes?

table <- CreateTableOne(data=data, vars=candidates,factorVars = cat_vars)

summary(table)

#Remove Low Coverage Variables (More than 25% Missing)
low_coverage_vars <- c("BPQ100D", #Currently taking cholesterol med?
                      "BPQ030", #Told high blood pressure more than once?
                      "BPQ090D", #Cholesterol med?
                      "SMQ040") #Currently smoke cigarettes?

data <- data %>% select(!any_of(low_coverage_vars))                 

#Combine BPs into one average reading




#Keep only complete cases


complete_data <- data %>% filter(complete.cases(.))
