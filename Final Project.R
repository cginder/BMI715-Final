#Libraries
library(tidyverse)

#Read Filtered Data
filtered_df <- read.csv("Filtered Data.csv")
filtered_df <- filtered_df[,-1] #remove rowID read in

#Selection of Indepenent Variables
variables <- c(  "X", #Respondent ID
                 "LBXAPB", #Apo B, dependent variable
                 "avgSBP", #Calculated Average SBP during processing script
                 "MCQ370A", #Controlling or losing weight?
                 "MCQ370B", #Increasing exercise?
                 "BMXBMI", #BMI
                 "SMQ020", #Tobacco Use
                 "LBXTC", #Total Cholesterol
                 "LBXGH", #A1c
                 "LBXSTR", #Triglycerides
                 "HUQ051", #Number of times received healthcare over last year
                 "HUQ010") #Health Quality 1-5

df <- filtered_df %>% select(any_of(variables))

#



plot(df[,-c(1:2)]) #excluding PatientID & ApoB
