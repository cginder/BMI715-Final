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

#Exploratory Analysis/QC
df %>% select(-c(X,LBXAPB)) %>% plot()
df %>% select(-c(X,LBXAPB)) %>% cor()

#MCQ370A and MCQ370B highly correlated at 0.47, remove MCQ370B
#MCQ370A and BMI negatively correlated at -.23, keep
#Triglycerides and TC correlated at 0.35, keep due to pathophysiologic plausability
#HUQ010 and HUQ051 correlated at 0.22, keep
df <- df %>% select(-MCQ370B)

#Remove Outliers in LBXTC and LBXSTR
head(sort(df$LBXTC,decreasing=T))
head(sort(df$LBXSTR,decreasing=T))

df <- df %>% filter(LBXTC != 612 | LBXSTR != 6057)

#Scale Continuous Variables
scaled_df <- df %>% select(-c(X,LBXAPB,MCQ370A,SMQ020,HUQ051,HUQ010)) %>% scale()

#Set Dummy Variables to 0/1 instead of 1/2
dummy_df <- df  %>% select(c(MCQ370A,SMQ020)) 
dummy_df$MCQ370A <- dummy_df$MCQ370A - 1
dummy_df$SMQ020 <- dummy_df$SMQ020 - 1

#Create Multiple Indicator Variables
df$HUQ010 <- factor(df$HUQ010)
df$HUQ051 <- factor(df$HUQ051)
indicator_df <- df %>% select(c(HUQ051, HUQ010)) %>%
  data.frame(., model.matrix( ~ HUQ010 + HUQ051, .)[, -1]) %>%
  select(!c(HUQ051, HUQ010))

#Combine back into dataframe
model_df <- cbind(df[,c("X","LBXAPB")],scaled_df,dummy_df,indicator_df)
                         
                         


     