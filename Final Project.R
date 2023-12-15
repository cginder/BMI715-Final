#Libraries
library(tidyverse)
library(glmnet)
library(MASS)

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

# Build the main linear regression model and check residuals
model_main <- lm(LBXAPB ~ avgSBP + MCQ370A + BMXBMI + SMQ020 + LBXTC + LBXGH + LBXSTR + HUQ051 + HUQ010, data = df)
summary(model_main)
par(mfrow = c(2, 2))
plot(model)


# Stepwise model selection
full_model <- lm(LBXAPB ~ avgSBP + MCQ370A + BMXBMI + SMQ020 + LBXTC + LBXGH + LBXSTR + HUQ051 + HUQ010, data = df)

stepwise_model <- stepAIC(full_model, direction = "both")
summary(stepwise_model)

# ElasticNet

x <- model.matrix(~ avgSBP + MCQ370A + BMXBMI + SMQ020 + LBXTC + LBXGH + LBXSTR + HUQ051 + HUQ010, data = df)[,-1]
y <- df$LBXAPB

cv_model_full <- cv.glmnet(x, y, alpha = 0.5) 
plot(cv_model_full)
#Optimal lambda is chosen based on the cross-validation result.I think this should be 2 here but will double check.
     