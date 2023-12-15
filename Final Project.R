#Libraries
library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
#library(MASS)

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
model_df <- cbind(df$LBXAPB,scaled_df,dummy_df,indicator_df)
colnames(model_df)[1] <- "LBXAPB"

# Build the main linear regression model and check residuals
model_main <- lm(LBXAPB ~ ., data = model_df)
summary(model_main)
par(mfrow = c(2, 2))
plot(model_main)


# Stepwise model selection
full_model <- lm(LBXAPB ~ ., data = model_df)

library(MASS)
stepwise_model <- stepAIC(full_model, direction = "both")
summary(stepwise_model)

# ElasticNet

x <- model.matrix(~ avgSBP + MCQ370A + BMXBMI + SMQ020 + LBXTC + LBXGH + LBXSTR + HUQ051 + HUQ010, data = model_df)[,-1]
y <- model_df$LBXAPB

cv_model_full <- cv.glmnet(x, y, alpha = 0.5) 
plot(cv_model_full)
#Optimal lambda is chosen based on the cross-validation result.I think this should be 2 here but will double check.


#Random Forest Analysis
set.seed(715)

#dichotomize ApoB to < 100 vs >= 100
rf_df <- df
rf_df$APOB_cat <- ""
rf_df$APOB_cat[rf_df$LBXAPB >= 100] <- 1
rf_df$APOB_cat[rf_df$LBXAPB < 100] <- 0
rf_df$APOB_cat <- factor(rf_df$APOB_cat)
rf_df <- rf_df %>% select(-c(X,LBXAPB))

#Split into 4:1 test/train data
train_index <- createDataPartition(rf_df$APOB_cat, p=0.8, list=F)
train_df <- rf_df[train_index,]
test_df <- rf_df[-train_index,]

#Train RF Model
rf_model <- randomForest(APOB_cat~.,data=train_df,ntree=500,importance=T)

#Predictions on Test Dataset
predictions <- predict(rf_model,newdata = test_df)

#Confusion Matrix
conf_matrix <- confusionMatrix(predictions,test_df$APOB_cat)

conf_matrix

#Importance Plot
importance_values <- data.frame(importance(rf_model)) %>% rownames_to_column("Variable")
importance_values$Variable <- factor(importance_values$Variable,levels = importance_values$Variable[order(importance_values$MeanDecreaseGini,decreasing = F)])
importance_values

importance_values %>% ggplot(aes(y=MeanDecreaseGini,x=Variable)) +
  geom_bar(stat="identity",fill="blue") +
  coord_flip() +
  labs(title = "Variable Importance")+
  theme(plot.title = element_text(hjust = 0.5))

  