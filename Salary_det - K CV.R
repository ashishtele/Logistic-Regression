rm(list = ls())

loadlb <- function()
{
  library(tidyr)
  library(dplyr)
  library(ROCR)
  library(readxl)
  library(caret)
}

loadlb()

## reading dataset 'adult'

adult <- read.csv("E:\\Study\\R Projects\\Vehicles/adult.csv")
tbl_df(adult)
glimpse(adult)

## Checking bias class - The proportion of events and non-events 
# in Y should appro. be the same 

table(adult$ABOVE50K)
# - there is bias

## Train and testing datasets

i_one <- adult[adult$ABOVE50K == 1,] #all 1
i_zero <-adult[adult$ABOVE50K == 0,] #all 0

set.seed(1992) #for repeatability

i_one_training <- sample(1:nrow(i_one),0.7*nrow(i_one))
i_zero_training <- sample(1:nrow(i_zero),0.7*nrow(i_zero)) # equale 0 and 1

training_one <- adult[i_one_training,]
training_zero <- adult[i_zero_training,]

training_data <- rbind(training_one,training_zero)

## Test data

test_one <- adult[-i_one_training,]
test_zero <- adult[-i_zero_training,]

test_data <- rbind(test_one,test_zero)

## k fold cross validation 

train_contrl <- trainControl(method = "cv", number = 10)

attach(adult)
model <- train(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM,
               data = training_data,trControl = train_contrl, method = "glm", family = "binomial")


summary(model)

predicted <- predict(model,newdata = test_data)


## decide the optimal prediction probability cutoff

##install.packages("InformationValue")
library(InformationValue)

optcut <- optimalCutoff(test_data$ABOVE50K,predicted)[1]
optcut

## VIF check
library(car)
vif(model)

## misclassification error
misClassError(test_data$ABOVE50K,predicted,threshold = optcut)

## ROC

plotROC(test_data$ABOVE50K,predicted)

## Concordance

Concordance(test_data$ABOVE50K, predicted)

## sensitivity

sensitivity(test_data$ABOVE50K, predicted, threshold = optcut)

confusionMatrix(test_data$ABOVE50K, predicted, threshold = optcut)



