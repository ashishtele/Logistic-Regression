###################################################
############ Salary determination #################
###################################################

rm(list = ls())

loadlb <- function()
{
  library(tidyr)
  library(dplyr)
  library(ROCR)
  library(readxl)
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

## Build logit model

logitmd <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM,data = training_data, family = binomial(link = "logit"))

predicted <- plogis(predict(logitmd,test_data)) #
#or
predicted_1 <- predict(logitmd,test_data,type = "response") #log(odd)
plot(predicted_1)

## decide the optimal prediction probability cutoff

install.packages("InformationValue")
library(InformationValue)

optcut <- optimalCutoff(test_data$ABOVE50K,predicted_1)[1]
optcut
summary(logitmd)


## VIF check
library(car)
vif(logitmd)

## misclassification error
misClassError(test_data$ABOVE50K,predicted_1,threshold = optcut)

## ROC

plotROC(test_data$ABOVE50K,predicted_1)

## Concordance

Concordance(test_data$ABOVE50K, predicted_1)

## sensitivity

sensitivity(test_data$ABOVE50K, predicted_1, threshold = optcut)

confusionMatrix(test_data$ABOVE50K, predicted_1, threshold = optcut)
