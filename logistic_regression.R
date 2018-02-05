##########################################################

##### Logistic Regression
##### Data: Credit card default, 10000 obs, 4 variables

#########################################################


# 1. Import data

library(ISLR)
library(MASS)
attach(Default)

head(Default)

?Default
summary(Default)

# 2. Build a simple logistic regression model

model.1 <- glm(default~income, data=Default,family=binomial())

summary(model.1)

model.1.pred.prob <- predict(model.1,type="response")

plot(model.1.pred.prob,default)

plot(model.1.pred.prob,jitter(as.numeric(default)))

# 3. Split data

set.seed(110)

sub <- sample(nrow(Default), floor(nrow(Default) * 0.6))

training_data <- Default[sub,]

validation_data <- Default[-sub,]


# 4. Build a multiple regression model

model.stock <- glm(default~.,data=training_data,family=binomial())

summary(model.stock)

model.step <- stepAIC(model.stock)

summary(model.step)

# 5. Predict on training and validation files

training_data$pred.prob <- predict(model.step,type="response") 

validation_data$pred.prob <- predict(model.step,type="response",newdata=validation_data) 

plot(training_data$pred.prob,jitter(as.numeric(training_data$default)))

plot(validation_data$pred.prob,jitter(as.numeric(validation_data$default)))

plot(jitter(as.numeric(validation_data$balance)),validation_data$pred.prob)

# 6 Confusion matrix and ROC

# 6.1 Confusion matrix
training_data$class <- ifelse(training_data$pred.prob>.05,1,0)

validation_data$class <- ifelse(validation_data$pred.prob>.05,1,0)

table(training_data$default,training_data$class)

table(validation_data$default,validation_data$class)

# 6.2 ROC

##install.packages("ROCR")
library(ROCR)

train_pred <- prediction(training_data$pred.prob, training_data$default)

train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr") 

plot(train_perf, col=rainbow(5))

val_pred <- prediction(validation_data$pred.prob, validation_data$default)

val_perf <- performance(val_pred, measure = "tpr", x.measure = "fpr") 

plot(val_perf, col=rainbow(10))