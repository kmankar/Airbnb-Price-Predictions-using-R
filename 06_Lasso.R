#############################################                                                                       
#   Project: Airbnb Price Prediction
#   Group#: 6
#   Script: Lasso Regression
#
#############################################

######################                                                                         
#   Libraries        (REDUNDANT LOAD)                                             
#                                                                        
######################
library(leaps)
library(ISLR)
library(glmnet)
library(devtools)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(GGally)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(scales)
library(stringr)
library(zipcode)
library(leaflet)
library(extracat)
library(gridExtra)
library(data.table)
library(tidyverse)
library(R.utils)
library(glmnet)
library(sparklyr)
library(sentimentr)
library(Metrics)
library(xgboost)
library(data.table)
library(mlr)
library(dummies)
library(car)
library(ggplot2)
library(ggthemes)
library(neuralnet)
library(keras)
install_keras()
library(tensorflow)


######################                                                                         
#    Ingesting Dataset                                                    
#                                                                         
######################
df <- full.data.model

######################                                                                         
# Preprocessing for                                    
#   XGBoost                                                      
######################
set.seed(71943)
splitter <- sample(nrow(df), 0.3*nrow(df))
df.train <- df[-splitter,]
df.test <- df[splitter,]

x <- sparse.model.matrix(~.,df.train[,-26])
x2 <- sparse.model.matrix(~.,df.test[,-26])
y <- df.train$price
y2 <- df.test$price

grid <- 10^seq(10,-2,length=100)
lasso.mod <- glmnet(x, y)#, lambda = grid)

plot(lasso.mod, label = T)
print(lasso.mod)
coef(lasso.mod, s = 0.5)

###############################
#     PLOTS
#
###############################
matplot(log(grid),t(coef(lasso.mod)[-1,]), type = c("l"),pch=1,col = "black", xlab="log(lambda)",ylab="Coefficients")

###############################
#     MIN LAMBDA
#
###############################
cvfit <- cv.glmnet(x,y)
plot(cvfit)

(bestlam <- cvfit$lambda.min)

as.matrix(coef(cvfit, s = "lambda.min"))
as.matrix(coef(cvfit, s = "lambda.1se"))

###############################
#     LASSO MODEL
#
###############################
lassofit1 <- predict(cvfit, newx = x2, s = "lambda.min")
summary(lassofit1)

###############################
#     KPI METRICS
#
###############################
error <- y2 - lassofit1
test.label.mean <- mean(y2)
tss <- sum((y2 - test.label.mean)^2)
rss <- sum(error^2)
rsq <- 1 - (rss/tss)
modelkpi[5,5] <- rsq
cal_metrics(5, y2, lassofit1)
