#############################################                                                                       
#   Project: Airbnb Price Prediction
#   Group#: 6
#   Script: Extreme Gradient Boosting
#
#############################################


######################                                                                         
#   Libraries            (REDUNDANT LOAD)                                         
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
df <- dummy.data.frame(df, c("host_response_time", "neighbourhood_group_cleansed", "is_location_exact", "property_type", "room_type", "cancellation_policy"))
set.seed(71943)
Train <- sample(nrow(df), 0.3*nrow(df))
train.data <- df[-Train,]
test.data <- df[Train,]
train.label <- train.data[,75]
test.label <- test.data[,75]

train.data <- train.data[,-c(1,75)]
test.data <- test.data[,-c(1,75)]

train.data <- as.matrix(train.data)
test.data <- as.matrix(test.data)

dtrain <- xgb.DMatrix(data = train.data, label = train.label)
dtest <- xgb.DMatrix(data = test.data, label = test.label)

######################                                                                         
#   XGBoost                                                      
#   Model
######################     


params <- list(booster = "gblinear",objective = "reg:linear", eta=0.3, gamma=20, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1,alpha = 1)

#check for optimal nrounds
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 10000, nfold = 5, showsd = T, stratified = T, print.every.n = 25, early.stop.round = 20, maximize = F)

xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 2000, watchlist = list(val=dtest,train=dtrain), print_every_n = 25, early_stop_round = 1000, maximize = F , eval_metric = "rmse")
xgbpred <- predict (xgb1,dtest)

###############################
#     PLOTS
#
###############################
plotdata <- data.frame(test.label,xgbpred)
ggplot(data = plotdata, aes(x = test.label, y = xgbpred)) + geom_point() + theme_tufte() + geom_abline()

###############################
#     KPI METRICS
#
###############################
test.label.mean <- mean(test.label)
tss <- sum((test.label - test.label.mean)^2 )
rss <- sum(error^2)
rsq <- 1 - (rss/tss)
modelkpi[3,5] <- rsq
cal_metrics(3, test.label, xgbpred)
