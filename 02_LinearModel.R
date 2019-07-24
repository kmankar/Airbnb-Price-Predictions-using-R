#############################################                                                                       
#   Project: Airbnb Price Prediction
#   Group#: 6
#   Script: Linear Regression
#
#############################################


######################                                                                         
#   Libraries (REDUNDANT LOAD)                                                  
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
full.data.pruned <- full.data.model


######################                                                                         
# Column Selection                                                    
#                                                                         
######################     

lm.data <-  full.data.pruned %>% select("host_is_superhost" ,"host_response_time",
                                        "neighbourhood_group_cleansed","cancellation_policy","is_location_exact",
                                        "property_type" ,"room_type" ,"accommodates" ,"bathrooms" ,"bedrooms" ,
                                        "price" ,"security_deposit" ,"cleaning_fee" ,"guests_included" ,"extra_people"  ,"availability_30" ,
                                        "number_of_reviews" ,"review_scores_rating" ,"review_scores_accuracy" ,"review_scores_cleanliness" ,"review_scores_checkin" ,"review_scores_communication" ,"review_scores_location" ,"review_scores_value" ,
                                        "total_amenities","avg_sent")

######################                                                                         
#    Linear Model                                                    
#                                                                         
######################     

set.seed(71943)
splitter <- sample(nrow(lm.data),0.3*nrow(lm.data))
lm.data.train <- lm.data[-splitter,]
lm.data.test <- lm.data[splitter,]

linearmodel <- lm(price~., data = lm.data.train)
summary(linearmodel)

linearmodel.pred <- predict(linearmodel, newdata = lm.data.test, type = "response")

###############################
#     PLOTS
#
###############################
plotdata <- data.frame(lm.data.test$price,linearmodel.pred)
ggplot(data = plotdata, aes(x = lm.data.test.price, y = linearmodel.pred)) + geom_point() + theme_tufte() + geom_abline()

residualPlots(linearmodel, terms = ~1)

###############################
#     KPI METRICS
#
###############################
modelkpi[1,5] <- summary(linearmodel)$adj.r.squared
cal_metrics(1,lm.data.test$price, linearmodel.pred)
