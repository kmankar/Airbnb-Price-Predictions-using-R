#############################################                                                                       
#   Project: Airbnb Price Prediction
#   Group#: 6
#   Script: Neural Network
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
#   Neural Network
#
# ######################
df <- dummy.data.frame(df, c("host_response_time", "neighbourhood_group_cleansed", "is_location_exact", "property_type", "room_type", "cancellation_policy"))

df <- as.matrix(df)
dimnames(df) <- NULL

set.seed(71923)
Train <- sample(nrow(df), 0.7*nrow(df))
train.data <- df[Train,]
test.data <- df[-Train,]

train.label <- train.data[,75]
test.label <- test.data[,75]


train.data <- train.data[,-75]
test.data <- test.data[,-75]


m <- colMeans(train.data)
s <- apply(train.data, 2, sd)
nn.train <- scale(train.data, center = m, scale = s)
nn.test <- scale(test.data, center = m, scale = s)

######################                                                                         
#    Neural Net
#      Model                   
######################     
build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 30, activation = "relu",input_shape = dim(nn.train)[2]) %>%
    layer_dense(units = 15, activation = "relu") %>%
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  model
}

model <- build_model()
model %>% summary()

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

epo <- 200
history <- model %>% fit(
  train.data,
  train.label,
  epochs = epo,
  validation_split = 0.2,
  callbacks = list(print_dot_callback)
)



model %>% evaluate(test.data, test.label)
pred <- model %>% predict(test.data)

###############################
#     PLOTS
#
###############################
plot(test.label, pred)
plot(history)
###############################
#     KPI METRICS
#
###############################
error <- pred - test.label
test.label.mean <- mean(test.label)
tss <- sum((test.label - test.label.mean)^2 )
rss <- sum(error^2)
rsq <- 1 - (rss/tss)
modelkpi[6,4] <- rsq
cal_metrics(6, test.label, pred)
