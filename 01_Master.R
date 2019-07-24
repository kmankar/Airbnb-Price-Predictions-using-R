########################################                                                                         
#   Project: Airbnb Price Prediction
#   Group#: 6
#   Script: NYCAirbnb.R
#   Description: Ingest,Clean,Preprocess
#                and Joins datasets
########################################

######################                                                                         
#   Package                                                     
#   Installation                                                                      
######################
install.packages("car")
install.packages("tidyverse")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("choroplethrZip")
install.packages("GGally")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggmap")
install.packages("stringr")
install.packages("zipcode")
install.packages("leaflet")
install.packages("extracat")
install.packages("gridExtra")
install.packages("data.table")
install.packages("sentimentr")
install.packages("Metrics")
install.packages("xgboost")
install.packages("mlr")
install.packages("dummies")
install.packages("sparklyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("leaps")
install.packages("ISLR")
install.packages("glmnet")
install.packages("scales")
install.packages("keras")
install.packages("tensorflow")
install.packages("neuralnet")

######################                                                                         
#   Libraries                                                     
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

##############################
# Creating KPI DF
#
##############################
modelkpi <- data.frame("MODEL" = c("Linear (Base)","Linear Logged", "XG-Boost", "XG-Boost Logged", "Lasso Linear", "Neural Net"), "RMSE" = c(0,0,0,0,0,0), "MAE" = c(0,0,0,0,0,0), "MAPE" = c(0,0,0,0,0,0), "Adj RSquared"=c(0,0,0,0,0,0), "Avg Error"=c(0,0,0,0,0,0))
modelkpi$MODEL <- as.character(modelkpi$MODEL)
modelkpi
######################                                                                         
#  Clean function for                                                     
#  Visualizations                                                                      
######################

viz.clean <- function(x)
{
  df <- x
  df$amenities <- as.character(df$amenities)
  df <- df %>% mutate(total_amenities= ifelse(nchar(amenities)>2, str_count(amenities, ',')+1, 0))
  df <- df %>% select("id" ,"name","host_name" ,"host_response_time" ,"host_response_rate" ,"host_is_superhost" ,"host_neighbourhood" ,"host_listings_count" ,"host_total_listings_count" ,"host_identity_verified" ,
                      "neighbourhood" ,"neighbourhood_cleansed","neighbourhood_group_cleansed" ,"city" ,"zipcode" ,"latitude" ,"longitude" ,"is_location_exact" ,"property_type" ,"room_type" ,"accommodates" ,"bathrooms" ,"bedrooms" ,
                      "beds" ,"bed_type" ,"price" ,"security_deposit" ,"cleaning_fee" ,"guests_included" ,"extra_people" ,"minimum_nights" ,"maximum_nights" ,"availability_30" ,"availability_60" ,"availability_90","availability_365" ,
                      "number_of_reviews" ,"review_scores_rating" ,"review_scores_accuracy" ,"review_scores_cleanliness" ,"review_scores_checkin" ,"review_scores_communication" ,"review_scores_location" ,"review_scores_value" ,
                      "instant_bookable" ,"is_business_travel_ready" ,"cancellation_policy" ,"require_guest_phone_verification" ,"calculated_host_listings_count" ,"reviews_per_month", "total_amenities")
  df$host_name <- as.character(df$host_name)
  df$extra_people <- as.numeric(gsub("\\$", "", df$extra_people))
  df$price <- gsub("\\$", "", df$price)
  df$price <- as.numeric(gsub(",","",df$price))
  df$security_deposit <- gsub("\\$", "", df$security_deposit)
  df$security_deposit <- as.numeric(gsub(",","",df$security_deposit))
  df$security_deposit <- ifelse(is.na(df$security_deposit), 0, df$security_deposit)
  df$cleaning_fee <- gsub("\\$", "", df$cleaning_fee)
  df$cleaning_fee <- as.numeric(gsub(",","",df$cleaning_fee))
  df$cleaning_fee <- ifelse(is.na(df$cleaning_fee), 0, df$cleaning_fee)
  df$host_response_rate <- gsub("\\%", "", df$host_response_rate)
  df$host_response_rate <- as.numeric(df$host_response_rate)
  df$host_is_superhost <- as.numeric(factor(df$host_is_superhost, levels = c("f","t"))) - 1
  df$host_is_superhost <- ifelse(is.na(df$host_is_superhost), 0, df$host_is_superhost)
  df$host_identity_verified <- as.numeric(factor(df$host_identity_verified, levels = c("f","t"))) - 1
  df$neighbourhood_group <- factor(df$neighbourhood_group)
  df$neighbourhood_group_cleansed <- factor(df$neighbourhood_group_cleansed)
  df$room_type <- factor(df$room_type)
  df$host_response_time <- factor(df$host_response_time, levels = c("N/A","within an hour", "within a few hours", "within a day","a few days or more"))
  df$property_type <- factor(df$property_type)
  df$bed_type <- factor(df$bed_type)
  df$minimum_nights <- as.numeric(df$minimum_nights)
  df$maximum_nights <- as.numeric(df$maximum_nights)
  df$neighbourhood <- factor(df$neighbourhood)
  df$neighbourhood_cleansed <- factor(df$neighbourhood_cleansed)
  df$zipcode <- factor(df$zipcode)
  df$host_neighbourhood <- factor(df$host_neighbourhood)
  df$host_listings_count <- as.numeric(df$host_listings_count)
  df$host_total_listings_count <- as.numeric(df$host_total_listings_count)
  df$accommodates <- as.numeric(df$accommodates)
  df$bathrooms <- as.numeric(df$bathrooms)
  df$bedrooms <- as.numeric(df$bedrooms)
  df$beds <- as.numeric(df$beds)
  df$guests_included <- as.numeric(df$guests_included)
  df$calculated_host_listings_count <- as.numeric(df$calculated_host_listings_count)
  df$is_location_exact <- as.numeric(factor(df$is_location_exact, levels = c("f","t"))) - 1
  df$is_location_exact <- as.factor(df$is_location_exact)
  df$cancellation_policy <- as.factor(df$cancellation_policy)
  df$require_guest_phone_verification <- as.numeric(factor(df$require_guest_phone_verification, levels = c("f","t"))) - 1
  df$require_guest_phone_verification <- as.factor(df$require_guest_phone_verification)
  df$is_business_travel_ready <- as.factor(df$is_business_travel_ready)
  df$is_business_travel_ready <- as.numeric(factor(df$is_business_travel_ready, levels = c("f","t"))) - 1
  df$instant_bookable <- as.numeric(factor(df$instant_bookable, levels = c("f","t"))) - 1
  df$instant_bookable <- as.factor(df$instant_bookable)
  df$cancellation_policy <- factor(df$cancellation_policy)
  x <- df
}

########################                                                                         
#    Clean function for                                                     
#    Predictive Models                                                                      
########################

pred.clean <- function(x){
  df <- x
  df$amenities <- as.character(df$amenities)
  df <- df %>% mutate(total_amenities = ifelse(nchar(amenities)>2, str_count(amenities, ',')+1, 0))
  df <- df %>% select("id","host_is_superhost" ,"host_response_time",
                      "neighbourhood_group_cleansed","is_location_exact" ,"property_type" ,"room_type" ,"accommodates" ,"bathrooms" ,"bedrooms" ,
                      "security_deposit" ,"cleaning_fee" ,"guests_included" ,"extra_people"  ,"availability_30" ,
                      "number_of_reviews" ,"review_scores_rating" ,"review_scores_accuracy" ,"review_scores_cleanliness" ,"review_scores_checkin" ,"review_scores_communication" ,"review_scores_location" ,"review_scores_value" ,
                      "cancellation_policy" , "total_amenities","price")
  names(df)[1]<-"listing_id"
  df$price <- gsub("\\$", "", df$price)
  df$price <- as.numeric(gsub(",","",df$price))
  df$security_deposit <- gsub("\\$", "", df$security_deposit)
  df$security_deposit <- as.numeric(gsub(",","",df$security_deposit))
  df$security_deposit <- ifelse(is.na(df$security_deposit), 0, df$security_deposit)
  df$cleaning_fee <- gsub("\\$", "", df$cleaning_fee)
  df$cleaning_fee <- as.numeric(gsub(",","",df$cleaning_fee))
  df$cleaning_fee <- ifelse(is.na(df$cleaning_fee), 0, df$cleaning_fee)
  df$bedrooms <- ifelse(is.na(df$bedrooms), mean(df$bedrooms,na.rm=TRUE), df$bedrooms)
  df$bathrooms <- ifelse(is.na(df$bathrooms), mean(df$bathrooms,na.rm=TRUE), df$bathrooms)
  df$review_scores_rating <- ifelse(is.na(df$review_scores_rating), mean(df$review_scores_rating,na.rm=TRUE), df$review_scores_rating)
  df$review_scores_accuracy <- ifelse(is.na(df$review_scores_accuracy), mean(df$review_scores_accuracy,na.rm=TRUE), df$review_scores_accuracy)
  df$review_scores_cleanliness <- ifelse(is.na(df$review_scores_cleanliness), mean(df$review_scores_cleanliness,na.rm=TRUE), df$review_scores_cleanliness)
  df$review_scores_checkin <- ifelse(is.na(df$review_scores_checkin), mean(df$review_scores_checkin,na.rm=TRUE), df$review_scores_checkin)
  df$review_scores_communication <- ifelse(is.na(df$review_scores_communication), mean(df$review_scores_communication,na.rm=TRUE), df$review_scores_communication)
  df$review_scores_location <- ifelse(is.na(df$review_scores_location), mean(df$review_scores_location,na.rm=TRUE), df$review_scores_location)
  df$review_scores_value <- ifelse(is.na(df$review_scores_value), mean(df$review_scores_value,na.rm=TRUE), df$review_scores_value)
  df$host_is_superhost <- as.numeric(factor(df$host_is_superhost, levels = c("f","t"))) - 1
  df$host_is_superhost <- ifelse(is.na(df$host_is_superhost), 0, df$host_is_superhost)
  df$neighbourhood_group_cleansed <- factor(df$neighbourhood_group_cleansed)
  df$room_type <- factor(df$room_type)
  df$host_response_time <- factor(df$host_response_time, levels = c("N/A","within an hour", "within a few hours", "within a day","a few days or more"))
  df$property_type <- factor(df$property_type)
  df$accommodates <- as.numeric(df$accommodates)
  df$bathrooms <- as.numeric(df$bathrooms)
  df$bedrooms <- as.numeric(df$bedrooms)
  df$guests_included <- as.numeric(df$guests_included)
  df$is_location_exact <- as.numeric(factor(df$is_location_exact, levels = c("f","t"))) - 1
  df$is_location_exact <- as.factor(df$is_location_exact)
  df$cancellation_policy <- as.factor(df$cancellation_policy)
  df$cancellation_policy <- factor(df$cancellation_policy)
  df$property_type <- as.factor(df$property_type)
  x <- df
}

#########################                                                                         
#    Sentiment Analysis                                                     
#    on Reviews                                                                      
#########################

sentimentanalysis <- function(x)
{
  x$comments <- as.character(x$comments)
  sentiment = sentiment_by(x$comments)
  sentiment$listing_id = x$listing_id
  gradedsentiment <- sentiment %>%
    group_by(listing_id = listing_id) %>%
    summarise(avg_sent = mean(ave_sentiment, na.rm = T))
  return(gradedsentiment)
}

###############################                                                                        
#   Calculate All Metrics
#   
###############################

cal_metrics <- function(id,act,pred)
{
  error <- pred-act
  modelkpi[id,2] <- sqrt(mean(error^2, na.rm = T))
  modelkpi[id,3] <- mae(act,pred)
  modelkpi[id,4] <- mean(abs(error/act) * 100)
  modelkpi[id,6] <- mean(error)
  assign("modelkpi",modelkpi,envir=.GlobalEnv)
  return(modelkpi)
}

######################                            
#    Ingesting Dataset                                                    
#                                                                         
######################     
mar19         <- read.csv("listings.csv", stringsAsFactors = F)
mar19.reviews <- read.csv("reviews.csv",  stringsAsFactors = F)

######################                                                                         
#    Cleaning Data                                                    
#                                                                         
######################     
pred.mar19          <- pred.clean(mar19)
viz.mar19           <- viz.clean(mar19)
senti.mar19.reviews <- sentimentanalysis(mar19.reviews)

###########################                                                                         
# Joining Sentiment Scores                                            
# to Main Data                                                              
###########################     

full.data.model <- full_join(pred.mar19, senti.mar19.reviews, by = c("listing_id"))
full.data.model$avg_sent <- ifelse(is.na(full.data.model$avg_sent), median(full.data.model$avg_sent, na.rm = T), full.data.model$avg_sent)

##########################                                                                         
# Preprocessing Full Data                                                    
#                                                                         
##########################     

full.data.model <- full.data.model %>%
  filter(!(price == 0))
full.data.model <- subset(full.data.model, price <= 3000)
full.data.model[!complete.cases(full.data.model),3] <- "N/A"

################################
#   Writing Cleaned CSVs
#
################################
write.csv(full.data.model, "Model_Listings_Cleaned")
write.csv(full.data.viz,  "Visual_Listings_Cleaned")
