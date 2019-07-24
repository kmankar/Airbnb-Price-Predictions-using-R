###############################################                                                                       
#   Project: Airbnb Price Prediction
#   Group#: 6
#   Script: Exploratory Data Analysis
#
###############################################

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
#    Package                                                     
#  Installation                                                                      
######################
install.packages("tidyverse")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("choroplethrZip")
install.packages("GGally")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggmap")
install.packages("scales")
install.packages("stringr")
install.packages("zipcode")
install.packages("leaflet")
install.packages("extracat")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("ggthemes")
install.packages("choroplethrZip")
######################                                                                         
#    Libraries                                                     
#                                                                        
######################
library(devtools)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(GGally)
library(ggplot2)
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
library(gtable)
library(ggpubr)
library(corrplot)
library(ggcorrplot)
library(ggthemes)


df <- full.data.viz

str(df)

######################                                                                         
#    Interactive                                                      
#    Visualization 
#   of NYC listings 
######################

leaflet(df) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),popup = paste0("<b> Name: </b>", df$name , "<br/><b> Host Name: </b>", df$host_name, "<br> <b> Price: </b>", df$price, "<br/><b> Room Type: </b>", df$room_type, "<br/><b> Property Type: </b>", df$property_type
  )) %>% 
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")

######################                                                                         
#    Average Location                                                      
#    Score By Area
######################

zipReviews <- df %>% group_by(zipcode = zipcode) %>% summarise(avg_loc_review = mean(review_scores_location, na.rm = TRUE))
colnames(zipReviews) <- c("region","value")
zipReviews$region <- as.character(zipReviews$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
g_locations <- zip_choropleth(zipReviews,
                              county_zoom = nyc_fips,
                              title = "Location Review Scores by Region",
                              legend = "Average Score") + ggtitle("Which area is the best?",
                                                                  subtitle = "Map showing Average Location Score by Area") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+ scale_fill_brewer("Location Review Score",palette=3) + theme_tufte()
g_locations


######################                                                                         
#    Average Price                                                      
#     By Area
######################
zipPrices <- df %>% group_by(zipcode = zipcode) %>% summarise(avg_price = mean(price, na.rm = TRUE))
colnames(zipPrices) <- c("region","value")
zipPrices$region <- as.character(zipPrices$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
g_price_location <- zip_choropleth(zipPrices,
                                   county_zoom = nyc_fips,
                                   title = "Average Price by Region",
                                   legend = "Average Score") + ggtitle("Which area is expensive?",
                                                                       subtitle = "Map showing Average Price by Area") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+ scale_fill_brewer("Average Price",palette=4) + theme_tufte()
g_price_location

######################                                                                         
#    Types of Listings                                                      
#      Percentage
######################

propertydf <-  df %>% group_by(neighbourhood_group_cleansed, property_type) %>% summarize(Freq = n())
propertydf <- propertydf %>% filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))
totalproperty<-  df %>% filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))%>% group_by(neighbourhood_group_cleansed) %>% summarize(sum = n())
propertyratio <- merge(propertydf, totalproperty, by="neighbourhood_group_cleansed")
propertyratio <- propertyratio %>% mutate(ratio = Freq/sum)
ggplot(propertyratio, aes(x=neighbourhood_group_cleansed, y=ratio, fill = property_type)) +
  geom_bar(position = "dodge",stat="identity") + xlab("Borough") + ylab("Count")+
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in NYC?",
          subtitle = "Map showing Count of Listing Type by Borough ") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("Neighborhood") + ylab("Percentage") + theme_tufte()


######################                                                                         
#    Variable Selection                                                      
#          Grid
######################
price_hist <- ggplot(data = df, aes(x = price,fill = neighbourhood_group_cleansed)) + geom_histogram(binwidth = 50) + scale_x_continuous(limits = c(0,3000), breaks = c(0,500,1000,1500,2000,2500,3000)) + scale_fill_discrete(name = "Property Type")+
  ggtitle("Price Distribution Over Neighbourhoods $0 to $3000") + theme(plot.title = element_text(face = "bold")) + scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) + xlab("Price") + ylab("Count") + theme_tufte()

price_hist1 <- ggplot(data = df, aes(x = price, fill = neighbourhood_group_cleansed)) + geom_histogram(binwidth = 50) + scale_x_continuous(limits = c(3001,10000), breaks = c(3001,5000,7000,8000,10000)) + scale_fill_discrete(name = "Property Type")  +
  ggtitle("Price Distribution Over Neighbourhoods $3001 to $10000") + theme(plot.title = element_text(face = "bold")) +  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) + xlab("Price") + ylab("Count") + theme_tufte()

bedroom_hist <- ggplot(data = df, aes(x = bedrooms,fill = neighbourhood_group_cleansed)) + geom_bar() + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + scale_fill_discrete(name = "Property Type")  + scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458"))+
  ggtitle("Bedrooms Distribution Over Neighbourhoods") + theme(plot.title = element_text(face = "bold")) + xlab("Number of Bedrooms") + ylab("Count") + theme_tufte()

correlation <- df %>% select(bedrooms, beds, bathrooms, accommodates, price, security_deposit, cleaning_fee, total_amenities)
res <- cor(correlation, method = "pearson", use = "complete.obs")
correlation_plot <- ggcorrplot(res, method = "circle") + ggtitle("Correlation Matrix") + theme(plot.title = element_text(face = "bold")) + theme_tufte()

g <- rbind(price_hist, price_hist1, bedroom_hist, correlation_plot, size = "first")
grid.arrange(price_hist, price_hist1, bedroom_hist,correlation_plot, nrow = 2) + theme_tufte()

######################                                                                         
#     Reviews 
#   Distribution                                                      
######################

reviews <- df %>% select(number_of_reviews, price)
review_plot <- ggplot(reviews, aes(x = price, y = number_of_reviews) ) + geom_point(color="#e06f69", alpha = 4/10) + scale_x_continuous(limits = c(0,500)) + ggtitle("Review Distribution $0 to $500") +
  theme(plot.title = element_text(face = "bold")) + xlab("Price") + ylab("Number of Reviews") + theme_tufte()
review_plot1 <- ggplot(reviews, aes(x = price, y = number_of_reviews) ) + geom_point(color="#357b8a", alpha = 4/10) + scale_x_continuous(limits = c(501,10000)) + ggtitle("Review Distribution $501 to $10000") +
  theme(plot.title = element_text(face = "bold")) + xlab("Price") + ylab("Number of Reviews") + theme_tufte()

g1 <- rbind(review_plot,review_plot1, size = "first")
grid.arrange(review_plot,review_plot1, nrow = 2) + theme_tufte()