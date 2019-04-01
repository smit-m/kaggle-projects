
library(ggplot2)
library(ggmap)
library(dplyr)
library(splitstackshape)
library(e1071)
library(C50)
library(randomForest)
library(data.table)

setwd("C:/Users/mehta/Documents/GitHub/kaggle-projects/west-nile-virus/data")

#importing data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
spray <- read.csv("spray.csv")
weather <- read.csv("weather.csv")
mapdata <- readRDS("mapdata_copyright_openstreetmap_contributors.rds")


#converting variables to required format
weather$Date <- as.Date(as.character(weather$Date), format = "%Y-%m-%d")
train$Date <- as.Date(as.character(train$Date), format = "%Y-%m-%d")
test$Date <- as.Date(as.character(test$Date), format = "%Y-%m-%d")






weatherdata <- weather %>% group_by(Date) %>% summarise(AvgTemp = mean(Tmax, na.rm = TRUE))



training <- merge(train, weatherdata, by = "Date", all.x = TRUE)
testing <- merge(test, weatherdata, by = "Date", all.x = TRUE)

training$WnvPresent <- as.factor(training$WnvPresent)


t_Species<-c(as.character(training$Species),as.character(testing$Species))
t_Species[t_Species=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
t_Species<-factor(t_Species,levels=unique(t_Species))

## data.table syntax for adding a column; could overwrite the existing column as well
training <- as.data.table(training)[,Species2:=factor(t_Species[1:nrow(training)],levels=unique(t_Species))]
testing <- as.data.table(testing)[,Species2:=factor(t_Species[(nrow(training)+1):length(t_Species)],levels=unique(t_Species))]



#Naive Bayes Classification
nb_model <- naiveBayes(WnvPresent ~ Species + Trap + AvgTemp, data = training)
pred_nb_model <- predict(nb_model, newdata = testing)

rf_model <- randomForest(WnvPresent ~ Species + AvgTemp, data = training, ntree = 200, importance = TRUE)
pred_rf_model <- predict(rf_model, newdata = testing)



# Logistic Regression

#Random Forest

#SMOTE and then regression

#SMOTE and then boosting(try bagging as well)








##############


dist_geo <- function(lat_a, lon_a, lat_b, lon_b) { 
  if(anyNA(c(lat_a, lon_a, lat_b, lon_b))) 
    return(NA) 
  round(distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)/1000,2) 
} 

dgeo$distance_km=mapply(lat_a=dgeo$latitude, lon_a=dgeo$longitude, lat_b=dgeo$latitude_lag, lon_b=dgeo$longitude_lag, FUN = dist_geo)