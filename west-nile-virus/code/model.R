
library(ggplot2)
library(ggmap)
library(dplyr)
library(splitstackshape)

setwd("C:/Users/mehta/Documents/GitHub/kaggle-projects/west-nile-virus/data")

#importing data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
spray <- read.csv("spray.csv")
weather <- read.csv("weather.csv")
mapdata <- readRDS("mapdata_copyright_openstreetmap_contributors.rds")


#weather <- cSplit(weather, "CodeSum", sep = " ", type.convert = F)



a <- train %>% group_by(Block,Trap, Latitude, Longitude, Date, Species) %>% summarise(NumMosquitos = sum(NumMosquitos), WNV = sum(WnvPresent))





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