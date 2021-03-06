
library(ggplot2)
library(ggmap)
library(dplyr)
library(e1071)
library(C50)
library(randomForest)
library(data.table)
library(caret)
library(lubridate)
library(fastDummies)
library(DMwR)
library(gridExtra)



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



t_Species<-c(as.character(training$Species),as.character(testing$Species))
t_Species[t_Species=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
t_Species<-factor(t_Species,levels=unique(t_Species))

## data.table syntax for adding a column; could overwrite the existing column as well
training <- as.data.table(training)[,Species2:=factor(t_Species[1:nrow(training)],levels=unique(t_Species))]
testing <- as.data.table(testing)[,Species2:=factor(t_Species[(nrow(training)+1):length(t_Species)],levels=unique(t_Species))]


training$Month <- as.factor(month(training$Date))
testing$Month <- as.factor(month(testing$Date))



t <- training[, c(12:15)]

new_t <- dummy_cols(t)
new_training <- new_t[, c(1, 2, 5:17)]
names(new_training)[3:9] <- c("Cu_PR", "Cu_R", "Cu_P", "Cu_S", "Cu_T", "Cu_Ta", "Cu_E")

new_training$WnvPresent <- as.factor(make.names(new_training$WnvPresent))

te <- testing[, c(2, 12, 13, 14)]
new_te <- dummy_cols(te)
new_testing <- new_te[, c(1, 2, 5:16)]
names(new_testing)[3:9] <- c("Cu_PR", "Cu_R", "Cu_P", "Cu_S", "Cu_T", "Cu_Ta", "Cu_E")
new_testing$Month_5 <- as.integer(0)


#perc.over is used to increase minor samples in multiples of 100s
#perc.under is used to decrease major samples in multiples of 100s
#k is k-nn value


final_training <- SMOTE(WnvPresent ~., new_training, perc.over = 1000, k = 3, perc.under = 200)






lr <- caret::train(final_training[, -"WnvPresent"], final_training$WnvPresent, method = "glmnet",
                    trControl = trainControl(method = 'cv',number = 5, classProbs = T, summaryFunction = twoClassSummary),
                    metric = 'ROC')


pred_lr_model <- predict(lr, new_testing, type = "prob")

final <- data.frame(cbind(new_testing$Id, pred_lr_model[,2]))
colnames(final) <- c("Id","WnvPresent")
write.csv(final, "sub2.csv", row.names=FALSE)



train_2011 <- subset(train, year(train$Date) == 2011)
train_2011_WNVP <- subset(train_2011, WnvPresent == 1)

train_2007 <- subset(train, year(train$Date) == 2007)
train_2007_WNVP <- subset(train_2007, WnvPresent == 1)

train_2009 <- subset(train, year(train$Date) == 2009)
train_2009_WNVP <- subset(train_2009, WnvPresent == 1)

train_2013 <- subset(train, year(train$Date) == 2013)
train_2013_WNVP <- subset(train_2013, WnvPresent == 1)



a <- ggmap(mapdata) + geom_point(aes(Longitude, Latitude), data = train_2007, col = "blue", size = 2) + geom_point(aes(Longitude, Latitude), data = train_2007_WNVP, col = "red", shape = 4, size = 5) + ggtitle("2007")
#a

b <- ggmap(mapdata) + geom_point(aes(Longitude, Latitude), data = train_2009, col = "blue", size = 2) + geom_point(aes(Longitude, Latitude), data = train_2009_WNVP, col = "red", shape = 4, size = 5) + ggtitle("2009")
#b
c <- ggmap(mapdata) + geom_point(aes(Longitude, Latitude), data = train_2011, col = "blue", size = 2) + geom_point(aes(Longitude, Latitude), data = train_2011_WNVP, col = "red", shape = 4, size = 5) + ggtitle("2011")
#c
d <- ggmap(mapdata) + geom_point(aes(Longitude, Latitude), data = train_2013, col = "blue", size = 2) + geom_point(aes(Longitude, Latitude), data = train_2013_WNVP, col = "red", shape = 4, size = 5) + ggtitle("2013")
#d

grid.arrange(a, b, c, d, nrow = 1)



g <- ggplot(train, aes(Species)) + geom_bar(aes(fill = as.factor(WnvPresent)))
g


p <- ggplot(training, aes(Month)) + geom_bar(aes(fill = as.factor(WnvPresent)))
p


q <- ggplot(training, aes(Date, AvgTemp, color = as.factor(WnvPresent), size = 3)) + geom_point()
q
##############

# 
# dist_geo <- function(lat_a, lon_a, lat_b, lon_b) { 
#   if(anyNA(c(lat_a, lon_a, lat_b, lon_b))) 
#     return(NA) 
#   round(distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)/1000,2) 
# } 
# 
# dgeo$distance_km=mapply(lat_a=dgeo$latitude, lon_a=dgeo$longitude, lat_b=dgeo$latitude_lag, lon_b=dgeo$longitude_lag, FUN = dist_geo)