# Clearing the environment
rm(list=ls(all=T))
# Setting working directory
setwd("G:/Data Analytics/Cab-Fare-Project")
getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
install.packages("xlsx")
library(xlsx)
library(ggplot2)

## Reading the data
df_cab_train= read.csv('train_cab.csv')
df_cab_test=read.csv('test.csv')

#----------------------------------------------------Exploratory Data Analysis------------------------------------------------------
# Shape of the data
dim(df_cab_train)
# Viewing data
View(df_cab_train)
# Structure of the data
str(df_cab_train)
# Variable namesof the data
colnames(df_cab_train)
#summary of the data set
summary(df_cab_train)
#
class(df_cab_train)

# Shape of the data
dim(df_cab_test)
# Viewing data
View(df_cab_test)
# Structure of the data
str(df_cab_test)
# Variable namesof the data
colnames(df_cab_test)
#summary of the data set
summary(df_cab_test)
#
class(df_cab_test)


#---------------------------------Analysis----------------------
#During the loading of the data it was found that fare amount was considered as a favtor value.
#Comparing the same inital load with python as the corresponding values, it was found that the Null values in R were replaced as 1 in the fare_amount section
#Hence that is incorrect data being populated and we have to drop those values

#since fare_amount was of type factor, converting it to numeric
df_cab_train$fare_amount=as.numeric(df_cab_train$fare_amount)
df_cab_train= subset(df_cab_train, fare_amount != 1)

train_col= colnames(df_cab_train)
#------------------------------------Missing Values Analysis---------------------------------------------------#


#converting factor type pickup time to a character first
df_cab_test$pickup_datetime=as.character(df_cab_test$pickup_datetime)

#Creating dataframe with missing values present in each variable
missing_val = data.frame(apply(df_cab_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

#Calculating percentage missing value
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df_cab_train)) * 100
missing_val
# Sorting missing_val in Descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL

# Reordering columns
missing_val = missing_val[,c(2,1)]

# Saving output result into csv file
write.csv(missing_val, "Missing_perc_R.csv", row.names = F)

#Since missing values are present only in passenger count with a 0.34%, hence removing those values from our data set
df_cab_train=na.omit(df_cab_train)
df_cab_test=na.omit(df_cab_test)

ggplot(data = missing_val[1:18,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "orange")+xlab("Variables")+
  ggtitle("Variables Vs Missing Percent Plot") + theme_bw()

#-------------------------------Outliers and Plots--------------------------

# Boxplot for continuous variables

boxplot(df_cab_train$fare_amount, xlab="Fare",ylab="count")
boxplot(df_cab_train$pickup_longitude, xlab="Pickup_Longitude",ylab="count")
boxplot(df_cab_train$pickup_latitude, xlab="Pickup_Latitude",ylab="count")
boxplot(df_cab_train$dropoff_latitude, xlab="Dropoff_Latitude",ylab="count")
boxplot(df_cab_train$dropoff_longitude, xlab="Dropoff_Longitude",ylab="count")
boxplot(df_cab_train$passenger_count, xlab="Passenger_Count",ylab="count")

#-------------------------------Refining the data----------------------------

#converting factor type pickup time to a character first
df_cab_train$pickup_datetime=as.character(df_cab_train$pickup_datetime)
#converting inot datetime
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
df_cab_train$pickup_datetime<- as.POSIXct(df_cab_train$pickup_datetime,format = "%Y-%m-%d %H:%M:%S",tz="")

#Extracting date and time values
df_cab_train$Hours <- as.numeric(format(as.POSIXct(strptime(df_cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H"))
df_cab_train$Minutes <- as.numeric(format(as.POSIXct(strptime(df_cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%M"))
df_cab_train$Date <- as.numeric(format(as.POSIXct(strptime(df_cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d"))
df_cab_train$Month <- as.numeric(format(as.POSIXct(strptime(df_cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m"))
df_cab_train$Year <- as.numeric(format(as.POSIXct(strptime(df_cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y"))

df_cab_train=na.omit(df_cab_train)


#converting into datetime
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
df_cab_test$pickup_datetime<- as.POSIXct(df_cab_test$pickup_datetime,format = "%Y-%m-%d %H:%M:%S",tz="")

#Extracting date and time values
df_cab_test$Hours <- as.numeric(format(as.POSIXct(strptime(df_cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H"))
df_cab_test$Minutes <- as.numeric(format(as.POSIXct(strptime(df_cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%M"))
df_cab_test$Date <- as.numeric(format(as.POSIXct(strptime(df_cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d"))
df_cab_test$Month <- as.numeric(format(as.POSIXct(strptime(df_cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m"))
df_cab_test$Year <- as.numeric(format(as.POSIXct(strptime(df_cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y"))

df_cab_test=na.omit(df_cab_test)

#--------------------------------------------------------------------------

#Fare amount should be a positive value, passenger to be less than 7

df_cab_train= subset(df_cab_train, passenger_count <7 & passenger_count >0)
df_cab_train= subset(df_cab_train, passenger_count!=0.12)

df_cab_test=subset(df_cab_test, passenger_count < 7 & passenger_count > 0)

df_cab_train= subset(df_cab_train, fare_amount >0)

install.packages('geosphere')
library(geosphere)

df_cab_train= subset(df_cab_train, (pickup_longitude <180 & pickup_longitude > -180))
df_cab_train= subset(df_cab_train, (pickup_latitude<90 & pickup_latitude > -90))
df_cab_train= subset(df_cab_train, (dropoff_latitude<90 & dropoff_latitude > -90))
df_cab_train= subset(df_cab_train, (dropoff_longitude <180 & dropoff_longitude > -180))

df_cab_test= subset(df_cab_test, (pickup_longitude <180 & pickup_longitude > -180))
df_cab_test= subset(df_cab_test, (pickup_latitude<90 & pickup_latitude > -90))
df_cab_test= subset(df_cab_test, (dropoff_latitude<90 & dropoff_latitude > -90))
df_cab_test= subset(df_cab_test, (dropoff_longitude <180 & dropoff_longitude > -180))


#Using Haversine Function of Geosphere package
#distHaversine(c(-73.84431,40.72132), c(-73.84161,40.71228),  r=6378137)
df_cab_train$Distance_travelled <-distHaversine(cbind(df_cab_train$pickup_longitude, df_cab_train$pickup_latitude),cbind(df_cab_train$dropoff_longitude,df_cab_train$dropoff_latitude))

df_cab_test$Distance_travelled <-distHaversine(cbind(df_cab_test$pickup_longitude, df_cab_test$pickup_latitude),cbind(df_cab_test$dropoff_longitude,df_cab_test$dropoff_latitude))


#Converting in KM
df_cab_train$Distance_travelled<-(df_cab_train$Distance_travelled)/1000

df_cab_test$Distance_travelled<-(df_cab_test$Distance_travelled)/1000
#Analysis over distance
#BoxPlot
boxplot(df_cab_train$Distance_travelled)

summary(df_cab_train$Distance_travelled)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000    1.216    2.128   15.082    3.856 8677.252 

df_cab_train[order(-df_cab_train$Distance_travelled),]


#After googling we got to know that coordinate (0.00000,0.00000) lies in the middle of the ocean.
#Hence removing these values

df_cab_train= subset(df_cab_train, (pickup_longitude != 0.00000 & pickup_longitude != 0.00000))
df_cab_train= subset(df_cab_train, (dropoff_longitude != 0.00000 & dropoff_longitude != 0.00000))

df_cab_test= subset(df_cab_test, (pickup_longitude != 0.00000 | pickup_longitude != 0.00000))
df_cab_test= subset(df_cab_test, (dropoff_longitude != 0.00000 | dropoff_longitude != 0.00000))


#BoxPlot
boxplot(df_cab_train$Distance_travelled)
df_cab_train[order(df_cab_train$Distance_travelled),]
#After again plotting the values of boxplot, and ordering the data set in reverse order on basis of distance


df_cab_train= subset(df_cab_train, Distance_travelled < 4452067)

#considering a business case that a passenger can travel at max 50 km by cab
df_cab_train=subset(df_cab_train,Distance_travelled < 51)
df_cab_train
#----------------------------Visualizations------------------------------------
df_columns=colnames(df_cab_train)

df_columns=c('fare_amount','pickup_longitude','pickup_latitude','dropoff_longitude','dropoff_latitude',
             'passenger_count','Hours','Minutes','Date','Month','Year','Distance_travelled')


for (i in df_columns){
  hist(df_cab_train[,i], col="yellow", prob=TRUE, main="Histogram",xlab=i, ylab="Count")
  lines(density(df_cab_train[,i]), lwd=2)
}

#BoxPlot
boxplot(df_cab_test$Distance_travelled)

df_cab_test[order(-df_cab_test$Distance_travelled),]

#-------------------------Normalising data--------------------------

for(i in df_columns)
{
  print(i)
  df_cab_train[,i] = (df_cab_train[,i] - min(df_cab_train[,i]))/(max(df_cab_train[,i])-min(df_cab_train[,i]))
}

#Taking some business scenarios to draw some inference

h <- hist(df_cab_train$passenger_count)

#Maximum number of passengers were single

plot(df_cab_train$fare_amount, df_cab_train$Distance_travelled, main = "DistanceVsFare",
     xlab = "Fare", ylab = "Distance",
     pch = 19, frame = FALSE)
#------------------------------------------Model Development--------------------------------------------#

#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = sample(1:nrow(df_cab_train), 0.8 * nrow(df_cab_train))
train = df_cab_train[ train.index,]
test  = df_cab_train[-train.index,]

train =train[,-2]
test=test[,-2]

##Decision tree for classification
#Develop Model on training data

library(rpart)
df_dtree=rpart(fare_amount~., data = train, method = "anova")

dev.new()
plot(df_dtree)
text(df_dtree)
summary(df_dtree)
printcp(df_dtree)


#df_cab_train[,1]

#write rules into disk
write(capture.output(summary(df_dtree)), "Rules.txt")

#Lets predict for test data
pred_test_DT = predict(df_dtree,test[,-1])

#Lets predict for train data
pred_train_DT = predict(df_dtree,train[,-1])


install.packages("caret")
library(caret)

# For training data 
print(postResample(pred = pred_train_DT, obs = train[,1]))
#     RMSE  Rsquared       MAE 
#0.2868771 0.3944942 0.2146257

# For testing data 
print(postResample(pred = pred_test_DT, obs = test[,1]))
#     RMSE  Rsquared       MAE 
#0.2908909 0.3784100  0.2172989 
#-----------------Linear Regression-----------------------

#check multicollearity
install.packages("usdm")
library(usdm)
vif(df_cab_train[,-1:-2])

vifcor(df_cab_train[,-1:-2], th = 0.9)

#run regression model
LR_model = lm( fare_amount ~., data = train)

#Summary of the model
summary(LR_model)


#Lets predict for test data
pred_test_LR = predict(LR_model,test[,-1])

#Lets predict for train data
pred_train_LR = predict(LR_model,train[,-1])

#Predict
predictions_LR = predict(LR_model, test[,2:12])

# For training data 
print(postResample(pred = pred_train_LR, obs = train[,1]))
#         RMSE  Rsquared       MAE 
#0.35559168     0.06968557 0.31445119  

# For testing data 
print(postResample(pred = pred_test_LR, obs = test[,1]))
#     RMSE  Rsquared       MAE 
#0.35704205 0.06346288 0.31768677   


#------------------RANDOM FOREST------------------

install.packages("randomForest")
library(randomForest)
#RUN RANDOM FOREST
RF_model = randomForest(fare_amount ~., train, importance = TRUE, ntree = 300)

#Summary of the model
summary(RF_model)


#Lets predict for test data
pred_test_RF = predict(RF_model,test[,-1])

#Lets predict for train data
pred_train_RF = predict(RF_model,train[,-1])

#Predict
predictions_RF = predict(RF_model, test[,2:12])

# For training data 
print(postResample(pred = pred_train_RF, obs = train[,1]))
#     RMSE  Rsquared       MAE 
#0.12397933 0.92167556  0.08846157  

# For testing data 
print(postResample(pred = pred_test_RF, obs = test[,1]))
#     RMSE  Rsquared       MAE 
#0.2720930 0.4578688 0.1965762 


#----------------------------XGBOOST----------------------

install.packages('devtools')
install.packages('xgboost')
install.packages('gbm')

#https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/

library(xgboost)
library(readr)
library(stringr)
library(gbm)

fit_XGB = gbm(fare_amount~., data = train, n.trees = 500, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, train, n.trees = 500)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB,test, n.trees = 500)

# For training data 
print(postResample(pred = pred_XGB_train, obs = train$fare_amount))
#RMSE       Rsquared         MAE
#0.2658812  0.4811690     0.1949224 

# For testing data 
print(postResample(pred = pred_XGB_test, obs = test$fare_amount))
#RMSE       Rsquared         MAE
#0.2754497 0.4425815      0.2013333 



#---------------------------------------Predicting using XGBoost--------------

#Lets predict for test data, adding new column for Predicted fare
df_cab_test$Predicted_Fare = predict(fit_XGB,df_cab_test, n.trees = 500)
df_cab_test
