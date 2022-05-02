getwd()
setwd("C:/Users/lloyd/Desktop/simplilearn/pg_program_ds&business_analytics/course/courses/Data Science with R/Kaggle Competetion")
#Load File
data = read.csv("Walmart.csv")
head(data)
#shape of the data
dim(data)
#structure of data
str(data)
#1. EDA Analysis
#data statistics
summary(data)
#Storewise Sales Anlysis
library(dplyr)
store_sales = data.frame(summarise(group_by(data, Store), sum(Weekly_Sales)))
head(store_sales)
barplot(store_sales$sum.Weekly_Sales., xlab="Stores", ylab="Weekly_sum",ylim=c(0, 3e8), main='Weekly Sales per Store')
#Avarage Holiday and Non holiday wise sales analysis
hol_sales = data.frame(summarise(group_by(data, Holiday_Flag), mean(Weekly_Sales)))
pie(hol_sales$mean.Weekly_Sales., labels = paste0(hol_sales$Holiday_Flag," : ",as.character(hol_sales$mean.Weekly_Sales.)), col = c('blue', 'orange'), radius = 2)
#Monthly Sales Analysis
library(lubridate)
data$Date=as.Date(data$Date, format="%d-%m-%Y")
data$month = month(data$Date)
data$year = year(data$Date)
data$month_year = as.Date(paste0('01-',as.character(data$month), "-", as.character(data$year)), format="%d-%m-%Y")
monthly_sales = data.frame(summarise(group_by(data, month_year), sum(Weekly_Sales)))
plot(monthly_sales$month_year, monthly_sales$sum.Weekly_Sales., type='l', main='Monthly Sales')
#Anual Sales Analysis
yearly_sales = data.frame(summarise(group_by(data, year), sum(Weekly_Sales)))
plot(yearly_sales$year, yearly_sales$sum.Weekly_Sales., type='l', main='Monthly Sales')
#Analysing relationship between variables 
install.packages('GGally')
library(GGally)
data$month_year=NULL
data$days = day(data$Date)
print_if_interactive(ggpairs(data[,c(1,3,4,5,6,7,8,9,10,11)]))
#using Correlation matrix
corrplot::corrplot(cor(data[,c(1,3,4,5,6,7,8,9,10,11)]), method = 'square')
#All features are weekly correlated with Weekly_Sales. So I don't choose linear regression
#Since featueres are not normally distributed. I use box plot to check the outliers in the data
library(tidyr)
data[,c(1,4,5,6,7,8,9,10,11)] %>% gather() %>%
  ggplot(aes(value))+facet_wrap(~key, scales ='free')+geom_boxplot()
#2. Data Preparation
#Handling Outliers
columns = c('Temperature', 'Unemployment')
Q3 = quantile(data$Temperature, 0.75)
Q1 = quantile(data$Temperature, 0.25)
IQR = Q3-Q1
upper_limit = Q3 + (IQR * 1.5)
lower_limit = Q1 - (IQR * 1.5)
data=transform(data, Temperature_1=ifelse(Temperature> upper_limit, upper_limit, ifelse(Temperature<lower_limit, lower_limit, Temperature)))
head(data)
Q3 = quantile(data$Unemployment, 0.75)
Q1 = quantile(data$Unemployment, 0.25)
IQR = Q3-Q1
upper_limit = Q3 + (IQR * 1.5)
lower_limit = Q1 - (IQR * 1.5)
data=transform(data, Unemployment_1=ifelse(Unemployment> upper_limit, upper_limit, ifelse(Unemployment<lower_limit, lower_limit, Unemployment)))
head(data)
data[,c('Temperature_1', 'Unemployment_1')] %>% gather() %>%
  ggplot(aes(value))+facet_wrap(~key, scales ='free')+geom_boxplot()
data$Temperature=data$Temperature_1
data$Unemployment=data$Unemployment_1
data$Temperature_1=NULL
data$Unemployment_1=NULL
#checking missing values\
length(data[is.na(data)])
#Feature Scaling
install.packages("ade4")
library(ade4)
data$Store = as.factor(data$Store)
data_dummy = acm.disjonctif(data['Store'])
data['Store']=NULL
data=cbind(data, data_dummy)
data$Date=NULL
columns = c('Temperature', 'Fuel_Price', 'CPI', 'Unemployment', 'days', 'month', 'year')
for (col in columns){
  Features[, col]=scale(Features[, col], center = TRUE, scale = TRUE)
}
View(data)
#Splitting into training and testing set
set.seed(123)
index = sample(1:nrow(data), 0.8*nrow(data))
train_data = data[index, ]
test_data = data[-index, ]
#Selecting important features
model=lm(Weekly_Sales~.,train_data)
summary(model)
#Train the Model
columns = colnames(train_data)
remove=c('Fuel_Price', 'Store.15', 'Store.29', 'Store.33', 'Store.38', 'Store.42', 'Store.45')
columns = columns[! columns %in% remove]
model=lm(Weekly_Sales~.,train_data[,columns])
summary(model)
#Predictions
pred = predict(model, test_data[,columns])
#Accuracy
library(Metrics)
print(paste("RMSE: ", rmse(test_data$Weekly_Sales, pred)))
print(paste("R-squared: ", summary(model)$r.squared))
print(paste("Adjusted R-squared: ", summary(model)$adj.r.squared))
