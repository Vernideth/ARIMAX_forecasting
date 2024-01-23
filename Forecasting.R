
#install.packages("broom", type="binary")
# install.packages("haven")
# install.packages("rlang")
# install.packages("withr")
# install.packages("tidyverse")
# install.packages("fpp2")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("RCurl")
# install.packages("ggplot")
# install.packages("forecast", dependencies = TRUE)
# install.packages("magrittr")

#load libraries
library(haven)
library(tidyverse)
library(forecast)
library(ggplot2)
library(RCurl)
library(googledrive)
library(magrittr)
library(dplyr)
library(zoo)
library(lubridate)
library(dplyr)
library(reshape)
library(plyr)
library(readxl)


#call script to use for forecast training
source("/Users/verniefortuno/Downloads/1014_ForecastTrainingScript (3).R")
#call script to use for DHR k
source("/Users/verniefortuno/Downloads/0823_ForecastTrainingScript.R")


#set filepath for results
file_path_out <- '/Users/verniefortuno/Documents/SSH Initial Forecast Creation'

#import dataset
SSH_Raw <- read_excel("/Users/verniefortuno/Documents/SSH Initial Forecast Creation/KDD_STAT280/FILES/KDD_YouTube_email_chat.xlsx", sheet = "email_data")
SSH_Raw$Date <- as.Date(SSH_Raw$Date,format = '%m/%d/%Y')
vol_weekly <-SSH_Raw

#check seasonality
#annual weekly freq=52
fcst_ts <- ts(vol_weekly[,2], frequency=52)
autoplot(fcst_ts)
ggsubseriesplot(fcst_ts) #+ ylim(0,600)

#######
#decompose data (stl)
#annual freq=52, monthly freq=4
fcst_ts <- ts(vol_weekly[,2], frequency=52)
fcst_ts_vector <- as.vector(fcst_ts)
fcst_ts_vector_stl <- stl(ts(fcst_ts_vector, freq=12), s.window='periodic') #, robust=TRUE)  #robust = robust to outliers so that occasional unusual observations will not affect the estimates of the trend-cycle and seasonal components

autoplot(fcst_ts_vector_stl)
autoplot(trendcycle(fcst_ts_vector_stl))
autoplot(seasonal(fcst_ts_vector_stl))
autoplot(remainder(fcst_ts_vector_stl))
autoplot(seasadj(fcst_ts_vector_stl))


#convert original data to ts object

#05/26/2020 Added May Start Date##
fcst_ts_annual <- ts(vol_weekly[,2],frequency=52,start=decimal_date(ymd("2020-01-05"))) #annual seasonality
autoplot(fcst_ts_annual)

ggseasonplot(fcst_ts_annual)
gglagplot(fcst_ts_annual, lags=52)

ggAcf(fcst_ts_annual)
ggPacf(fcst_ts_annual)

head(fcst_ts_annual, n=4)
tail(fcst_ts_annual, n=4)

# Assuming vol_weekly is your data frame with columns Date, volume, and exog_column
# Make sure that the Date column is in a Date format

#Added Start Date##
fcst_ts_annual <- ts(vol_weekly[, 2], frequency = 52, start = decimal_date(ymd("2020-01-05"))) #annual seasonality

# ACF
acf_result <- acf(fcst_ts_annual, plot = FALSE)

# PACF
pacf_result <- pacf(fcst_ts_annual, plot = FALSE)

# Print the results
print("ACF:")
print(acf_result)

print("PACF:")
print(pacf_result)

#create train sets
#weeks consider the last forecasting week of the previous year
train_ts_annual_3mo <- window(fcst_ts_annual, end=c(2020,157)) #13 weeks 52

autoplot(train_ts_annual_3mo)


# Print accuracy measures
#set parameters
dateTitle <- Sys.Date()
vertical <- '1342'
startGraph <- 2021
endGraph <- 2024.9
y_lim1 <- 0
y_lim2 <- 15000
dateTitle <- as.Date(Sys.Date(), format='%m.%d.%Y')

#call forecast training script
ssh.results <- TrainForecastModels(train_ts_annual_3mo,fcst_ts_annual)

##wrapped table Forecast Accuracy
A <- ssh.results$`Accuracy Values`
x <- lapply(A,"[",2,,drop=FALSE)
y <- ldply(x, data.frame)

forecast.Accuracy.save <- y %>% write.csv(paste(file_path_out, "/forecast_accuracy.csv",sep = "/"))

###Wrapped table for forecast values
forecast.Values <- ssh.results$`Forecast Values`
forecast.Values[lengths(forecast.Values) == 0] <- 0
forecast.Values.save <- as.data.frame(lapply(forecast.Values, unlist)) %>% 
  write.csv(paste(file_path_out, "/forecast.csv",sep = "/"))


#data set for Forecast Accuracy Dashboard
#fa dashboard
fa <- as.data.frame(lapply(y, unlist))

#add months, add last update
long <- melt(fa,id.vars = ".id") %>% 
  arrange(variable, value) %>% 
  group_by(variable) %>% 
  dplyr::mutate(rank = row_number()) %>%  
  ungroup()

head(long)

long$date <- '2022-07-09'

g <- long %>% 
  write.csv(paste(file_path_out, "/MAPE.csv",sep = "/"))


#set max y for graph
max_y <- max(train_ts_annual_3mo)
max_x_train <-2024.35
min_x_train <-2019.4


### For Testing
############ simple naive - using most recent observation ##########
fc_naive_ts_annual_3mo <- naive(train_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual_3mo))
autoplot(fc_naive_ts_annual_3mo)
autoplot(fcst_ts_annual, series='actual') + ggtitle('Actual and Forecast Volumes') +
  forecast::autolayer(fc_naive_ts_annual_3mo$mean, series='forecast') +
  coord_cartesian(xlim=c(min_x_train,max_x_train), ylim=c(0,max_y)) + xlab('Time Index') + ylab('Volume')
accuracy(fc_naive_ts_annual_3mo, fcst_ts_annual)
write.csv(fc_naive_ts_annual_3mo, paste(file_path_out,"/naive.csv",sep = "/"))


########## ETS (Error, Trend, Seasonal) - Innovations state space model ##########
#need to use seaonality where frequency <= 24, anything higher ignores seasonality = try stlf() instead
ets_model_ts_annual_3mo <- ets(train_ts_annual_3mo)
fc_ets_ts_annual_3mo <- forecast(ets_model_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual_3mo))
autoplot(fc_ets_ts_annual_3mo)
autoplot(fcst_ts_annual, series='actual') + ggtitle('Actual and Forecast Volumes') +
  forecast::autolayer(fc_ets_ts_annual_3mo$mean, series='forecast') +
  coord_cartesian(xlim=c(min_x_train,max_x_train), ylim=c(0,max_y)) + xlab('Time Index') + ylab('Volume')
accuracy(fc_ets_ts_annual_3mo, fcst_ts_annual)
write.csv(fc_ets_ts_annual_3mo, paste(file_path_out,"/ets.csv",sep = "/"))

AIC_ets <- AIC(ets_model_ts_annual_3mo)

########## ARIMA ##########
arima_model_ts_annual_3mo <- auto.arima(train_ts_annual_3mo, stepwise=FALSE, approximation=FALSE)
fc_arima_ts_annual_3mo <- forecast(arima_model_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual_3mo))
autoplot(fc_arima_ts_annual_3mo)
autoplot(fcst_ts_annual, series='actual') + ggtitle('Actual and Forecast Volumes') +
  forecast::autolayer(fc_arima_ts_annual_3mo$mean, series='forecast') +
  coord_cartesian(xlim=c(min_x_train,max_x_train), ylim=c(0,max_y)) + xlab('Time Index') + ylab('Volume')
accuracy(fc_arima_ts_annual_3mo, fcst_ts_annual)
write.csv(fc_arima_ts_annual_3mo, paste(file_path_out,"/arima1.csv",sep = "/"))


AIC_arima <- AIC(arima_model_ts_annual_3mo)

########### ARIMAX ##################
# Prepare time series data
ts_data <- ts(vol_weekly$volume, start = c(2020, 1), frequency = 52)

# Split the data into training and testing sets
train_length <- floor(0.78 * length(ts_data))
train_data <- window(ts_data, end = c(2020,156))
test_data <- window(ts_data, start = c(2022,1))

# Fit ARIMAX model using training data
exog_train <- vol_weekly$exog_column[1:train_length]
arimax_model <- auto.arima(train_data, xreg = matrix(exog_train, ncol = 1), seasonal = TRUE)

# Forecast using the fitted model on the test data
exog_test <- vol_weekly$exog_column[(train_length + 1):(length(ts_data))]
arimax_forecast <- forecast(arimax_model, xreg = matrix(exog_test, ncol = 1))

# Plot Results
autoplot(ts_data) +
  autolayer(arimax_forecast$mean, series = "ARIMAX Forecast") +
  autolayer(test_data, series = "Actual") +
  labs(title = "ARIMAX Forecast vs Actual")

# Save Results to CSV
write.csv(arimax_forecast, paste(file_path_out, "/results/arimax.csv", sep = "/"))

AIC_arimax <- AIC(arimax_model)

accuracy(arimax_forecast, fcst_ts_annual)

# Compute accuracy for training data
accuracy_train <- accuracy(arimax_model, xreg = matrix(exog_train, ncol = 1))
cat("Accuracy on training data:\n")
print(accuracy_train)

# Compute accuracy for test data
accuracy_test <- accuracy(arimax_forecast, xreg = matrix(exog_test, ncol = 1))
cat("\nAccuracy on test data:\n")
print(accuracy_test)

#################### future forecasts ####################

#set horizon 
h_1 <- 52

############ simple naive - using most recent observation ##########
fc_naive_ts_annual <- naive(fcst_ts_annual, h=h_1, level=c(0,10))
autoplot(fc_naive_ts_annual)
autoplot(fcst_ts_annual, series='actual') + ggtitle('Naive (random walk) forecast') +
  forecast::autolayer(fc_naive_ts_annual$mean, series='forecast') +
  coord_cartesian(xlim=c(2019.4,2025.35), ylim=c(0,max_y)) + xlab('Time Index') + ylab('Volume')
accuracy(fc_naive_ts_annual)
write.csv(fc_naive_ts_annual, paste(file_path_out,"/results/naive.csv",sep = "/"))


f_naive <- function(x, h) {
  naive(x, h=h)
}

e_naive <- tsCV(fcst_ts_annual, f_naive, h=8)
rmse1_naive <- sqrt(mean(e_naive^2, na.rm=TRUE))
rmse1_naive
rmse2_naive <- sqrt(colMeans(e_naive^2, na.rm = TRUE))
rmse2_naive


########## ETS (Error, Trend, Seasonal) - Innovations state space model ##########
#need to use seaonality where frequency <= 24, anything higher ignores seasonality = try stlf() instead
ets_model_ts_annual <- ets(fcst_ts_annual)
fc_ets_ts_annual <- forecast(ets_model_ts_annual, h=h_1, level=c(0,10))
autoplot(fc_ets_ts_annual)
autoplot(fcst_ts_annual, series='actual') + ggtitle('ETS (state space model) forecast') +
  forecast::autolayer(fc_ets_ts_annual$mean, series='forecast') +
  coord_cartesian(xlim=c(2019.4,2022.35), ylim=c(0,max_y)) + xlab('Time Index') + ylab('Volume')
accuracy(fc_ets_ts_annual)
write.csv(fc_ets_ts_annual, paste(file_path_out,"/results/ets.csv",sep = "/"))

f_ets <- function(x, h) {
  forecast(ets(x), h=h)
}

e_ets <- tsCV(fcst_ts_annual, f_ets, h=8)
rmse1_ets <- sqrt(mean(e_ets^2, na.rm=TRUE))
rmse1_ets
rmse2_ets <- sqrt(colMeans(e_ets^2, na.rm = TRUE))
rmse2_ets


########## ARIMA ##########
arima_model_ts_annual <- auto.arima(fcst_ts_annual, stepwise=FALSE, approximation=FALSE)
fc_arima_ts_annual <- forecast(arima_model_ts_annual, h=h_1, level=c(0,20))
autoplot(fc_arima_ts_annual)
autoplot(fcst_ts_annual, series='actual') + ggtitle('ARIMA (AutoRegressive Integrated MovingAverage) forecast') +
  forecast::autolayer(fc_arima_ts_annual$mean, series='forecast') +
  coord_cartesian(xlim=c(2019.4,2022.7), ylim=c(0,max_y)) + xlab('Time Index') + ylab('Volume')
accuracy(fc_arima_ts_annual)
write.csv(fc_arima_ts_annual, paste(file_path_out,"/results/arima.csv",sep = "/"))


f_arima <- function(x, h) {
  forecast(auto.arima(x, stepwise=FALSE, approximation=FALSE), h=h)
}

e_arima <- tsCV(fcst_ts_annual, f_arima, h=8)
rmse1_arima <- sqrt(mean(e_arima^2, na.rm=TRUE))
rmse1_arima
rmse2_arima <- sqrt(colMeans(e_arima^2, na.rm = TRUE))
rmse2_arima


########### ARIMAX #####################
# Combine the time series and exogenous variable into a data frame
data_df <- data.frame(y = ts_data, xreg = vol_weekly$exog_column)

# Check the structure of the data frame
str(data_df)

# Fit ARIMAX model
arimax_model <- auto.arima(data_df$y, xreg = data_df$xreg, seasonal = TRUE)

# Forecast using ARIMAX model
arimax_forecast <- forecast(arimax_model, xreg = data_df$xreg)
write.csv(arimax_forecast, paste(file_path_out, "/results/arimax.csv", sep = "/"))

accuracy(arimax_forecast)
v <- AIC(arimax_model)

#### PLOT ALL 52 WEEKS FORECAST ##
autoplot(fcst_ts_annual, series = 'actual') +
  ggtitle('Forecast Comparison') +
  forecast::autolayer(fc_naive_ts_annual$mean, series = 'Naive') +
  forecast::autolayer(fc_arima_ts_annual$mean, series = 'ARIMA') +
  forecast::autolayer(arimax_forecast$mean, series = 'ARIMAX') +
  coord_cartesian(xlim = c(2019.4, 2025.35), ylim = c(0, 30000.0)) +
  xlab('Time Index') +
  ylab('Volume')




# Assuming exog_train is the exogenous variable data for the training period
arimax_model_ts_annual_3mo <- auto.arima(train_ts_annual_3mo, xreg = exog_train, stepwise = FALSE, approximation = FALSE)
fc_arimax_ts_annual_3mo <- forecast(arimax_model_ts_annual_3mo, xreg = exog_test, h = length(fcst_ts_annual) - length(train_ts_annual_3mo))

autoplot(fc_arimax_ts_annual_3mo)
autoplot(fcst_ts_annual, series = 'actual') + ggtitle('Actual and Forecast Volumes') +
  forecast::autolayer(fc_arimax_ts_annual_3mo$mean, series = 'forecast') +
  coord_cartesian(xlim = c(min_x_train, max_x_train), ylim = c(0, max_y)) + xlab('Time Index') + ylab('Volume')

accuracy(fc_arimax_ts_annual_3mo, fcst_ts_annual)
write.csv(fc_arimax_ts_annual_3mo, paste(file_path_out, "/arimax.csv", sep = "/"))

xyz <- AIC(arimax_model_ts_annual_3mo)
