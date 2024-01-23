# This script is intended to run all the forecast training models used by Alex P. 
# This script can be used for any frequency of data
# 2 Parameter Inputs:
# train_ts_annual - ts data training set for running the model
# fcst_ts_annual <- ts full data, the setdiff between fcst_ts_annual and train_ts_annual will be used for computing the accuracy metrics.
# train_dhr <- 1 month backwards your train_ts_annual to train the fourier arima series

# Outputs:
# The output will contain a list within a list
# Results will output the following list
# Actuals = fcst_ts_annual
# Test Length = length of test data
# Fitted Values = forecasting results for the train data
# Accuracy Values = list of accuracy metrics
# Forecast Values = forecasts for the test data
# Lower Limit = Lower Limit for both 80% and 95% confidence level, please take note that croston does not output confidence intervals
# Upper Limit = Upper Limit for both 80% and 95% confidence level, please take note that croston does not output confidence intervals

# You may change this script to add new algorithms/statistical models, and
# add new information to the output.

# to use this script, just source it on the data loading/running script you are using for the specific vertical using the following command
# source("/ForecastTrainingScript.R")

TrainForecastModels <- function(train_ts_annual, fcst_ts_annual){
  #xsource("/Users/verniefortuno/Downloads/1014_SES+Naive.R")
  #######10/14/2022 DIB####### ##################### ##################### ##################### ##################### 
  
 fc_ses.naive.combine <- ses.naive.combine(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual))
 #fc_ses.naive.combine_accuracy <- (forecast::accuracy(fc_ses.naive.combine, fcst_ts_annual))
 fc_ses.naive.combine_accuracy <- accuracy(fc_ses.naive.combine, fcst_ts_annual)
  
  ##################### ##################### ##################### ##################### #####################
    
  fc_naive_ts_annual_3mo <- naive(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80,95))
  autoplot(fc_naive_ts_annual_3mo)
  png(paste0('Naive','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: naive') +
          forecast::autolayer(fc_naive_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_naive_accuracy <- (forecast::accuracy(fc_naive_ts_annual_3mo, fcst_ts_annual))
  
  ########## rwf w/drift - random walk with drift = ARIMA(0,1,0) with optional drift ##########
  fc_rwf_drift_ts_annual_3mo <- rwf(train_ts_annual, drift=TRUE, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_rwf_drift_ts_annual_3mo)
  png(paste0('rwf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: rwf') +
          forecast::autolayer(fc_rwf_drift_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_rwf_drift_accuracy<- forecast::accuracy(fc_rwf_drift_ts_annual_3mo, fcst_ts_annual)
  
  ########## seasonal naive - using most recent season ##########
  fc_snaive_ts_annual_3mo <- snaive(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_snaive_ts_annual_3mo)
  png(paste0('snaive','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: snaive') +
          forecast::autolayer(fc_snaive_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_snaive_accuracy<- accuracy(fc_snaive_ts_annual_3mo, fcst_ts_annual)
  
  ########## meanf - fcst based on average of all observations ##########
  fc_meanf_ts_annual_3mo <- meanf(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual),  level=c(80, 95))
  autoplot(fc_meanf_ts_annual_3mo)
  png(paste0('meanf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: meanf') +
          forecast::autolayer(fc_meanf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_meanf_accuracy<- accuracy(fc_meanf_ts_annual_3mo, fcst_ts_annual)
  
  ########## ses - simple exponential smoothing ##########
  fc_ses_ts_annual_3mo <- ses(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_ses_ts_annual_3mo)
  png(paste0('ses','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: ses') +
          forecast::autolayer(fc_ses_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_ses_accuracy<- accuracy(fc_ses_ts_annual_3mo, fcst_ts_annual)
  
  ########## ses transformed - simple exponential smoothing with BoxCox transformation to stabilize the variance  ##########
  #transform (BoxCox)
  lambdaValue <- BoxCox.lambda(train_ts_annual) #lambda= #find lambda to use for transformation
  autoplot(BoxCox(train_ts_annual, lambda=lambdaValue)) #plot transformed series
  
  #ses transf
  fc_ses_transf_ts_annual_3mo <- ses(train_ts_annual, lambda= lambdaValue, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_ses_transf_ts_annual_3mo)
  png(paste0('ses.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: ses.transf') +
          forecast::autolayer(fc_ses_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_ses_transf_accuracy<-accuracy(fc_ses_transf_ts_annual_3mo, fcst_ts_annual)
  
  ########## Holt's linear trend - exponential smoothing with trend ##########
  fc_holt_ts_annual_3mo <- holt(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_holt_ts_annual_3mo)
  png(paste0('holt','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: holt') +
          forecast::autolayer(fc_holt_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_holt_accuracy<-accuracy(fc_holt_ts_annual_3mo, fcst_ts_annual)
  
  ########## holt linear trend transformed - using BoxCox transformation to stabilize the variance ##########
  #transform (BoxCox)
  #holt linear trend transf
  fc_holt_transf_ts_annual_3mo <- holt(train_ts_annual, lambda=lambdaValue, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_holt_transf_ts_annual_3mo)
  png(paste0('holt.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: holt.transf') +
          forecast::autolayer(fc_holt_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_holt_transf_accuracy <- accuracy(fc_holt_transf_ts_annual_3mo, fcst_ts_annual)
  
  ########## Holt's damped trend method - exponential smoothing with damped trend ##########
  fc_holt_damped_ts_annual_3mo <- holt(train_ts_annual, damped=TRUE, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_holt_damped_ts_annual_3mo)
  png(paste0('holt.damped','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: holt.transf') +
          forecast::autolayer(fc_holt_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_holt_damped_accuracy <- accuracy(fc_holt_damped_ts_annual_3mo, fcst_ts_annual)
  
  #holt damped trend transf
  fc_holt_damped_transf_ts_annual_3mo <- holt(train_ts_annual, lambda= lambdaValue,damped=TRUE, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_holt_damped_transf_ts_annual_3mo)
  png(paste0('holt.damped.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: holt.damped.transf') +
          forecast::autolayer(fc_holt_damped_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_holt_damped_transf_accuracy <- accuracy(fc_holt_damped_transf_ts_annual_3mo, fcst_ts_annual)
  
  ########## croston - intermittent demand forecasting - use simple exponential smoothing on non-zero elements and a separate application of SES to the times between non-zero elements of the time series. ##########
  fc_croston_ts_annual_3mo <- croston(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual))
  autoplot(fc_croston_ts_annual_3mo)
  png(paste0('croston','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: croston') +
          forecast::autolayer(fc_croston_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_croston_accuracy <- accuracy(fc_croston_ts_annual_3mo, fcst_ts_annual)
  
  ########## thetaf - simple exponential smoothing with drift - seasonality, if any, is adjusted using a classical multiplicative decomposition before applying theta method ##########
  fc_thetaf_ts_annual_3mo <- thetaf(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_thetaf_ts_annual_3mo)
  png(paste0('thetaf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: thetaf') +
          forecast::autolayer(fc_thetaf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_thetaf_accuracy <- accuracy(fc_thetaf_ts_annual_3mo, fcst_ts_annual)
  
  
  ########## splinef - local linear forecasts using cubic smoothing splines - equivalent to ARIMA(0,2,2) but with restricted parameter space - benefit over ARIMA = provides smooth historical trend and linear forecast function ##########
  fc_splinef_ts_annual_3mo <- tryCatch({splinef(train_ts_annual, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))},  error=function(cond){message(cond)})
  tryCatch({autoplot(fc_splinef_ts_annual_3mo)}, error=function(cond){message(cond)})
  png(paste0('spline','.', dateTitle, '.png'), width=720, height=560)
  tryCatch({print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: spline') +
          forecast::autolayer(fc_splinef_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))},  error=function(cond){message(cond)})
  dev.off()
  fc_splinef_accuracy <-  tryCatch({accuracy(fc_splinef_ts_annual_3mo, fcst_ts_annual)},  error=function(cond){message(cond)})
  
  
  ########## splinef transformed - using BoxCox transformation to stabilize variance ##########
  #transform (BoxCox)
  fc_splinef_transf_ts_annual_3mo <-tryCatch({splinef(train_ts_annual, lambda= lambdaValue, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))},  error=function(cond){message(cond)})
  tryCatch({autoplot(fc_splinef_transf_ts_annual_3mo)},  error=function(cond){message(cond)})
  png(paste0('spline.transf','.', dateTitle, '.png'), width=720, height=560)
  tryCatch({print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: spline.transf') +
          forecast::autolayer(fc_splinef_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))},  error=function(cond){message(cond)})
  dev.off()
  fc_splinef_transf_accuracy <- tryCatch({accuracy(fc_splinef_transf_ts_annual_3mo, fcst_ts_annual)},  error=function(cond){message(cond)})
  
  
  ########## Holt-Winters' additive method (seasonal component averages to 0) - exponential smoothing with trend and additive seasonality model ##########
  #need to use seasonality where frequency <= 24, higher doesn't work
  fc_hw_add_ts_annual_3mo <- tryCatch({hw(train_ts_annual, seasonal='additive', h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))},  error=function(cond){message(cond)})
  tryCatch({autoplot(fc_hw_add_ts_annual_3mo)},  error=function(cond){message(cond)})
  png(paste0('hw_add','.', dateTitle, '.png'), width=720, height=560)
  tryCatch({print(autoplot(fcst_ts_annual, series='actual') + ggtitle('Actual and Forecast Volumes') +
                    forecast::autolayer(fc_hw_add_ts_annual_3mo$mean, series='forecast') +
                    ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
                    ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))},  error=function(cond){message(cond)})
  dev.off()
  fc_hw_add_accuracy <- tryCatch({accuracy(fc_hw_add_ts_annual_3mo, fcst_ts_annual)},  error=function(cond){message(cond)})
  
  
  ########## Holt-Winters' additive damped method (seasonal component averages to 0) - exponential smoothing with damped trend and additive seasonality model ##########
  #need to use seaonality where frequency <= 24, higher doesn't work
  fc_hw_add_damped_ts_annual_3mo <- tryCatch({hw(train_ts_annual, seasonal='additive', damped=TRUE, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))},  error=function(cond){message(cond)})
  tryCatch({autoplot(fc_hw_add_damped_ts_annual_3mo)}, error=function(cond){message(cond)})
  png(paste0('hw_add_damped','.', dateTitle, '.png'), width=720, height=560)
  tryCatch({print(autoplot(fcst_ts_annual, series='actual') + ggtitle('Actual and Forecast Volumes: Holt Winters Additive Damped') +
                    forecast::autolayer(fc_hw_add_damped_ts_annual_3mo$mean, series='forecast') +
                    ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
                    ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))},  error=function(cond){message(cond)})
  dev.off()
  fc_hw_add_damped_accuracy <- tryCatch({accuracy(fc_hw_add_damped_ts_annual_3mo, fcst_ts_annual)},  error=function(cond){message(cond)})
  
  
  ########## Holt-Winters' mulitplicative method (seasonal component averages to 1) - exponential smoothing with trend and multiplicative seasonality model ##########
  #need to use seaonality where frequency <= 24, higher doesn't work
  fc_hw_mult_ts_annual_3mo <- tryCatch({hw(train_ts_annual, seasonal='multiplicative', h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))},  error=function(cond){message(cond)})
  tryCatch({autoplot(fc_hw_mult_ts_annual_3mo)},  error=function(cond){message(cond)})
  png(paste0('hw_mult','.', dateTitle, '.png'), width=720, height=560)
  tryCatch({
    print(autoplot(fcst_ts_annual, series='actual') +
            ggtitle('Actual and Forecast Volumes: Holt Winters Multiplicative') +
            forecast::autolayer(fc_hw_mult_ts_annual_3mo$mean, series='forecast') +
            ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
            ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))},  error=function(cond){message(cond)})
  dev.off()
  fc_hw_mult_accuracy <- tryCatch({accuracy(fc_hw_mult_ts_annual_3mo, fcst_ts_annual)},  error=function(cond){message(cond)})
  
  
  ########## Holt-Winters' mulitplicative damped method (seasonal component averages to 1) - exponential smoothing with damped trend and multiplicative seasonality model ##########
  #need to use seaonality where frequency <= 24, higher doesn't work
  fc_hw_mult_damped_ts_annual_3mo <- tryCatch({hw(train_ts_annual, seasonal='multiplicative', damped=TRUE, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))},  error=function(cond){message(cond)})
  tryCatch({autoplot(fc_hw_mult_damped_ts_annual_3mo)},  error=function(cond){message(cond)})
  png(paste0('hw_mult_damped','.', dateTitle, '.png'), width=720, height=560)
  tryCatch({
    autoplot(fcst_ts_annual, series='actual') + ggtitle('Actual and Forecast Volumes: Holt Winters Multiplicative Damped Trend') +
      forecast::autolayer(fc_hw_mult_damped_ts_annual_3mo$mean, series='forecast')    +
      ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
      ggplot2::xlab('Time Index') + ggplot2::ylab('Volume')},  error=function(cond){message(cond)})
  dev.off()
  fc_hw_mult_damped_accuracy <- tryCatch({accuracy(fc_hw_mult_damped_ts_annual_3mo, fcst_ts_annual)},  error=function(cond){message(cond)})
  
  ########## ETS (Error, Trend, Seasonal) - Innovations state space model ##########
  #need to use seasonality where frequency <= 24, anything higher ignores seasonality = try stlf() instead
  ets_model_ts_annual_3mo <- ets(train_ts_annual)
  fc_ets_ts_annual_3mo <- forecast(ets_model_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_ets_ts_annual_3mo)
  png(paste0('ets','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: ets') +
          forecast::autolayer(fc_ets_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_ets_model_accuracy <- accuracy(fc_ets_ts_annual_3mo, fcst_ts_annual)
  
  
  ########## ETS transformed - using BoxCox transformation to stabilize variance, but no need to transform ets models since can use multiplicative seasonality directly - this is here strictly for completion ##########
  #need to use seasonality where frequency <= 24, anything higher ignores seasonality  (try stlf() instead)
  #transform (BoxCox)
  
  ets_transf_model_ts_annual_3mo <- ets(train_ts_annual, lambda= lambdaValue)
  fc_ets_transf_ts_annual_3mo <- forecast(ets_transf_model_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual), level=c(80, 95))
  autoplot(fc_ets_transf_ts_annual_3mo)
  png(paste0('ets.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: ets.transf') +
          forecast::autolayer(fc_ets_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_ets_transf_accuracy <- accuracy(fc_ets_transf_ts_annual_3mo, fcst_ts_annual)
  
  ########## stlf - STL (Seasonal and Trend decomposition using Loess) + ETS (Error, Trend, Seasonality) ##########
  # fc_stlf_annual_3mo <- tryCatch({ stlf(train_ts_annual, s.window='periodic', h=length(fcst_ts_annual)-length(train_ts_annual),  level=c(80, 95))},
  #                                error=function(cond){message(cond)})
  # tryCatch({autoplot(fc_stlf_annual_3mo)}, error=function(cond){message(cond)})
  # tryCatch({
  #   png(paste0('stlf','.', dateTitle, '.png'), width=720, height=560)
  #   print(forecast::autoplot(fcst_ts_annual, series='actual') + 
  #           ggplot2::ggtitle('Actual and Forecast Volumes: stlf') +
  #           forecast::autolayer(fc_stlf_annual_3mo$mean, series='forecast') +
  #           ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
  #           ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  #   dev.off()
  # },error=function(cond){message(cond)})
  # fc_stlf_accuracy<- tryCatch({accuracy(fc_stlf_annual_3mo, fcst_ts_annual)}, error=function(cond){message(cond)})
  # 
  ########## stlf transformed - using BoxCox transformation to stabilize variance ##########
  
  # #stlf transf
  # fc_stlf_transf_ts_annual_3mo <- tryCatch({ stlf(train_ts_annual, lambda= lambdaValue, s.window='periodic', h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))}, error=function(cond){message(cond)})
  # tryCatch(autoplot(fc_stlf_transf_ts_annual_3mo), error=function(cond){message(cond)})
  # tryCatch({
  #   png(paste0('stlf.transf','.', dateTitle, '.png'), width=720, height=560)
  #   print(forecast::autoplot(fcst_ts_annual, series='actual') + 
  #           ggplot2::ggtitle('Actual and Forecast Volumes: stlf.transf') +
  #           forecast::autolayer( fc_stlf_transf_ts_annual_3mo$mean, series='forecast') +
  #           ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
  #           ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  #   dev.off()
  # })
  # fc_stlf_transf_accuracy <- tryCatch(accuracy(fc_stlf_transf_ts_annual_3mo, fcst_ts_annual), error=function(cond){message(cond)})
  # 
  ########## ARIMA - AutoRegressive Integrated Moving Average model ##########
  

  
  
  tryCatch({ arima_model_ts_annual_3mo <- auto.arima(train_ts_annual)
  #model uses stepwise selection and the estimation is via conditional sums of squares and the information criteria used for model selection, to go through all models (very slow) and to use maximum likelihood to select model (computationally intensive), add the following:
  #stepwise=FALSE, approximation=FALSE
  fc_arima_ts_annual_3mo <- forecast(arima_model_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_arima_ts_annual_3mo)
  png(paste0('arima','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: arima') +
          forecast::autolayer( fc_arima_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  fc_arima_accuracy <- accuracy(fc_arima_ts_annual_3mo, fcst_ts_annual)
  }, error=function(cond){message(cond)})
  
  ########## ARIMA transformed - use BoxCox transformation to stabilize variance ##########
  tryCatch({ arima_model_ts_annual_3mo <- auto.arima(train_ts_annual)
  arima_model_ts_annual_transf_3mo <- auto.arima(train_ts_annual, lambda= lambdaValue)
  fc_arima_ts_annual_transf_3mo <- forecast(arima_model_ts_annual_transf_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_arima_ts_annual_transf_3mo)
  png(paste0('arima.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: arima.transf') +
          forecast::autolayer( fc_arima_ts_annual_transf_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  arima_transf_accuracy <- accuracy(fc_arima_ts_annual_transf_3mo, fcst_ts_annual)
  }, error=function(cond){message(cond)})
# #1022#######################################################  ############################
  #find optimal K for DHR
#   fc_dhr_ts_annual_2mo  <- list()
#   dhr_model_ts_annual_2mo <- list()
#
#   train_ts_annual_2mo <- train_dhr
#   g <- list()
#   for (i in 1:26){   #10/22 Manually change
#     dhr_model_ts_annual_2mo[[i]] <- auto.arima(train_ts_annual_2mo,xreg = fourier(train_ts_annual_2mo, K = i), seasonal = FALSE, stepwise=FALSE, approximation=FALSE)
#     fc_dhr_ts_annual_2mo [[i]] <- forecast(dhr_model_ts_annual_2mo[[i]], xreg = fourier(train_ts_annual_2mo, K = i, h =length(fcst_ts_annual)-length(train_ts_annual_2mo)), biasadj=TRUE)
#     g[[i]] <- accuracy(fc_dhr_ts_annual_2mo[[i]],  fcst_ts_annual)
#     if(i==1){
#       gmin = g[[1]][2, 2]
#       k=1
#     }else{
#       k = ifelse(g[[i]][2, 2]<gmin, i, k)
#       gmin = ifelse(g[[i]][2, 2]<gmin, g[[i]][2,2], gmin)
#     }
#     cat('k = ', i,'','and', 'RMSE = ' , g[[i]][2,2], '\n')
#   }
#
# #   ########## dhr - dynamic harmonic regression (DHR) - periodic seasonality is handled using pairs of Fourier terms (sums of sin and cos terms); error is modeled as a non-seasonal ARMA process; assumes seasonal pattern is unchanging ##########
  
   k <- 2
   tryCatch({ 
  harmonics_ts_annual_3mo <- fourier(train_ts_annual, K=k) #find optimal K using below code,; K can't be more than half the frequency
  dhr_model_ts_annual_3mo <- auto.arima(train_ts_annual, xreg = harmonics_ts_annual_3mo, seasonal = FALSE)
  #to look through all possible models, use:
  #stepwise=FALSE, approximation=FALSE

  pred_harmonics_ts_annual_3mo <- fourier(train_ts_annual, K=k, h=length(fcst_ts_annual)-length(train_ts_annual)) #this K has to be the same as the one used , h=length(fcst_ts_annual)-length(train_ts_annual))
  fc_dhr_ts_annual_3mo <- forecast(dhr_model_ts_annual_3mo, xreg = pred_harmonics_ts_annual_3mo,level=c(80, 95))
  autoplot(fc_dhr_ts_annual_3mo)
  png(paste0('dhr','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') +
          ggplot2::ggtitle('Actual and Forecast Volumes: dhr') +
          forecast::autolayer( fc_dhr_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) +
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  dhr_model_accuracy <- accuracy(fc_dhr_ts_annual_3mo, fcst_ts_annual)
   }, error=function(cond){message(cond)})
#   ########## dhr transformed - using BoxCox transformation to stabilize variance ##########
#   fc_dhr_transf_ts_annual_2mo  <- list()
#   dhr_model_transf_ts_annual_2mo <- list()
# 
#   train_ts_annual_2mo <- train_dhr
#   g <- list()
# for (i in 1:26){
#   dhr_model_transf_ts_annual_2mo[[i]] <- auto.arima(train_ts_annual_2mo,lambda=lambdaValue, xreg = fourier(train_ts_annual_2mo, K = i), seasonal = FALSE, stepwise=FALSE, approximation=FALSE)
#   fc_dhr_transf_ts_annual_2mo [[i]] <- forecast(dhr_model_transf_ts_annual_2mo[[i]], xreg = fourier(train_ts_annual_2mo, K = i, h =length(fcst_ts_annual)-length(train_ts_annual_2mo)), biasadj=TRUE)
#   g[[i]] <- accuracy(fc_dhr_transf_ts_annual_2mo[[i]],  fcst_ts_annual)
#   if(i==1){
#     gmin = g[[1]][2, 2]
#     k=1
#   }else{
#     k = ifelse(g[[i]][2, 2]<gmin, i, k)
#     gmin = ifelse(g[[i]][2, 2]<gmin, g[[i]][2,2], gmin)
#   }
#   cat('k = ', i,'','and', 'RMSE = ' , g[[i]][2,2], '\n')
# }

#   ########## dhr - dynamic harmonic regression (DHR) - transformed periodic seasonality is handled using pairs of Fourier terms (sums of sin and cos terms); error is modeled as a non-seasonal ARMA process; assumes seasonal pattern is unchanging ##########
  k2 <- 2
   tryCatch({ 
  harmonics_transf_ts_annual_3mo <- fourier(train_ts_annual, K=k2) #find optimal K using below code,; K can't be more than half the frequency
  dhr_model_transf_ts_annual_3mo <- auto.arima(train_ts_annual,lambda=lambdaValue,  xreg = harmonics_transf_ts_annual_3mo, seasonal = FALSE)
  #to look through all possible models, use:
  #stepwise=FALSE, approximation=FALSE

  pred_harmonics_transf_ts_annual_3mo <- fourier(train_ts_annual, K=k2,  h=length(fcst_ts_annual)-length(train_ts_annual)) #this K has to be the same as the one used , h=length(fcst_ts_annual)-length(train_ts_annual))
  fc_dhr_transf_ts_annual_3mo <- forecast(dhr_model_transf_ts_annual_3mo, xreg = pred_harmonics_transf_ts_annual_3mo,level=c(80, 95))
  autoplot(fc_dhr_transf_ts_annual_3mo)
  png(paste0('dhr.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') +
          ggplot2::ggtitle('Actual and Forecast Volumes: dhr transformed') +
          forecast::autolayer( fc_dhr_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) +
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  dhr_transf_model_accuracy <- accuracy(fc_dhr_transf_ts_annual_3mo, fcst_ts_annual)
   }, error=function(cond){message(cond)})
  #1022#######################################################  ############################
  ########## tslm (season, no trend) - time series regression with seasonality, but no trend ##########
  tslm_model_no_trend_ts_annual_3mo <- tslm(train_ts_annual ~ season)
  fc_tslm_no_trend_ts_annual_3mo <- forecast(tslm_model_no_trend_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_no_trend_ts_annual_3mo)
  png(paste0('tslm_nt','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_nt') +
          forecast::autolayer( fc_tslm_no_trend_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_no_trend_accuracy <-accuracy(fc_tslm_no_trend_ts_annual_3mo, fcst_ts_annual)
  
  ########## tslm (linear trend, no season) - time series regression with linear trend but no seasonality ##########
  tslm_model_lt_no_season_ts_annual_3mo <- tslm(train_ts_annual ~ trend)
  fc_tslm_lt_no_season_ts_annual_3mo <- forecast(tslm_model_lt_no_season_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_lt_no_season_ts_annual_3mo)
  png(paste0('tslm_lt_ns','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_lt_ns') +
          forecast::autolayer( fc_tslm_lt_no_season_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_lt_no_season_accuracy <- accuracy(fc_tslm_lt_no_season_ts_annual_3mo, fcst_ts_annual)
  
  #tslm (linear trend, no season) transf
  tslm_model_lt_no_season_transf_ts_annual_3mo <- tslm(train_ts_annual ~ trend, lambda= lambdaValue)#input lambda from above)
  fc_tslm_lt_no_season_transf_ts_annual_3mo <- forecast(tslm_model_lt_no_season_transf_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_lt_no_season_transf_ts_annual_3mo)
  png(paste0('tslm_lt_ns.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_lt_ns.transf') +
          forecast::autolayer( fc_tslm_lt_no_season_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_lt_no_season_transf_accuracy <- accuracy(fc_tslm_lt_no_season_transf_ts_annual_3mo, fcst_ts_annual)
  
  
  ########## tslm (nonlinear trend, no season) - time series regression with nonlinear trend but no seasonality ##########
  tslm_model_nlt_no_season_ts_annual_3mo <- tslm(train_ts_annual ~ trend + I(trend^2))
  fc_tslm_nlt_no_season_ts_annual_3mo <- forecast(tslm_model_nlt_no_season_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_nlt_no_season_ts_annual_3mo)
  png(paste0('tslm_nlt_ns.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_nlt_ns.transf') +
          forecast::autolayer( fc_tslm_nlt_no_season_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_nlt_no_season_accuracy <-accuracy(fc_tslm_nlt_no_season_ts_annual_3mo, fcst_ts_annual)
  
  #tslm (nonlinear trend, no season) transf
  tslm_model_nlt_no_season_transf_ts_annual_3mo <- tslm(train_ts_annual ~ trend + I(trend^(2)), lambda= lambdaValue)
  fc_tslm_nlt_no_season_transf_ts_annual_3mo <- forecast(tslm_model_nlt_no_season_transf_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_nlt_no_season_transf_ts_annual_3mo)
  png(paste0('tslm_nlt_ns.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_nlt_ns.transf') +
          forecast::autolayer( fc_tslm_nlt_no_season_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_nlt_no_season_transf_accuracy <- accuracy(fc_tslm_nlt_no_season_transf_ts_annual_3mo, fcst_ts_annual)
  
  
  ########## tslm (linear trend + season) - time series regression with linear trend and seasonality ##########
  tslm_model_lt_season_ts_annual_3mo <- tslm(train_ts_annual ~ trend + season)
  fc_tslm_lt_season_ts_annual_3mo <- forecast(tslm_model_lt_season_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_lt_season_ts_annual_3mo)
  png(paste0('tslm_lt_s','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_lt_s') +
          forecast::autolayer( fc_tslm_lt_season_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_lt_season_accuracy <- accuracy(fc_tslm_lt_season_ts_annual_3mo, fcst_ts_annual)
  
  #tslm (linear trend + season) transf
  tslm_model_lt_season_transf_ts_annual_3mo <- tslm(train_ts_annual ~ trend + season, lambda= lambdaValue)
  fc_tslm_lt_season_transf_ts_annual_3mo <- forecast(tslm_model_lt_season_transf_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_lt_season_transf_ts_annual_3mo)
  png(paste0('tslm_lt_s.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_lt_s.transf') +
          forecast::autolayer( fc_tslm_lt_season_transf_ts_annual_3mo$mean,series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_lt_season_transf_accuracy <- accuracy(fc_tslm_lt_season_transf_ts_annual_3mo, fcst_ts_annual)
  
  ########## tslm (nonlinear trend + season) - time series regression with nonlinear trend and seasonality ##########
  tslm_model_nlt_season_ts_annual_3mo <- tslm(train_ts_annual ~ trend + I(trend^2) + season)
  fc_tslm_nlt_season_ts_annual_3mo <- forecast(tslm_model_nlt_season_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_nlt_season_ts_annual_3mo)
  png(paste0('tslm_nlt_s','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_nlt_s') +
          forecast::autolayer(fc_tslm_nlt_season_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_nlt_season_accuracy <- accuracy(fc_tslm_nlt_season_ts_annual_3mo, fcst_ts_annual)
  
  ########## tslm (nonlinear trend + season) transformed - using BoxCox transformation to stabilize variance ##########
  #transform (BoxCox)
  
  #tslm (nonlinear trend + season) transf
  tslm_model_nlt_season_transf_ts_annual_3mo <- tslm(train_ts_annual ~ trend + I(trend^(2)) + season, lambda=lambdaValue)
  fc_tslm_nlt_season_transf_ts_annual_3mo <- forecast(tslm_model_nlt_season_transf_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tslm_nlt_season_transf_ts_annual_3mo)
  png(paste0('tslm_nlt_s.transf','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tslm_nlt_s.transf') +
          forecast::autolayer(fc_tslm_nlt_season_transf_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tslm_model_nlt_season_transf_accuracy <- accuracy(fc_tslm_nlt_season_transf_ts_annual_3mo, fcst_ts_annual)
  
  
  ########## TBATS - Trigonometric term for seasonality, BoxCox transformation, ARMA errors, Trend, Seasonal model ##########
  tbats_model_ts_annual_3mo <- tbats(train_ts_annual)
  fc_tbats_ts_annual_3mo <- forecast(tbats_model_ts_annual_3mo, h=length(fcst_ts_annual)-length(train_ts_annual),level=c(80, 95))
  autoplot(fc_tbats_ts_annual_3mo)
  png(paste0('tbats','.', dateTitle, '.png'), width=720, height=560)
  print(forecast::autoplot(fcst_ts_annual, series='actual') + 
          ggplot2::ggtitle('Actual and Forecast Volumes: tbats') +
          forecast::autolayer(fc_tbats_ts_annual_3mo$mean, series='forecast') +
          ggplot2::coord_cartesian(xlim=c(startGraph,endGraph), ylim=c(y_lim1,y_lim2)) + 
          ggplot2::xlab('Time Index') + ggplot2::ylab('Volume'))
  dev.off()
  tbats_model_accuracy <- accuracy(fc_tbats_ts_annual_3mo, fcst_ts_annual)
  
  FittedList <- list('naive'=fc_naive_ts_annual_3mo$fitted,
                     'rwf with drift'=fc_rwf_drift_ts_annual_3mo$fitted,
                     'seasonal naive'=fc_rwf_drift_ts_annual_3mo$fitted,
                     'mean'=fc_meanf_ts_annual_3mo$fitted,
                     'SES' = fc_ses_ts_annual_3mo$fitted,
                     'SES BoxCox Transformed'=fc_ses_transf_ts_annual_3mo$fitted,
                     #'HW Additive' = fc_hw_add_ts_annual_3mo$fitted,
                     #'HW Additive Damped'=fc_hw_add_damped_ts_annual_3mo$fitted,
                     #'HW Multiplicative' = fc_hw_mult_ts_annual_3mo$fitted,
                     #'HW Multiplicative Damped'=fc_hw_mult_damped_ts_annual_3mo$fitted,
                     'Holts Linear Trend'= fc_holt_ts_annual_3mo$fitted,
                     'Holts Linear Trend BoxCox Transformed'=fc_holt_transf_ts_annual_3mo$fitted,
                     'Holts Damped Trend'=fc_holt_damped_ts_annual_3mo$fitted,
                     'Holts Damped Trend BoxCox Transformed'=fc_holt_damped_transf_ts_annual_3mo$fitted,
                     'Croston'=fc_croston_ts_annual_3mo$fitted,
                     'Theta' = fc_thetaf_ts_annual_3mo$fitted,
                     'Spline' =  fc_splinef_ts_annual_3mo$fitted,
                     'Spline BoxCox Transformed'= fc_splinef_transf_ts_annual_3mo$fitted,
                     'ETS' = ets_model_ts_annual_3mo$fitted,
                     'ETS BoxCox Transformed'=ets_transf_model_ts_annual_3mo$fitted,
                     # 'STLF'=fc_stlf_annual_3mo$fitted,
                     # 'STLF BoxCox Transformed' = fc_stlf_transf_ts_annual_3mo$fitted,
                     'ARIMA' = arima_model_ts_annual_3mo$fitted,
                     'ARIMA BoxCox Transformed' = arima_model_ts_annual_transf_3mo$fitted,
                     'DHR' = dhr_model_ts_annual_3mo$fitted,
                     'DHR Transformed' = dhr_model_transf_ts_annual_3mo$fitted,
                     'TSLM No Trend' = tslm_model_no_trend_ts_annual_3mo$fitted.values,
                     'TSLM Linear Trend No Season'= tslm_model_lt_no_season_ts_annual_3mo$fitted.values,
                     'TSLM Linear Trend No Season BoxCox Transformed' = tslm_model_lt_no_season_transf_ts_annual_3mo$fitted.values,
                     'TSLM NonLinear Trend' =tslm_model_nlt_no_season_ts_annual_3mo$fitted.values,
                     'TSLM NonLinear Trend BoxCox Transformed' = tslm_model_nlt_no_season_transf_ts_annual_3mo$fitted.values,
                     'TSLM Linear Trend Season' = tslm_model_lt_season_ts_annual_3mo$fitted.values,
                     # 'TSLM Season Transformed'= tslm_model_lt_season_transf_ts_annual_3mo$fitted.values,
                     'TSLM NonLinear Trend Season' =tslm_model_nlt_season_ts_annual_3mo$fitted.values,
                     'TSLM Linear Trend Season Transformed' = tslm_model_lt_season_transf_ts_annual_3mo$fitted.values,
                     'TSLM NonLinear Trend Season Transformed' =tslm_model_nlt_season_transf_ts_annual_3mo$fitted.values,
                     'TBATS' = tbats_model_ts_annual_3mo$fitted.values,
                     'Combined Naive Seasonal & SES Model' = fc_ses.naive.combine$fitted.values
  )
  
  AccuracyList<- list('naive' =fc_naive_accuracy, 
                      'rwf with drift'=fc_rwf_drift_accuracy, 
                      'seasonal naive'=fc_snaive_accuracy,
                      'mean'= fc_meanf_accuracy,
                      'SES' = fc_ses_accuracy,
                      'SES BoxCox Transformed'=fc_ses_transf_accuracy,
                      #'HW Additive' = fc_hw_add_accuracy,
                      #'HW Additive Damped'=fc_hw_add_damped_accuracy,
                      #'HW Multiplicative' = fc_hw_mult_accuracy,
                      #'HW Multiplicative Damped'=fc_hw_mult_damped_accuracy, 
                      'Holts Linear Trend'= fc_holt_accuracy,
                      'Holts Linear Trend BoxCox Transformed'=fc_holt_transf_accuracy,
                      'Holts Damped Trend'=fc_holt_damped_accuracy,
                      'Holts Damped Trend BoxCox Transformed'=fc_holt_damped_transf_accuracy,
                      'Croston'=fc_croston_accuracy,
                      'Thetaf'=fc_thetaf_accuracy,
                      'Spline'= fc_splinef_accuracy,
                      'Spline BoxCox Transformed'= fc_splinef_transf_accuracy,
                      'ETS' =  fc_ets_model_accuracy,
                      'ETS BoxCox Transformed'=fc_ets_transf_accuracy,
                      # 'STLF'=fc_stlf_accuracy,
                      # 'STLF BoxCox Transformed' = fc_stlf_transf_accuracy,
                      'ARIMA' = fc_arima_accuracy,
                      'ARIMA BoxCox Transformed'=arima_transf_accuracy,
                      'DHR' = dhr_model_accuracy,
                      'DHR Transformed' = dhr_transf_model_accuracy,
                      'TSLM No Trend' = tslm_model_no_trend_accuracy,
                      'TSLM Linear Trend No Season' = tslm_model_lt_no_season_accuracy,
                      'TSLM Linear Trend No Season BoxCox Transformed' = tslm_model_lt_no_season_transf_accuracy,
                      'TSLM NonLinear Trend' =tslm_model_nlt_no_season_accuracy,
                      'TSLM NonLinear Trend BoxCox Transformed' = tslm_model_nlt_no_season_transf_accuracy,
                      'TSLM Linear Trend Season' = tslm_model_lt_season_accuracy,
                      #'TSLM Season Transformed'= tslm_model_lt_season_transf_accuracy,
                      'TSLM NonLinear Trend Season' =tslm_model_nlt_season_accuracy,
                      'TSLM Linear Trend Season Transformed' = tslm_model_lt_season_transf_accuracy,
                      'TSLM NonLinear Trend Season Transformed' =tslm_model_nlt_season_transf_accuracy,
                      'TBATS' = tbats_model_accuracy,
                      'Combined Naive Seasonal & SES Model' =fc_ses.naive.combine_accuracy
  )
  
  ForecastList <- list('naive'=fc_naive_ts_annual_3mo$mean,
                       'rwf with drift'=fc_rwf_drift_ts_annual_3mo$mean,
                       'seasonal naive'=fc_snaive_ts_annual_3mo$mean,
                       'mean'=fc_meanf_ts_annual_3mo$mean,
                       'SES' = fc_ses_ts_annual_3mo$mean,
                       'SES BoxCox Transformed'=fc_ses_transf_ts_annual_3mo$mean,
                       #'HW Additive' = fc_hw_add_ts_annual_3mo$mean,
                       #'HW Additive Damped'=fc_hw_add_damped_ts_annual_3mo$mean,
                       #'HW Multiplicative' = fc_hw_mult_ts_annual_3mo$mean,
                       #'HW Multiplicative Damped'=fc_hw_mult_damped_ts_annual_3mo$mean,
                       'Holts Linear Trend'= fc_holt_ts_annual_3mo$mean,
                       'Holts Linear Trend BoxCox Transformed'=fc_holt_transf_ts_annual_3mo$mean,
                       'Holts Damped Trend'=fc_holt_damped_ts_annual_3mo$mean,
                       'Holts Damped Trend BoxCox Transformed'=fc_holt_damped_transf_ts_annual_3mo$mean,
                       'Croston'=fc_croston_ts_annual_3mo$mean,
                       'Theta' = fc_thetaf_ts_annual_3mo$mean,
                       'Spline' =  fc_splinef_ts_annual_3mo$mean,
                       'Spline BoxCox Transformed'= fc_splinef_transf_ts_annual_3mo$mean,
                       'ETS' = fc_ets_ts_annual_3mo$mean,  #xxxx
                       'ETS BoxCox Transformed'= fc_ets_transf_ts_annual_3mo$mean,  #xxx
                       # 'STLF'=fc_stlf_annual_3mo$mean,
                       # 'STLF BoxCox Transformed' = fc_stlf_transf_ts_annual_3mo$mean,
                       'ARIMA' = fc_arima_ts_annual_3mo$mean, #xxx
                       'ARIMA BoxCox Transformed' = fc_arima_ts_annual_transf_3mo$mean,
                       'DHR' = fc_dhr_ts_annual_3mo$mean,
                       'DHR Transformed' = fc_dhr_transf_ts_annual_3mo$mean,
                       'TSLM No Trend' = fc_tslm_no_trend_ts_annual_3mo$mean,
                       'TSLM Linear Trend No Season'= fc_tslm_lt_no_season_ts_annual_3mo$mean,
                       'TSLM Linear Trend No Season BoxCox Transformed' = fc_tslm_lt_no_season_transf_ts_annual_3mo$mean,
                       'TSLM NonLinear Trend' =fc_tslm_nlt_no_season_ts_annual_3mo$mean,
                       'TSLM NonLinear Trend BoxCox Transformed' = fc_tslm_nlt_no_season_transf_ts_annual_3mo$mean,
                       'TSLM Linear Trend Season' = fc_tslm_lt_season_ts_annual_3mo$mean,
                       #'TSLM Season Transformed'= fc_tslm_lt_season_transf_ts_annual_3mo$mean,
                       'TSLM NonLinear Trend Season' = fc_tslm_nlt_season_ts_annual_3mo$mean,
                       'TSLM Linear Trend Season Transformed' = fc_tslm_lt_season_transf_ts_annual_3mo$mean,
                       'TSLM NonLinear Trend Season Transformed' =fc_tslm_nlt_season_transf_ts_annual_3mo$mean,
                       'TBATS' = fc_tbats_ts_annual_3mo$mean,
                       'Combined Naive Seasonal & SES Model' =fc_ses.naive.combine$mean
                       
  )
  
  UpperLimit <- list('naive'=fc_naive_ts_annual_3mo$upper,
                     'rwf with drift'=fc_rwf_drift_ts_annual_3mo$upper,
                     'seasonal naive'=fc_snaive_ts_annual_3mo$upper,
                     'mean'= fc_meanf_ts_annual_3mo$upper,
                     'SES' = fc_ses_ts_annual_3mo$upper,
                     'SES BoxCox Transformed'=fc_ses_transf_ts_annual_3mo$upper,
                     #'HW Additive' = fc_hw_add_ts_annual_3mo$upper,
                     #'HW Additive Damped'=fc_hw_add_damped_ts_annual_3mo$upper,
                     #'HW Multiplicative' = fc_hw_mult_ts_annual_3mo$upper,
                     #'HW Multiplicative Damped'=fc_hw_mult_damped_ts_annual_3mo$upper,
                     'Holts Linear Trend'= fc_holt_ts_annual_3mo$upper,
                     'Holts Linear Trend BoxCox Transformed'=fc_holt_transf_ts_annual_3mo$upper,
                     'Holts Damped Trend'=fc_holt_damped_ts_annual_3mo$upper,
                     'Holts Damped Trend BoxCox Transformed'=fc_holt_damped_transf_ts_annual_3mo$upper,
                     'Croston'=fc_croston_ts_annual_3mo$upper,
                     'Theta' = fc_thetaf_ts_annual_3mo$upper,
                     'Spline' =  fc_splinef_ts_annual_3mo$upper,
                     'Spline BoxCox Transformed'= fc_splinef_transf_ts_annual_3mo$upper,
                     'ETS' = fc_ets_ts_annual_3mo$upper,  #xxxx
                     'ETS BoxCox Transformed'= fc_ets_transf_ts_annual_3mo$upper,  #xxx
                     # 'STLF'=fc_stlf_annual_3mo$upper,
                     # 'STLF BoxCox Transformed' = fc_stlf_transf_ts_annual_3mo$upper,
                     'ARIMA' = fc_arima_ts_annual_3mo$upper, #xxx
                     'ARIMA BoxCox Transformed' = fc_arima_ts_annual_transf_3mo$upper,
                     'DHR' = fc_dhr_ts_annual_3mo$upper,
                     'DHR Transformed' = fc_dhr_transf_ts_annual_3mo$upper,
                     'TSLM No Trend' = fc_tslm_no_trend_ts_annual_3mo$upper,
                     'TSLM Linear Trend No Season'= fc_tslm_lt_no_season_ts_annual_3mo$upper,
                     'TSLM Linear Trend No Season BoxCox Transformed' = fc_tslm_lt_no_season_transf_ts_annual_3mo$upper,
                     'TSLM NonLinear Trend' =fc_tslm_nlt_no_season_ts_annual_3mo$upper,
                     'TSLM NonLinear Trend BoxCox Transformed' = fc_tslm_nlt_no_season_transf_ts_annual_3mo$upper,
                     'TSLM Linear Trend Season' = fc_tslm_lt_season_ts_annual_3mo$upper,
                     #'TSLM Season Transformed'= fc_tslm_lt_season_transf_ts_annual_3mo$upper,
                     'TSLM NonLinear Trend Season' = fc_tslm_nlt_season_ts_annual_3mo$upper,
                     'TSLM Linear Trend Season Transformed' = fc_tslm_lt_season_transf_ts_annual_3mo$upper,
                     'TSLM NonLinear Trend Season Transformed' =fc_tslm_nlt_season_transf_ts_annual_3mo$upper,
                     'TBATS' = fc_tbats_ts_annual_3mo$upper,
                    'Combined Naive Seasonal & SES Model' = fc_ses.naive.combine$upper
)
  
  
  LowerLimit <- list('naive'=fc_naive_ts_annual_3mo$lower,
                     'rwf with drift'=fc_rwf_drift_ts_annual_3mo$lower,
                     'seasonal naive'=fc_snaive_ts_annual_3mo$lower,
                     'mean'=fc_meanf_ts_annual_3mo$lower,
                     'SES' = fc_ses_ts_annual_3mo$lower,
                     'SES BoxCox Transformed'=fc_ses_transf_ts_annual_3mo$lower,
                     #'HW Additive' = fc_hw_add_ts_annual_3mo$lower,
                     #'HW Additive Damped'=fc_hw_add_damped_ts_annual_3mo$lower,
                     #'HW Multiplicative Damped'=fc_hw_mult_damped_ts_annual_3mo$lower,
                     'Holts Linear Trend'= fc_holt_ts_annual_3mo$lower,
                     'Holts Linear Trend BoxCox Transformed'=fc_holt_transf_ts_annual_3mo$lower,
                     'Holts Damped Trend'=fc_holt_damped_ts_annual_3mo$lower,
                     'Holts Damped Trend BoxCox Transformed'=fc_holt_damped_transf_ts_annual_3mo$lower,
                     'Croston'=fc_croston_ts_annual_3mo$lower,
                     'Theta' = fc_thetaf_ts_annual_3mo$lower,
                     'Spline' =  fc_splinef_ts_annual_3mo$lower,
                     'Spline BoxCox Transformed'= fc_splinef_transf_ts_annual_3mo$lower,
                     'ETS' = fc_ets_ts_annual_3mo$lower,  #xxxx
                     'ETS BoxCox Transformed'= fc_ets_transf_ts_annual_3mo$lower,  #xxx
                     # 'STLF'=fc_stlf_annual_3mo$lower,
                     # 'STLF BoxCox Transformed' = fc_stlf_transf_ts_annual_3mo$lower,
                     'ARIMA' = fc_arima_ts_annual_3mo$lower, #xxx
                     'ARIMA BoxCox Transformed' = fc_arima_ts_annual_transf_3mo$lower,
                     'DHR' = fc_dhr_ts_annual_3mo$lower,
                     'DHR Transformed' = fc_dhr_transf_ts_annual_3mo$lower,
                     'TSLM No Trend' = fc_tslm_no_trend_ts_annual_3mo$lower,
                     'TSLM Linear Trend No Season'= fc_tslm_lt_no_season_ts_annual_3mo$lower,
                     'TSLM Linear Trend No Season BoxCox Transformed' = fc_tslm_lt_no_season_transf_ts_annual_3mo$lower,
                     'TSLM NonLinear Trend' =fc_tslm_nlt_no_season_ts_annual_3mo$lower,
                     'TSLM NonLinear Trend BoxCox Transformed' = fc_tslm_nlt_no_season_transf_ts_annual_3mo$lower,
                     'TSLM Linear Trend Season' = fc_tslm_lt_season_ts_annual_3mo$lower,
                     #'TSLM Season Transformed'= fc_tslm_lt_season_transf_ts_annual_3mo$lower,
                     'TSLM NonLinear Trend Season' = fc_tslm_nlt_season_ts_annual_3mo$lower,
                     'TSLM Linear Trend Season Transformed' = fc_tslm_lt_season_transf_ts_annual_3mo$lower,
                     'TSLM NonLinear Trend Season Transformed' =fc_tslm_nlt_season_transf_ts_annual_3mo$lower,
                     'TBATS' = fc_tbats_ts_annual_3mo$lower,
                    'Combined Naive Seasonal & SES Model' = fc_ses.naive.combine$lower
  )
  
  
  Results <- list('Actuals'= fcst_ts_annual, 
                  'Test Length'=length(fcst_ts_annual)-length(train_ts_annual), 
                  'Fitted Values'= FittedList, 
                  'Accuracy Values'=AccuracyList,
                  'Forecast Values' = ForecastList,
                  'Lower Limit' = LowerLimit,
                  'Upper Limit' = UpperLimit)
  Results
}

