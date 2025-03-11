#Basics of Time Series

wd = '/Users/sonali/Documents/GitHub/MSIS5223/Week6/time-series-basics-ice-Sonali1743/data'
setwd(wd)

global_temp = read.csv('global.txt',header = TRUE, sep = '\t')
class(global_temp)

global_temp_ts = ts(global_temp, frequency = 12, start = c(1856,1))
class(global_temp_ts)
global_temp_ts

#Decompose the time series into its three components and plot them (1 pt)

global_temp_dc = decompose(global_temp_ts)
plot(global_temp_dc)

#Conduct an exponential smoothing on the appropriate components of the data; 
#assess a forecast for normality and homoscedasticity; plot the forecast (1 pt)

es1_global_temp = global_temp_ts - (global_temp_dc$seasonal + global_temp_dc$trend)
es1_global_temp = na.omit(es1_global_temp)
es1 = HoltWinters(es1_global_temp, beta = FALSE, gamma = FALSE)
plot(es1)

es1_fore = forecast(es1, h=100)
plot(es1_fore)

#Assess homoscedasticity
plot(es1_fore$residuals)
lines(c(0, 2015), c(0, 0), col = 'red')
#Residuals are not constant

#Function to plot distribution for residuals
plotForecastErrors = function(forecasterrors,forecasttitle) {
  #Function provided by Avril Coghlan
  forecasterrors = na.omit(forecasterrors)
  # make a histogram of the forecast errors:
  mybinsize = IQR(forecasterrors) / 4
  mysd = sd(forecasterrors)
  mymin = min(forecasterrors) - mysd * 5
  mymax = max(forecasterrors) + mysd * 3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, main=forecasttitle)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}

#Assess normality of residuals
plotForecastErrors(es1_fore$residuals,'Assessing Normal Distribution')
#Residuals are not normally distributed

#Conduct a trend-exponential smoothing on the appropriate components of the data; 
#assess a forecast for normality and homoscedasticity; plot the forecast (1 pt)

es2_global_temp = global_temp_ts - global_temp_dc$seasonal
es2 = HoltWinters(es2_global_temp, gamma = FALSE)
plot(es2)

es2_fore = forecast(es2, h=100)
plot(es2_fore)

#Assess homoscedasticity
plot(es2_fore$residuals)
lines(c(0, 2015), c(0, 0), col = 'red')
#Residuals are not constant

#Assess normality of residuals
plotForecastErrors(es2_fore$residuals,'Assessing Normal Distribution')
#Residuals are not normally distributed

#Using Holt-Winters, assess the global temperature data and create a forecast. 
#Plot your forecast and assess the model. (2 pts)

es3 = HoltWinters(global_temp_ts)
plot(es3)

es3_fore = forecast(es3, h=100)
plot(es3_fore)

#Assess homoscedasticity
plot(es3_fore$residuals)
lines(c(0, 2015), c(0, 0), col = 'red')
#Residuals are not constant

#Assess normality of residuals
plotForecastErrors(es3_fore$residuals,'Assessing Normal Distribution')
#Residuals are not normally distributed

#Create a boxplot to look at seasonality. According to the boxplot, does it 
#appear to have a seasonal component? (2 pts)

boxplot(global_temp_ts~cycle(global_temp_ts))
#The mean global temperatures seem nearly same across all the months indicating almost
#null or minor seasonal component

#Another method to assess the existence of a seasonal component is to calculate 
#the standard deviation of the original time series and compare it to the standard 
#deviation of the time series without its seasonal component.

#Perform this assessment (2 pts)
sd(global_temp_ts)
sd(es2_global_temp)

#How similar are the standard deviations? What does this indicate 
#about a seasonal component? (1 pt)
#Standard deviations are nearly similar (0.2735 and 0.2715) indicating minor 
#seasonal component

