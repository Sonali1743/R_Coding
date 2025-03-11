#Forecasting the Future of Healthcare

hos_data = read.table('calihospital_financial.txt', header = TRUE, sep = '\t')

#Selecting Total net patient revenue for the analysis
hos_net = hos_data$NET_TOT
hos_net_ts = ts(hos_net, frequency = 4, start = c(2007,1))

#Create a boxplot to look at seasonality. According to the boxplot, does it appear 
#to have a seasonal component? (1 pt.)

boxplot(hos_net_ts~cycle(hos_net_ts))
#Yes, Total net patient revenue appears to have a seasonal component; there is shift 
#in mean revenue across quarters; Q2 and Q3 seem to have higher median revenue than Q1 and Q4

hos_net_dc = decompose(hos_net_ts)
sd(hos_net_ts)
sd(hos_net_dc$seasonal)
#The standard deviations are different confirming seasonality

#Determine whether to use an exponential smoothing, trend-adjusted exponential 
#smoothing, or Holt-Winters model for the data by creating a plot of the components (1pt.)

hos_net_dc = decompose(hos_net_ts)
plot(hos_net_dc)
#since the time series has both trend and seasonality components; we would use Holt-Winters model

#After selecting your model, use a pre-determined smoothing parameter(s) for the 
#time series and justify your selection (1 pt.)

model1 = HoltWinters(hos_net_ts, alpha = 0.4, beta = 0.4, gamma = 0.4)
plot(model1)
#We have selected all alpha (level parameter), beta (trend parameter) and gamma
#(seasonality parameter) to be 0.4; the parameters are chosen such that they are
#in middle (between 0 and 1) and place emphasis on both recent and distant observations

#Allow R to determine the smoothing parameter(s); compare your model to R’s model 
#and explain the difference (0.5 pts.)

model2 = HoltWinters(hos_net_ts)
plot(model2)
#Smoothing parameters have changed from 0.4 to:
#0.6727358 for alpha
#0 for beta
#0 for gamma

#Compare your model and R’s model by performing the following:

#Assess homoscedasticity and normality; what do the results suggest? (0.5 pts.)
model1_fore = forecast(model1, h=12)
model2_fore = forecast(model2, h=12)

#Assess homoscedasticity
plot(model1_fore$residuals)
lines(c(2007,2017),c(0,0), col = 'red')

plot(model2_fore$residuals)
lines(c(2007,2017),c(0,0), col = 'red')
#The variance in residuals is not constant in either of the models; the variance
#seems to be high in 2013 and 2014

#Funtion to assess normality 
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
plotForecastErrors(model1_fore$residuals,'Assessing Normal Distribution')
plotForecastErrors(model2_fore$residuals,'Assessing Normal Distribution')
#Model1 (my model) clearly does not indicate normally distributed residuals and 
#seems to be left skewed; #Model2 (R's model) is also not normally distributed but
#has better distribution than Model1

#Forecast 30 periods into the future for both models; assess homoscedasticity and 
#normality for both. Which performs better? (0.5 pts.)
model1_fore2 = forecast(model1, h=30)
model2_fore2 = forecast(model2, h=30)

##Assess homoscedasticity
plot(model1_fore2$residuals)
lines(c(2007,2017),c(0,0), col = 'red')

plot(model2_fore2$residuals)
lines(c(2007,2017),c(0,0), col = 'red')
#The variance in residuals is not constant in either of the models; the variance
#seems to be high in 2013 and 2014

#Assess normality of residuals
plotForecastErrors(model1_fore2$residuals,'Assessing Normal Distribution')
plotForecastErrors(model2_fore2$residuals,'Assessing Normal Distribution')
#Model1 (my model) clearly does not indicate normally distributed residuals and 
#seems to be left skewed; #Model2 (R's model) is also not normally distributed but
#has better distribution than Model1; therefore Model2 (R's model) is performing
#better than Model1 (my model)

#Once complete, perform the previous tasks for a different variable in the table above. (4.5 pts.)

#Selecting Total operating expenses for the analysis
hos_exp = hos_data$TOT_OP_EXP
hos_exp_ts = ts(hos_exp, frequency = 4, start = c(2007,1))

#Create a boxplot to look at seasonality. According to the boxplot, does it appear 
#to have a seasonal component? (1 pt.)

boxplot(hos_exp_ts~cycle(hos_exp_ts))
#Yes, Total operating expenses appears to have a seasonal component; there is shift 
#in mean expenses across quarters; Q2 and Q3 seem to have higher median expense than Q1 and Q4

hos_exp_dc = decompose(hos_exp_ts)
sd(hos_exp_ts)
sd(hos_exp_dc$seasonal)
#The standard deviations are different confirming seasonality

#Determine whether to use an exponential smoothing, trend-adjusted exponential 
#smoothing, or Holt-Winters model for the data by creating a plot of the components (1pt.)

hos_exp_dc = decompose(hos_exp_ts)
plot(hos_exp_dc)
#since the time series has both trend and seasonality components; we would use Holt-Winters model

#After selecting your model, use a pre-determined smoothing parameter(s) for the 
#time series and justify your selection (1 pt.)

model3 = HoltWinters(hos_exp_ts, alpha = 0.4, beta = 0.4, gamma = 0.4)
plot(model3)
#We have selected all alpha (level parameter), beta (trend parameter) and gamma
#(seasonality parameter) to be 0.4; the parameters are chosen such that they are
#in middle (between 0 and 1) and place emphasis on both recent and distant observations

#Allow R to determine the smoothing parameter(s); compare your model to R’s model 
#and explain the difference (0.5 pts.)

model4 = HoltWinters(hos_exp_ts)
plot(model4)
model4
#Smoothing parameters have changed from 0.4 to:
#0.607884 for alpha
#0 for beta
#0.0823646 for gamma

#Compare your model and R’s model by performing the following:

#Assess homoscedasticity and normality; what do the results suggest? (0.5 pts.)
model3_fore = forecast(model3, h=12)
model4_fore = forecast(model4, h=12)

#Assess homoscedasticity
plot(model3_fore$residuals)
lines(c(2007,2017),c(0,0), col = 'red')

plot(model4_fore$residuals)
lines(c(2007,2017),c(0,0), col = 'red')
#The variance in residuals is not constant in either of the models; the variance
#seems to be high in 2013 and 2014

#Assess normality of residuals
plotForecastErrors(model3_fore$residuals,'Assessing Normal Distribution')
plotForecastErrors(model4_fore$residuals,'Assessing Normal Distribution')
#Model3 (my model) clearly does not indicate normally distributed residuals and 
#seems to be left skewed; #Model4 (R's model) is also not normally distributed but
#has better distribution than Model3

#Forecast 30 periods into the future for both models; assess homoscedasticity and 
#normality for both. Which performs better? (0.5 pts.)
model3_fore2 = forecast(model3, h=30)
model4_fore2 = forecast(model4, h=30)

##Assess homoscedasticity
plot(model3_fore2$residuals)
lines(c(2007,2017),c(0,0), col = 'red')

plot(model4_fore2$residuals)
lines(c(2007,2017),c(0,0), col = 'red')
#The variance in residuals is not constant in either of the models; the variance
#seems to be high in 2013 and 2014

#Assess normality of residuals
plotForecastErrors(model3_fore2$residuals,'Assessing Normal Distribution')
plotForecastErrors(model4_fore2$residuals,'Assessing Normal Distribution')
#Model3 (my model) clearly does not indicate normally distributed residuals and 
#seems to be left skewed; #Model4 (R's model) is also not normally distributed but
#has better distribution than Model3; therefore Model4 (R's model) is performing
#better than Model3 (my model)

#Provide a recommendation to Randall Cunningham about your model selection based on your research. (1 pt.)
#Based on my research, I would recommend Model2 for forecasting Total net patient revenue
#and Model4 for forecasting Total operating expenses; these are the models with parameters 
#generated by R; although the residuals for both of these models are not perfectly normally
#distributed or homoscedastic but they are better than model1 and model3



