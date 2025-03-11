#Crop Yield Data
data = read.table('data/splityield.txt', header = TRUE)

data$block = as.factor(data$block)
data$irrigation = as.factor(data$irrigation)
data$density = as.factor(data$density)
data$fertilizer = as.factor(data$fertilizer)

library(fastDummies)
data = dummy_cols(data, select_columns = 'block')
data = dummy_cols(data, select_columns = 'irrigation')
data = dummy_cols(data, select_columns = 'density')
data = dummy_cols(data, select_columns = 'fertilizer')

colnames(data)

#Perform a regression using this dataset and all of the variables. (2 pts)
reg1 = lm(yield ~ block_A + block_B + block_C + irrigation_control + density_high +
            density_low + fertilizer_N + fertilizer_NP, data = data)
summary(reg1)

#Provide the equation and explain it. (2 pts)
##Regression equation is "yield = 116.2639 - 21.4444*irrigation_control - 10.8750*density_low"
##We can interpret that irrigation_control and density_low are statistically significant variables.
##Yield decreases by 21.4444 when irrigation is from 'non-irrigated' or 'control' group 
##(keeping all other factors constant). Also, it decreases by 10.8750 when density of seeds sown
##is low (keeping all other factors constant).

#Please explain (i.e. interpret) the output of your regression model. (2 pts)
##The p-value for our regression model is less than 5% implying that our model is
##statistically significant and can help in predicting yield. The Adjusted R-squared 
##is 0.4521 meaning ~45% of the variance in the target variable (yield) can be 
##explained through this model. Also, irrigation_control and density_low are statistically
##significant variables. All other variables are insignificant and do not impact yield.

#Determine which variables significantly influence crop yield. Do they make sense? (2 pts)
##irrigation_control and density_low significantly influence crop yield. Yes, this
##makes sense as we would expect the yield to increase if the crop is irrigated. Also,
##we would expect yield to decrease if the density of seeds sown is low

#Generate a new model including only the significant variables. 
#Explain how this model performs better or worse than the previous one. (2 pts)
reg2 = lm(yield ~ irrigation_control + density_low, data = data)
summary(reg2)
##The Adjusted R-squared for this model is  0.4053 meaning 40.5% of the variance 
##in the target variable (yield) can be explained through this model, whereas it was
##~45% for the previous model. Therefore, previous model is performing better than this model.

