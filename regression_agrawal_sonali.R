#Regression THA
#Regression Model

hos_dataset = read.csv('data/calihospital.txt', sep = '\t', header = TRUE)
hos_dataset$Teaching=as.factor(hos_dataset$Teaching)
num_var = hos_dataset[c('NoFTE', 'NetPatRev','InOperExp','OutOperExp','OperRev','OperInc','AvlBeds')]
num_var_scaled = as.data.frame(scale(num_var[,1:7]))

#For the logistic regression model, choose between the following variables for use as the 
#dependent variable and build the model: Teaching or DonorType. Provide the output. (2 pts.)
model1 = glm(Teaching~NoFTE*NetPatRev*InOperExp*OutOperExp*OperRev*OperInc*AvlBeds,binomial, data=hos_dataset)
summary(model1)
anova(model1, test="Chisq")

#As per the deviance values and associated p-values, only NoFTE, NetPatRev and InOperExp are relevant
#and significant for predicting Teaching
model1_new = glm(Teaching~NoFTE+NetPatRev+InOperExp, binomial, data=hos_dataset)
summary(model1_new)

#For the multiple regression model, build two models, one using Operating Income as the 
#dependent variable and another using Operating Revenue. (1 pt.)
model2 = lm(OperInc~NoFTE+NetPatRev+InOperExp+OutOperExp+OperRev+AvlBeds, data=num_var_scaled)
summary(model2)

model3 = lm(OperRev~NoFTE+NetPatRev+InOperExp+OutOperExp+OperInc+AvlBeds, data=num_var_scaled)
summary(model3)

#Check all the assumptions of regression for validity and show your work. (3 pts.)
#1. Linearity
pairs(num_var_scaled, panel = panel.smooth)

#The plots with OperInc as dependent variable and other numeric variables as independent do not look linear
#OperRev seems to have linear relationship with all other numeric variables except OperInc 

#2. Multicollinearity
library(car)
vif(model2)
vif(model3)

#In both the models, all the variables except AvlBeds are correlated with other variables 
#(high vif score, >10) thereby Multicollinearity assumption is not met

#3.Homescedasticity
plot(model2)
plot(model3)
#For both the models, the variance in residuals is not constant across predicted values, due to the outliers
#the variance seems to be maximum in the center, thereby assumption is not met

#4. Normality
plot(model2)
plot(model3)
#For both the models, the distribution for residuals is not following the normal 
#distribution line thereby assumption is not met

#5. Independence
durbinWatsonTest(model2)
#p-value is not significant therefore we cannot reject the null hypothesis that the 
#residuals are independent; assumption is valid

durbinWatsonTest(model3)
#p-value is not significant therefore we cannot reject the null hypothesis that the 
#residuals are independent; assumption is valid

#With your multiple regression models built, you need to assess them. 
#Interpret your findings for these models. That is, explain:

#which variables are included and others are not (1 pt.)

#For model2 (OperInc as target variable), F-statistic p-value is significant therefore, 
#overall model is significant. In terms of variables, t-statistic p-value is significant 
#for all numeric variables (NoFTE, NetPatRev, InOperExp, OutOperExp,OperRev and AvlBeds), therefore
#all variables should be included in the model

#For model3 (OperRev as target variable), F-statistic p-value is significant therefore, 
#overall model is significant. In terms of variables, t-statistic p-value is significant 
#for all numeric variables (NoFTE, NetPatRev, InOperExp, OutOperExp,OperInc and AvlBeds), therefore
#all variables should be included in the model

#the interpretation of your variables (1 pt.)

#OperRev has the maximum affect on OperInc since a unit increase in OperRev leads to
#5.940 increase in OperInc (keeping everything else constant); OperInc has negative relationship with
#NetPatRev, InOperExp, OutOperExp and AvlBeds; an increase in these variables leads to decrease in OperInc

#InOperExp has the maximum affect on OperRev; OperRev has negative relationship with NoFTE and 
#positive relationship with all other variables

#whether the results make sense from a real-world perspective (1 pt.),

#For both the models, the adjusted R-squared is 1 which means that the models are 100% accurate
#in predicting the values for target variables, however, it is not possible in the real world;
#Also model2 does not make sense since operating income should have positive relationship with 
#NetPatRev and AvlBeds (unlike in the model); the relationship between the target and independent 
#variables in model3 make sense from a real-world perspective

#and determine which model performs the best. (1 pt.)
#Between these 2 models, model3 (with OperRev as the target variable) looks better
#since the relationship between the target and independent variables in model3 makes 
#more sense from a real-world perspective than model2

#Since the assumptions were not met in the above models; the following transformations have been done

num_var_scaled$trans_OperInc = log(num_var_scaled$OperInc+5)
num_var_scaled$trans_OperRev = log1p(num_var_scaled$OperRev)

model4 = lm(trans_OperInc~NoFTE+NetPatRev+InOperExp+OutOperExp+OperRev+AvlBeds, data=num_var_scaled)
summary(model4)
model5 = lm(trans_OperRev~NoFTE+NetPatRev+InOperExp+OutOperExp+OperInc+AvlBeds, data=num_var_scaled)
summary(model5)

#Testing assumptions:
pairs(num_var_scaled,panel = panel.smooth)
plot(model4)
plot(model5)
vif(model4)
vif(model5)
durbinWatsonTest(model4)
durbinWatsonTest(model5)

#Looking at the assumption tests for transformed variables:
#For model4, although linearity between transformed OperInc and other variables has improved
#but it still has high multicollinearity; also residual distribution is not normal and 
#homoscedasticity assumption is still not being met
#For model5, the residual distribution is now nearly normal and variance in residuals also
#look more constant than before

#which variables are included?the interpretation of your variable?whether the results 
#make sense from a real-world perspective?
#For model5 (transformed OperRev as target), only 2 variables are included - InOperExp and
#AvlBeds since T-statistic p-value is significant for only these 2 variables. Overall
#the model is significant and can help in predicting target variable; target variable
#decreases by 0.54275 when InOperExp increases by 1 unit (keeping other variables
#constant); also it increases by 0.40883 when AvlBeds increases by 1 unit (keeping 
#other variables constant); adjusted R-square is 96% which is very good; it makes sense from 
#a real-world perspective

#I feel model5 is the best model since it is meeting most of the assumptions unlike
#other models and is statistically significant with high R-square value
