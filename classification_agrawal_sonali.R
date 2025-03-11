#Classification THA
library(tree)

hos_data = read.csv('MSIS5223/Week3/classification-tha-Sonali1743/data/calihospital.txt',header = TRUE, sep = '\t')
str(hos_data)
hos_data$TypeControl = as.factor(hos_data$TypeControl)
hos_data$Teaching = as.factor(hos_data$Teaching)
hos_data$DonorType = as.factor(hos_data$DonorType)

#Decide on the predictor variables you use for these analyses.

#Using operating income as a target variable, create a tree (1 pt.)
operinc_data = hos_data[c('OperInc','TypeControl','Teaching','DonorType','NoFTE',
                          'NetPatRev','InOperExp','OutOperExp','OperRev','AvlBeds')]
operinc_tree = tree(operinc_data)
operinc_tree
plot(operinc_tree)
text(operinc_tree)

#Using operating revenue as a target variable, create a tree (1 pt.)
operrev_data = hos_data[c('OperRev','TypeControl','Teaching','DonorType','NoFTE',
                          'NetPatRev','InOperExp','OutOperExp','OperInc','AvlBeds')]
operrev_tree = tree(operrev_data)
operrev_tree
plot(operrev_tree)
text(operrev_tree)

#Using TypeControl as a target variable, create a tree (1 pt.)
typecon_data = hos_data[c('TypeControl','Teaching','DonorType','NoFTE','NetPatRev',
                          'InOperExp','OutOperExp','OperRev','OperInc','AvlBeds')]
typecon_tree = tree(TypeControl~.,typecon_data,mindev=1e-6, minsize=2)
typecon_tree
plot(typecon_tree)
text(typecon_tree)

#Using DonorType as a target variable, create a tree (1 pt.)
dontype_data = hos_data[c('DonorType','TypeControl','Teaching','NoFTE','NetPatRev',
                          'InOperExp','OutOperExp','OperRev','OperInc','AvlBeds')]
dontype_tree = tree(DonorType~.,dontype_data,mindev=1e-6, minsize=2)
dontype_tree
plot(dontype_tree)
text(dontype_tree)

#Interpret your findings for these trees. (4 pts.)

#1. Operating income as a target variable - for Control type City/County, the average operating income 
#is significantly lower than the hospitals with Control type District,Investor or Non Profit. 
#Out Operating Expense and number of full time employees also significantly affect the Operating income, 
#with hospitals having higher Operating Expense (more than 292 million) having more income.Further, 
#higher number of FTE means higher operating income. This makes sense since expenses and 
#number of employees indicate the size of the hospital with bigger hospitals having higher operating income.

#2. Operating revenue as a target variable - Net patient revenue is the most important 
#factor in determining Operating revenue which makes sense. Higher Net patient revenue is 
#leading to higher Operating revenue

#3. Using TypeControl as a target variable - Hospitals with lower operating income (<2.66235e+06) and lesser
#number of beds (<61) are more likely to be Non-profit.
#Hospitals with lower operating income (<2.66235e+06) and more than 61 beds are likely to be Non-profit or District
#Hospitals with significantly high operating income are likely to be controlled by Investor
#Hospitals with high operating income and high operating expense are either City/County or Non-profit controlled

#4. Using DonorType as a target variable - Donor type can be determined by the Teaching variable.
#Small/Rural hospitals get donations through Charity, whereas Alumni funds the teaching type hospitals

#Which of these trees would you choose as your best model? Justify your position. (2 pts.)
#I would choose the first model (Operating income as a target variable) since:
#1. It does not seem to be very complex and overfit, thereby, expected to give accurate results on test data
#2. Each leaf has enough sample data (5 or more)
#3. We can estimate the operating income of the hospital. Also, make improvements in other factors to improve income
#TypeControl tree seems to be too complex and overfit; whereas DonorType seems to be underfit
#operrev tree also seems to be good but taking only 1 variable 'NetPatRev' into consideration to predict OperRev

