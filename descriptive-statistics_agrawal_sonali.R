library(tidyverse)

#Reading files
hospital = read.csv('data/CaliforniaHospitalData.csv', header = TRUE)
employee_info = read.csv('data/CaliforniaHospitalData_Personnel.txt', header = TRUE, sep = '\t')

#Merge Data Files and Format Dataframe (2 pts.)
#Merge the two data files
merged_df = merge(hospital,employee_info)
write.table(merged_df,file="merged_df",sep = '\t')

#Convert any date-time columns into a datetime datatype
merged_df[['StartDate']] = as.Date(merged_df[['StartDate']],"%m/%d/%Y")

#Ensure factor data has the factor data type
cols = c('TypeControl','Teaching','DonorType','Gender','PositionID')
merged_df[cols] = lapply(merged_df[cols],as.factor)
str(merged_df)
#Provide a summary of the mean, median, minimum value, and maximum value for each numeric variable
num_cols = unlist(lapply(merged_df,is.numeric))
summary(merged_df[,num_cols])

#Provide a summary of the same statistics using the Tidyverse way
summary_td = merged_df %>%
  summarise(across(c('HospitalID','NoFTE','NetPatRev','InOperExp','OutOperExp','OperRev','OperInc','AvlBeds','Work_ID','Compensation','MaxTerm'), list(mean=mean, median=median, min = min, max=max)))
summary_td

#Add a New Record (2 pts.)
merged_df_new = rbind(merged_df, data.frame(HospitalID=33283, Name='Sutter Lakeside Hospital', Zip='95453-6112',	Website='www.sutterlake.org', TypeControl='Non Profit',	Teaching='Small/Rural',	DonorType='Charity',	NoFTE=353,	NetPatRev=173227.4863,	InOperExp=28647611.24,	OutOperExp=39621438.76,	OperRev=64464737,	OperInc=-3804313,	AvlBeds=25, Work_ID=704361,	LastName='Agrawal',	FirstName='Sonali',	Gender='F',	PositionID='3',	PositionTitle='Acting Director',	Compensation=248904, MaxTerm=8,	StartDate='2010-01-19'))
merged_df_new %>% as_tibble() %>% print(n=62)
write.table(merged_df_new,file="merged_df_new",sep = '\t')

#Descriptive Analysis (6 pts.)
#Provide summary statistics of your text variables
char_cols = unlist(lapply(merged_df_new,is.character))
summary(merged_df_new[,char_cols])

#Provide summary statistics of your categorical variables
cat_cols = unlist(lapply(merged_df_new,is.factor))
summary(merged_df_new[,cat_cols])

#Create histograms
par(mfrow=c(3,3))
hist(merged_df_new$NoFTE, main='Full-time employees')
hist(merged_df_new$NetPatRev, main='Net patient revenue')
hist(merged_df_new$InOperExp, main='Inpatient operating costs (Estimate)')
hist(merged_df_new$OutOperExp, main='Outpatient operating costs (Estimate)')
hist(merged_df_new$OperRev, main='Operating revenue')
hist(merged_df_new$OperInc, main='Operating income')
hist(merged_df_new$AvlBeds, main='Available beds')

#Create scatterplots using net patient revenue as the target variable
par(mfrow=c(3,2))
plot(merged_df_new$NoFTE,merged_df_new$NetPatRev)
abline(lm(merged_df_new$NetPatRev ~ merged_df_new$NoFTE, data=merged_df_new), col='blue')
#There is positive linear relationship between number of full-time employees and net patient revenue; as the number of employees increases, net patient revenue also increases

plot(merged_df_new$InOperExp,merged_df_new$NetPatRev)
abline(lm(merged_df_new$NetPatRev ~ merged_df_new$InOperExp, data=merged_df_new), col='blue')
#There is positive linear relationship between inpatient operating costs (estimate) and net patient revenue; as the inpatient operating costs (estimate) increase, net patient revenue also increases

plot(merged_df_new$OutOperExp,merged_df_new$NetPatRev)
abline(lm(merged_df_new$NetPatRev ~ merged_df_new$OutOperExp, data=merged_df_new), col='blue')
#There is positive linear relationship between outpatient operating costs (estimate) and net patient revenue; as the outpatient operating costs (estimate) increase, net patient revenue also increases

plot(merged_df_new$OperRev,merged_df_new$NetPatRev)
abline(lm(merged_df_new$NetPatRev ~ merged_df_new$OperRev, data=merged_df_new), col='blue')
#There is strong positive linear relationship between operating revenue and net patient revenue; as the operating revenue increases, net patient revenue also increases

plot(merged_df_new$OperInc,merged_df_new$NetPatRev)
abline(lm(merged_df_new$NetPatRev ~ merged_df_new$OperInc, data=merged_df_new), col='blue')
#There seem to be weak positive linear relationship between operating income and net patient revenue; as the operating income increases, net patient revenue also increases

plot(merged_df_new$AvlBeds,merged_df_new$NetPatRev)
abline(lm(merged_df_new$NetPatRev ~ merged_df_new$AvlBeds, data=merged_df_new), col='blue')
#There seem to be positive linear relationship between available beds and net patient revenue; as the number of available beds increases, net patient revenue also increases

#Create a boxplot and assess the lack or presence of outliers
par(mfrow=c(3,3))
boxplot(merged_df_new$NoFTE, main='Full-time employees')
#There are extremely high values (greater than 1.5 times the inter-quartile range)

boxplot(merged_df_new$NetPatRev, main='Net patient revenue')
#There are extremely high values (greater than 1.5 times the inter-quartile range)

boxplot(merged_df_new$InOperExp, main='Inpatient operating costs (Estimate)')
#There are extremely high values (greater than 1.5 times the inter-quartile range)

boxplot(merged_df_new$OutOperExp, main='Outpatient operating costs (Estimate)')
#There are extremely high values (greater than 1.5 times the inter-quartile range)

boxplot(merged_df_new$OperRev, main='Operating revenue')
#There are extremely high values (greater than 1.5 times the inter-quartile range)

boxplot(merged_df_new$OperInc, main='Operating income')
#There are outliers (either greater or lower than 1.5 times the inter-quartile range)

boxplot(merged_df_new$AvlBeds, main='Available beds')
#There are extremely high values (greater than 1.5 times the inter-quartile range)

#Choose three variables and create a QQ plot, providing assessment of the normality
par(mfrow=c(3,1))
qqnorm(merged_df_new$NoFTE)
qqline(merged_df_new$NoFTE, lty=2, col='blue')
#The plot(in black) deviates from the normal distribution line(in blue), thereby we can tell that distribution is not normal

qqnorm(merged_df_new$NetPatRev)
qqline(merged_df_new$NetPatRev, lty=2, col='blue')
#The plot(in black) deviates from the normal distribution line(in blue), thereby we can tell that distribution is not normal

qqnorm(merged_df_new$InOperExp)
qqline(merged_df_new$InOperExp, lty=2, col='blue')
#The plot(in black) deviates from the normal distribution line(in blue), thereby we can tell that distribution is not normal

#Perform a Shapiro-Wilk test of those three variables; do results coincide with the QQ plots?
shapiro.test(merged_df_new$NoFTE)
#The p-value is significant (less than 5%) meaning distribution is not normal, thereby indicating same as QQ plot

shapiro.test(merged_df_new$NetPatRev)
#The p-value is significant (less than 5%) meaning distribution is not normal, thereby indicating same as QQ plot

shapiro.test(merged_df_new$InOperExp)
#The p-value is significant (less than 5%) meaning distribution is not normal, thereby indicating same as QQ plot

