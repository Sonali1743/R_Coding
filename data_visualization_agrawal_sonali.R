library('tidyverse')
library('car')
library('dplyr')

#Reading the data file
dataset = read.table('~//Documents//GitHub//fundamentals-of-data-visualization-tha-Sonali1743//data//taxonomy.txt', header = TRUE)

#1. Plot Petals, Internode, and Sepal against Fruit
plot(dataset$Fruit, dataset$Petals)
#Ans: There is no relationship between Fruit and Petals.

plot(dataset$Fruit, dataset$Internode)
#Ans: There is no relationship between Fruit and Internode.

plot(dataset$Fruit, dataset$Sepal)
#Ans: There is no relationship between Fruit and Sepal.

#2. Generate histograms for Petals, Internode, Sepal, and Fruit
hist(dataset$Petals)
#Ans: The distribution is skewed to the right

hist(dataset$Internode)
#Ans: The distribution appears to be uniform (mildly skewed to the right)

hist(dataset$Sepal)
#Ans: It is a U-shaped distribution with maximum frequencies at the extremes of the data range

hist(dataset$Fruit)
#Ans: The distribution appears to be uniform (mildly skewed to the right)

#3. Bin the values of Bract
range(dataset$Bract)
min_bract = min(dataset$Bract)
max_bract = max(dataset$Bract)
(range_bract = max_bract - min_bract)
bins = seq(min_bract, max_bract, range_bract/5)
bins[6] = 20
bins

Bract_Category = cut(dataset$Bract, bins, right = FALSE, labels = c('BractI','BractII','BractIII','BractIV','BractV'))

#4. Add the new column of data to the existing dataframe. Generate a boxplot with Fruit and Bract_Cat
dataset$Bract_Cat = Bract_Category
dataset$Bract_Cat = as.factor(dataset$Bract_Cat)
plot(dataset$Bract_Cat, dataset$Fruit,horizontal = TRUE, las=2)

#Ans: BractI and BractII have larger variance and standard deviations compared to other categories (since the boxes are comparatively larger than other boxes)
# In terms of skewness, BractI, BractIII, BractIV and BractV are right skewed (medians are closer to lower edge)
# For BractIII and BractV boxes are also closer to lower whiskers indicating right skewness

#5.Generate a bar chart for Taxon and Fruit
#Taxon
dataset1 = dataset[c(41,4,93,84),]
dataset2 = dataset1[order(dataset1$Taxon),]
barplot(dataset2$Fruit, names.arg = dataset2$Taxon)
#Bract_Cat
dataset3 = dataset[c(86,101,118,3,37),]
dataset4 = dataset3[order(dataset3$Bract_Cat),]
barplot(dataset4$Fruit, names.arg = dataset4$Bract_Cat, las=2)


