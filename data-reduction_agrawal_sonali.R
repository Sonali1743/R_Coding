#PCA and Factor Analysis with R (6 pts.)
ect = read.csv('data/ect_data.txt',header = T, sep = '')

#Perform PCA; how many components have eigenvalues greater than 1.0? (1 pt.)
ect.pca = princomp(ect[c('attitude1_01',	'attitude1_02',	'attitude1_03',	'attitude1_04',	'intent1_01',	'intent1_02',
                         'intent1_03',	'intent1_04',	'peruse01',	'peruse02',	'peruse03',	'peruse04',	'satis01',	'satis02',
                         'satis03',	'satis04')])
ect.pca$sdev^2
#4 components have a value greater than 1.0

#Create a scree plot and compare it to your PCA results (1 pt.)
plot(ect.pca)
#It looks like from the plot that we are dealing with first 4 components

#Conduct a factor analysis and look at the loadings (with 4 factors)
ect.fa = factanal(~attitude1_01+attitude1_02+attitude1_03+attitude1_04+intent1_01+intent1_02+
                    intent1_03+intent1_04+peruse01+peruse02+peruse03+peruse04+satis01+
                    satis02+satis03+satis04, factors = 4, data = ect, rotation = 'varimax', scores = 'none')
ect.fa

#Conducting factor analysis with 3 factors
ect.fa1 = factanal(~attitude1_01+attitude1_02+attitude1_03+attitude1_04+intent1_01+intent1_02+
                    intent1_03+intent1_04+peruse01+peruse02+peruse03+peruse04+satis01+
                    satis02+satis03+satis04, factors = 3, data = ect, rotation = 'varimax', scores = 'none')
ect.fa1

#Conducting factor analysis with 5 factors
ect.fa2 = factanal(~attitude1_01+attitude1_02+attitude1_03+attitude1_04+intent1_01+intent1_02+
                     intent1_03+intent1_04+peruse01+peruse02+peruse03+peruse04+satis01+
                     satis02+satis03+satis04, factors = 5, data = ect, rotation = 'varimax', scores = 'none')
ect.fa2

#How do the results compare against the PCA? (1 pt.)
#The FA results confirms the PCA results. The factor analysis with 4 factors is producing the best results;
#majority of the items are being accounted for in the generated 4 factors; thereby 16 variables can be reduced to 4 factors

#Which items belong to which factors? (1 pt.)
#satis01, satis02, satis03 and satis04 belong to Factor1 (contributing >0.7),
#peruse01, peruse02, peruse03 and peruse04 belong to Factor2 (contributing >0.7), 
#intent1_01, intent1_02 and intent1_03 belong to Factor3 (contributing >0.7),
#and attitude1_03 and attitude1_04 belong to Factor4 (contributing >0.7)

#Items that do not load cleanly onto a single factor (1 pt.)
#attitude1_01 and attitude1_02 do not load cleanly onto a single factor, 
#intent1_04 also does not load cleanly onto a single factor although contributing ~0.6 to Factor3 (<0.7)

#Average your measures or select a representative variable for each factor? Why? (1 pt.)
#Based on my analysis, I would select a representative variable for each factor that is satis02 for Factor1,
#peruse04 for Factor2, intent1_02 for Factor3 and attitude1_03 for Factor4. This is because these variables
#are contributing the most to the factors. To use average of measures, we would have to further process the data by:
#1. Removing the items contributing <0.7 and again conducting the factor analysis (until all Intention items are greater than 0.7)
#2. Normalize variables before averaging their scores so that they all have same scale

#Cluster Analysis with R (4 pts.)
#Perform a k-means analysis twice; choose 2 different values of k between 4 and 10
cartest = read.csv('data/car.test.frame.txt',header = T, sep = '')
#Removing rows with missing information
cartest = na.omit(cartest)

#k=6
cartest_kmeans1 = kmeans(data.frame(cartest$Price,cartest$Reliability,cartest$Mileage,cartest$Weight,
                                    cartest$Disp.,cartest$HP), 6)
plot(cartest$Price,cartest$Weight, col = cartest_kmeans1[[1]])
table(cartest$Type,cartest_kmeans1[[1]])

#k=8
cartest_kmeans2 = kmeans(data.frame(cartest$Price,cartest$Reliability,cartest$Mileage,cartest$Weight,
                                    cartest$Disp.,cartest$HP), 8)
plot(cartest$Price,cartest$Weight, col = cartest_kmeans2[[1]])
table(cartest$Type,cartest_kmeans2[[1]])

#Which of the two does it appear to fit with and why? (2 pts.)
#The k-means analysis appears to fit better with 6 groups than 8 groups. In the plots, the data points are segregated 
#well into distinct groups for k=6, whereas for k=8 there is quite an overlap of data points from different groups. Also, looking at the
#classification tables, the table for k=6 seems to be doing a better job at making clusters more homogeneous than k=8

#Perform an agglomerative analysis
cartest_hclust = hclust(dist(data.frame(cartest$Price,cartest$Reliability,cartest$Mileage,cartest$Weight,
                                        cartest$Disp.,cartest$HP)))
clusters = cutree(cartest_hclust,6)
plot(hclust(dist(data.frame(cartest$Price,cartest$Reliability,cartest$Mileage,cartest$Weight,
                            cartest$Disp.,cartest$HP))), main='Cars Cluster Analysis')
table(cartest$Type,clusters)

#How do the results compare to the k-means? (1 pt.)
#For k-means, cluster4 and cluster6 are perfectly homogeneous and rest other clusters have overlapping data points; 
#whereas with hcluster analysis, cluster2 and cluster6 are perfectly homogeneous and rest other clusters have overlapping data points;

#Which of the two performs better and why? (1 pt.)
#K-means seems to be performing a little better than h-cluster since the clusters are 
#more uniform with the original categorization of the cars

