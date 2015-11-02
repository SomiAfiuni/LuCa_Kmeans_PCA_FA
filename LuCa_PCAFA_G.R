#title: "PCA and FA on *Lung Cancer* dataset"
#author: "Somi Afiuni"
### [http://archive.ics.uci.edu/ml/]
  
install.packages("psych")
require(psych)
require(stats)
require(graphics)
require(ggplot2)

## Full Dataset
data<- read.csv('~/Desktop/Somi/Informatics/Codes/UCLA_LuCa/ucla_lnca.csv')
#head(data)
names(data)

## Building sub dataset Age, lungcapacity, tumorsize, BMI
data1 <- data[, c(10, 9, 1, 19)]
names(data1)
str(data1)
head(data1)

## Scaling 
scaled.data1 <- scale(data1, center = TRUE, scale = TRUE)

## PCA
pcadata1 <- prcomp(scaled.data1)
print(pcadata1)
plot(pcadata1, type = "l", pch = 16, lwd = 2, main="PCA chart")
summary(pcadata1)
pcadata1$rotation[,1:4]
corpcadata1 <- round(cor(scaled.data1), digits = 3) ## Corrolation matrix

## FA
factor.data1 <- data1
names(factor.data1)
eigen.values <- eigen(cor(factor.data1)) ## extract eigenvalues from cor()
plot(eigen.values$values, type="l", ylab = 'Unrotated Eigenvalues', xlab = 'Components', lwd = 3)
eigen.values$values/(sum(eigen.values$values)) ## compute proportion of total variance
sum((eigen.values$values/(sum(eigen.values$values)))[c(1, 2, 3)]) #variance of 3 first factors
corefadata1 <- round(cor(factor.data1), digits = 3)
efa1data1 <- fa(r = corefadata1, nfactors = 2, rotate = "varimax", fm = 'pa')
efa1data1 
pchisq(.39, 4, ncp = 0, lower.tail = FALSE, log.p = FALSE)
fa.diagram(efa1data1)
fa.parallel(factor.data1, n.iter = 100)

#Create means for Component/factor 1 (Age and BMI)
data1$Age <- rowMeans(data1(c(1,4)))
#Create means for Component/factor 2 (tomorsize and lungcapacity)
data$tumorsize <- rowMeans(data1(c(2, 3)))
#Fit the model tumorsize from BMI and lungcapacity
fit1 <- lm(tumorsize ~ BMI + lungcapacity, data = data1) 
summary(fit1)
#Fit the model Age from BMI
fit2 <- lm(Age ~ BMI, data = data1) 
summary(fit2)