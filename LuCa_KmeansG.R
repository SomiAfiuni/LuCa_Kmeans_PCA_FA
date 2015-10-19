
#title: "K-means on *Lung Cancer* dataset"
#author: "Somi Afiuni"
#date: "October 13, 2015"
#output: html_document
### [http://archive.ics.uci.edu/ml/]
  
install.packages("psych")
require(psych)
require(stats)
require(graphics)
require(ggplot2)

## Full Dataset
data<- read.csv('ucla_lnca.csv')
head(data)
names(data)

## Building sub dataset Age, lungcapacity, tumorsize, BMI
data1 <- data[, c(10, 9, 1, 19)]
names(data1)
str(data1)
head(data1)

## Scaling 
scaled.data1 <- scale(data1, center = TRUE, scale = TRUE)

## Kmeans Clustering
kdata1 <- kmeans(scaled.data1, 3)
kdata1$size  #size of each cluster
plot(data1[c("Age", "lungcapacity", "tumorsize", "BMI")], col = kdata1$cluster)  

