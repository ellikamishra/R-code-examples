getwd()
#load data set 

aird=read.csv2("Air.csv",header=FALSE,sep=",")
#get dimensions and print head values
dim(aird)
head(aird)
names <- c("date","time","co","pt08O","nmhc","c6h6","pt08N","no1","pt08NO","no2","pt08NO2","pt08O3","t","rh","ah")
colnames(aird) <- names          
names(aird)
#print mean values
mean(aird$co)
mean(aird$pt08O)
#print summmary
summary(aird)
str(aird)
#bar plot
barplot(table(aird$co), main="Plot", col="black")
is.na(aird$co)
is.na(aird$nmhc)

install.packages("corrplot")
library(corrplot)
a<-cor(aird[c("co","pt08O","nmhc","c6h6","pt08N","no1","pt08NO")])
corrplot(a,method='circle',type='upper')
#create subset
subset<-aird[c("co","pt08O","nmhc","c6h6","pt08N","no1","pt08NO")]
#install packages
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(subset, histogram=TRUE, pch=19)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = a, col = col, symm = TRUE)
#splititng data in test train
install.packages("caTools")
library(caTools)
aird[, c(1)] <- sapply(aird[, c(1)], as.numeric)
set.seed(123)
split = sample.split(aird$ah, SplitRatio = 2/3)
train_aird = subset(aird, split == TRUE)
test_aird = subset(aird, split == FALSE)
dim(train_aird)
dim(test_aird)
#linear regression plot
library(caTools)
regressor=lm(formula = ah~rh, data=train_aird)
plot(train_aird$rh,train_aird$ah,col = "blue",
     abline(regressor,cex = 1.3,pch = 16,xlab="rh",ylab="ah"))
ad_ah_predict=predict(regressor, newdata=test_aird)
round_ah=ad_ah_predict
rage=round(round_ah)

install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
#df=confusionMatrix(as.factor(rage),as.factor(test_aird$ah))


#normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
a1data<-aird
a1data_n <- as.data.frame(lapply(a1data[1:6], normalize))
