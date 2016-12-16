getwd()
require(ggplot2)
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
#Read Data
data <- read.table("GPAData.txt", header=TRUE)
data1<- read.table("GPAMajorData.txt", header = TRUE)
View(data)
View(data1)
#Combine the two files
data$Major<-data1$Major
#Scatterplot with different symbols for major=0 and major=1
plot<- ggplot(data, aes(x=data$ACT, y=data$GPA))+ geom_point(aes(colour=factor(data$Major), shape=factor(data$Major)))
print(plot)
#fitted simple linear regression model
model <- lm(data$GPA ~ data$ACT, data)
summary(model)
#best fitted line
plot(data$ACT, data$GPA)
abline(model)
#Residual v/s fitted value plots for Major=0 and Major=1
x<-which(data$Major==1)
x1<-which(data$Major==0)
par(mfrow=c(1,2))
plot(model$fit[x], model$residuals[x], main="residual vs. fitted Plot for Major=1")
abline(h=0)
plot(model$fit[x1], model$residuals[x1], main="residual vs. fitted Plot for Major=0")
abline(h=0)
#fitted simple linear regression model
model1 <- lm(data$GPA ~ data$ACT+data$Major, data)
summary(model1)
multiply<-data$ACT*data$Major
#data$Major<-as.factor(data$Major)
model2 <- lm(data$GPA ~ data$ACT+factor(data$Major)+multiply, data)
summary(model2)
plotter<- ggplot(data, aes(x=data$ACT, y=data$GPA))+ geom_point(aes(colour=factor(data$Major), shape=factor(data$Major)))+ geom_abline(intercept = model2$coefficients[1]+model2$coefficients[3], slope = model2$coefficients[2]+model2$coefficients[4], color="blue", size=1)
plotter1<- plotter + geom_abline(intercept = model2$coefficients[1], slope = model2$coefficients[2], color="red", size=1)
print(plotter1)
#Fitting separate regression models
par(mfrow=c(1,2))
model3<-lm(data$GPA[x] ~ data$ACT[x], data)
summary(model3)
plot(data$ACT, data$GPA)
abline(model3)
model4<-lm(data$GPA[x1] ~ data$ACT[x1], data)
summary(model4)
plot(data$ACT, data$GPA)
abline(model4)
