getwd()
require(ggplot2)
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
#Read Data
data <- read.table("Ch6GroceryData.txt", header=TRUE)
View(data)
data$lc<-(data$labor-mean(data$labor))/sd(data$labor)
data$cc<-(data$cases-mean(data$cases))/sd(data$cases)
model<-lm(data$lc~data$cc+ data$cc*data$cc+factor(data$holiday)+data$cc* factor(data$holiday) + data$cc*data$cc*factor(data$holiday))
summary(model)
model1<-lm(data$lc~data$cc+factor(data$holiday))
summary(model1)
anova(model, model1)
plotter<- ggplot(data, aes(x=data$cases, y=data$labor))+ geom_point(aes(colour=factor(data$holiday), shape=factor(data$holiday)))+ geom_abline(intercept = model1$coefficients[1]+model1$coefficients[3], slope = model1$coefficients[2], color="blue", size=1)
plotter1<- plotter + geom_abline(intercept = model1$coefficients[1], slope = model1$coefficients[2], color="red", size=1)
print(plotter1)