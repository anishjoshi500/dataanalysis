getwd()
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
data <- read.table("SteroidLevels.txt", header = TRUE)
head(data)
names(data)
View(data)
#Centering Predictor
cp <- data$Age - mean(data$Age)
#fitted regression model
model<- lm(Level ~ cp + I(cp^2), data)
summary(model)
#Plot for regression function and the data
plot(data$Level~cp, xlab="Age", ylab="Steroid Levels")
lines(sort(cp), fitted(model)[order(cp)], col='red')
#steroid levels of females aged 15 using a 99% prediction interval
data_new = data.frame(cp = -0.77778, cp1= 0.6049383)
predict(model, data_new, interval="prediction", level=0.99)
#Testing whether the quadratic term can be dropped
model1<- lm(Level ~ cp, data)
summary(model1)
anova(model, model1)
