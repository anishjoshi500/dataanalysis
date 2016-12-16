getwd()
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
bodyfat_data <- read.table("bodyfat.txt")
head(bodyfat_data)
names(bodyfat_data)
View(bodyfat_data)
# V1 - triceps skinfold thickness V2 - thigh circumference V3 - midarm circumference V4 - body fat
model <- lm(V4 ~ V1, bodyfat_data)
summary(model)
bodyfat_newdata=data.frame(V1=25)
predict.lm(model, bodyfat_newdata, interval="confidence", level= 0.95, type="response")
#Updated regression model with two predictors
model1 <- lm(V4 ~ V1+V2, bodyfat_data)
summary(model1)
#We find values for thigh circumference corresponding to the tricep skinfold thickness values between #range 24 to 26.
val1=53.5  
val2=51  
val3=49.8  
val4=53.9
#Prediction interval for the above values
x= list(V1=25, V2=val1)
predict.lm(model1, x, interval = "prediction") 
x1= list(V1=25, V2=val2)
predict.lm(model1, x1, interval = "prediction") 
x2= list(V1=25, V2=val3)
predict.lm(model1, x2, interval = "prediction")  
x3= list(V1=25, V2=val4)
predict.lm(model1, x3, interval = "prediction")  
