getwd()
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
#Read Data
data <- read.table("ESR.txt", header=TRUE)
View(data)

model <- glm(y~fib,data=data,family = binomial(link="probit"))
summary(model)

plot(data$fib,data$y,pch=20,xlab="Fib",ylab="y")
lines(sort(data$fib),model$fitted.values[order(data$fib)],lty=1,lwd=2)

model1 <- glm(y~fib,data=data,family = binomial(link="logit"))
summary(model1)

lines(sort(data$fib),model1$fitted.values[order(data$fib)],lty=2,lwd=2)
legend("bottomright",inset=0.5,legend=c("Probit","Logit"),lty=1:2,lwd=2,bty="n")
