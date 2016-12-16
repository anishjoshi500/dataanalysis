getwd()
require(ggplot2)
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
#Read Data
data <- read.table("Ch6GroceryData.txt", header=TRUE)
View(data)
#fit model and calculate studentized deleted residuals
model<-lm(data$labor~data$cases + data$costs + factor(data$holiday))
summary(model)
ex.std.res <- rstudent(model)
ti.max <- ex.std.res[abs(ex.std.res)==max(abs(ex.std.res))]
#to test whether observation 40 is an outlier or not we use the bonferroni correction 
p.val <- 2*(1-pt(abs(ti.max),df=52-3-1))
p.val<0.05/52
p.val
x.mat <- model.matrix(model)
lev <- hat(x.mat)
plot(lev,ylab = "Leverages",main="Index plot of leverages",pch=20)
abline(h=(2*3)/52)
identify(1:52,lev,1:52)
nd<-data.frame(labor = 300000, costs=7.2, holiday=0)
plot(data$cases, data$costs, xlab="Cases", ylab="Costs")
points(nd, pch=20)
hm<-t(matrix(c(1,300000,7.2,0), nrow=4, ncol=1))%*%solve(t(x.mat)%*%x.mat)%*%matrix(c(1,300000,7.2,0), nrow=4, ncol=1)
model1<-lm(data$labor~data$cases+data$costs, data=data)
cook<-cooks.distance(model1)
plot(cook, xlab="Cases", ylab="Cook's distance", pch=20)
segments(1:52, 0, 1:52, cook)
identify(1:52, cook, 1:52)
cook[c(16, 22, 43, 48, 10, 32, 38, 40)]
