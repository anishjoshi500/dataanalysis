#Read Data
library(XLConnect)
Data <- readWorksheet(loadWorkbook("C:/Users/anishjoshi500/Downloads/Data Analysis Homework/RealEstateData.xlsx"),sheet=1)
View(Data)
Data$Quality<-as.factor(Data$Quality)
Data$Style<- as.factor(Data$Style)
Data$Pool<-as.factor(Data$Pool)
Data$AC<-as.factor(Data$AC)
Data$Hwy<-as.factor(Data$Hwy)
#Form training and testing datasets
set.seed(100)
traindata<- Data[sample(1:nrow(Data), 300, replace=FALSE),]
testdata<-Data[ !(Data$ID %in% traindata$ID), ]
#fitting a regression model
traindata$ID<-NULL
testdata$ID<-NULL
model <- lm(Price ~ ., data=traindata) 
summary(model)

#Residual Plot and QQ Plot
par(mfrow=c(1,2))
plot(model$fit, model$residuals, main="Residual vs. Fitted Plot")
abline(h=0)
qqnorm(model$residuals)
qqline(model$residuals)

#Log transformation on Y and fitting the model
model1 <- lm(log(Price) ~ ., data=traindata)
summary(model1)
#Residual and QQ plot for new Model with transformed Y
par(mfrow=c(1,2))
plot(model1$fitted.values,model1$residuals,xlab="Fitted values",ylab = "Residual values",pch=20)
abline(h=0)
qqnorm(model1$residuals,pch=20)
qqline(model1$residuals)


#Best subset model selection
library(faraway)
library(leaps)
x.mat<-model.matrix(model1)[, -1]
y<-traindata$Price
checks<-leaps(x.mat, y, method="adjr2")
maxadjr(checks, best=30)
checks2<-leaps(x.mat, y, method="Cp")
Cpplot(checks2)
#from the above plot and maxadjr we get 1,7,8,9,10,11,15,16,18,19 model as the best subset model with highest R of 0.842


# forward, backward, and stepwise selection method
null<- lm(log(Price) ~ 1, traindata)
full<- model1
stepAIC(null, scope = list(lower= null, upper= full),  direction="forward", trace= F)
#SqFt, Quality2, Quality3, Year, LotSize, Style2, Style3, Style4, Style5, Style6, Style7, Style9, Style10, AC1, Bath
stepAIC(full, scope = list(lower= null, upper= full),  direction="backward", trace= F)
#SqFt, Quality2, Quality3, Year, LotSize, Style2, Style3, Style4, Style5, Style6, Style7, Style9, Style10, AC1, Bath
stepAIC(full, scope = list(lower= null, upper= full),  direction="both", trace= F)
#SqFt, Quality2, Quality3, Year, LotSize, Style2, Style3, Style4, Style5, Style6, Style7, Style9, Style10, AC1, Bath

# model keeps selecting the same SqFt, Quality2, Quality3, Year, LotSize, Style2, Style3, Style4, Style5, Style6, Style7, Style9, Style10, AC1, Bath as predictors

# Check best model for differrent model sizes
subsets <- regsubsets(log(Price) ~ ., traindata)
summary(subsets)
summary(subsets)$bic
# looking at the above output we can see that the lowest BIC values are -504.9942 -504.8697 -503.2514

mod1<-lm(log(Price)~SqFt+Year+I(Quality==2)+I(Quality==3)+I(Style==7)+LotSize, data=traindata)
summary(mod1)
mod2<-lm(log(Price)~SqFt+Year+I(Quality==2)+I(Quality==3)+I(Style==6)+I(Style==7)+LotSize, data=traindata)
summary(mod2)
mod3<-lm(log(Price)~SqFt+Year+I(Quality==2)+I(Quality==3)+I(Style==2)+I(Style==6)+I(Style==7)+LotSize, data=traindata)
summary(mod3)

press.p<- function(model){
x.mat<-model.matrix(model)
lev<-hat(x.mat)
d=model$res/(1-lev)
return(sum(d^2))}
press.stats<- c(press.p(mod1), press.p(mod2), press.p(mod3))
mod.ss<- c(tail(anova(mod1)$"Sum Sq",1), tail(anova(mod2)$"Sum Sq",1), tail(anova(mod2)$"Sum Sq",1))
#Both press.stats and mod.ss have similar values

#Fitting the above models for the test dataset
mod11<-lm(log(Price) ~ SqFt+Year+I(Quality==2)+I(Quality==3)+I(Style==7)+LotSize, testdata)
summary(mod11)
mod22<-lm(log(Price)~SqFt+Year+I(Quality==2)+I(Quality==3)+I(Style==6)+I(Style==7)+LotSize, testdata)
summary(mod22)
mod33<-lm(log(Price)~SqFt+Year+I(Quality==2)+I(Quality==3)+I(Style==2)+I(Style==6)+I(Style==7)+LotSize, testdata)
summary(mod33)
#All the three models appear to have similar coefficients compared to the three models fitted on the training dataset

#Prediction for test dataset using the three models fitted on the training dataset
predict1<-predict.lm(mod1, testdata)
predict2<-predict.lm(mod2, testdata)
predict3<-predict.lm(mod3, testdata)
#Comparing Mean square prediction error and MSE
yp<-log(testdata$Price)
sum((yp-predict1)^2)/length(yp)
sum((yp-predict2)^2)/length(yp)
sum((yp-predict3)^2)/length(yp)
c(tail(anova(mod1)$"Mean Sq",1), tail(anova(mod2)$"Mean Sq",1), tail(anova(mod3)$"Mean Sq",1))
#mod3's MSPR is the highest and farthest from the original MSE thus we neglect that model
#mod1 and mod2 have similar MSPR's and as mod1 has fewer predictors we select that model moving forward

#Residual Plot for mod1 which is the selected best predictor subset model
par(mfrow=c(1,2))
plot(mod1$fit, mod1$residuals, main="Residual vs. Fitted Plot")
abline(h=0)
qqnorm(mod1$residuals)
qqline(mod1$residuals)


#Finding outlier with maximum studentized ratio
ex.std.res <- rstudent(mod1)
ti.max <- ex.std.res[abs(ex.std.res)==max(abs(ex.std.res))]

p.val <- 2*(1-pt(abs(ti.max),df=300-5-1))
p.val
p.val<0.05/300
#104 is the outlier
x.mat <- model.matrix(mod1)
lev <- hat(x.mat)
plot(lev,ylab = "Leverages",main="Index plot of leverages",pch=20)
abline(h=(2*3)/300)
identify(1:300,lev,1:300)

#cook's distance
cook<-cooks.distance(mod1)
cook
plot(cook, xlab="Cases", ylab="Cook's distance", pch=20)
segments(1:300, 0, 1:300, cook)
identify(1:300, cook, 1:300)
#from above plot we can see that 17, 14, 72 and 220 are the influential outliers

#neglecting all the qualitative predictors we get the following correlation matrix
cor(Data[c(2,3,4,5,7,9,12)])
