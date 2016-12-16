getwd()
setwd("C:/Users/anishjoshi500/Downloads/Data Analysis Homework")
data <- read.table("PatientSatisfaction.txt", header = TRUE)
names(data)
#Scatterplot Matrix
pairs(data)
#Coorelation Matrix
cor(data)
#First Order Regression Model for three Predictors
model<- lm(Satis~ Age+Severity+Anxiety, data)
summary(model)
#90% interval estimate of the mean satisfaction for a 35-year-old patient with severity
#index 45 and anxiety index 2.2
data_new = data.frame(Age = 35, Severity = 45, Anxiety = 2.2)
predict(model, data_new, interval="confidence", level=0.90)
#ANOVA table
model1<- lm(Satis~ Severity+Age+Anxiety, data)
summary(model1)
anova(model1)
#Testing whether x2 and x3 can be dropped from the regression model given that x1 is retained
model2<-lm(Satis ~ Age, data)
summary(model2)
anova(model2,model1)
#Testing whether β1 = −1.0 and β2 = 0
model3<- lm(Satis+Age ~ Anxiety, data)
summary(model3)
anova(model3)
#first-order linear regression model for relating patient satisfaction to patient’s age and severity
#of illness.
model4<- lm(Satis~ Age+Severity, data)
summary(model4)
#SSR(x1)
model5<- lm(Satis ~ Age, data)
anova(model5)
#SSR(x1|x3)
model6<- lm(Satis ~ Anxiety+Age, data)
anova(model6)
#SSR(x2)
model7<- lm(Satis ~ Severity, data)
anova(model7)
#SSR(x2|x3)
model8<- lm(Satis ~ Anxiety+Severity, data)
anova(model8)
#Standardizing the preditors
mf<-(1/sqrt(46-1))
Age<-((data$Age-mean(data$Age))/sd(data$Age))*mf
Severity<-((data$Severity-mean(data$Severity))/sd(data$Severity))*mf
Anxiety<-((data$Anxiety-mean(data$Anxiety))/sd(data$Anxiety))*mf
#Standardizing the response
Satisfaction<-((data$Satis-mean(data$Satis))/sd(data$Satis))*mf
#Model Fit
model9<-lm(Satisfaction ~ Age+Severity+Anxiety)
summary(model9)
Sdata<-data.frame(Age, Severity, Anxiety)
#Correlation
scorr<-cor(Sdata)
#Standard Deviations for predictors
sdAge<-sd(data$Age)
sdSeverity<-sd(data$Severity)
sdAnxiety<-sd(data$Anxiety)
#Standard Deviation for response
sdSatisfaction<-sd(data$Satis)
# Transformations
bAge<-model9$coefficients[2]*(sdSatisfaction/sdAge)
bSeverity<-model9$coefficients[3]*(sdSatisfaction/sdSeverity)
bAnxiety<-model9$coefficients[4]*(sdSatisfaction/sdAnxiety)
