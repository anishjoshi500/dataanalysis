library(alr3)
data(challeng)
#challeng from alr3
data1 <- data.frame(challeng)

data1$propfail <- data1$Fail/data1$n
model2 <- glm(propfail~Temp,data=data1,family = binomial(link="logit"))
summary(model2)

y = (exp(5.0850-0.1156*31)/(1+exp(5.0850-0.1156*31)))
x = (log(0.5/(1-0.5))-5.0850)/-0.1156

