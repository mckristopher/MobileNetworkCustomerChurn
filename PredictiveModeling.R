# set working directory
setwd("/home/dell/mne/BABI/GreatLearning/PredictiveModelling/")
# install.packages("tidyverse")
library(readr)
library(tidyverse)

# importing data
calldata = readxl::read_excel("Cellphone.xlsx", sheet = 2)

# data description
names(calldata)
dim(calldata)

str(calldata)
# All variables are numeric; Churn is the dependant variable.

anyNA(calldata)
# no missing values

summary(calldata)
# Possible Outliers - AccountWeeks, DataUsage, CustServCalls, DayMins, DayCalls, MonthlyCharge, OverageFee, RoamMins

scaled.calldata = scale(calldata[,-c(1, 3, 4)], center = TRUE, scale = TRUE)
scaled.calldata = cbind(scaled.calldata, calldata[,c(1, 3, 4)])

attach(scaled.calldata)

# Numeric Variables
scaled.calldata %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

boxplot(scaled.calldata[, -c( 9, 10, 11)])

plot(Churn, MonthlyCharge)
plot(Churn, AccountWeeks)
plot(Churn, DayCalls)
plot(Churn, CustServCalls)
plot(Churn, OverageFee)
plot(Churn, RoamMins)

pairs(calldata)

plot(DataPlan, MonthlyCharge)
plot(DataUsage, MonthlyCharge)
plot(DayMins, MonthlyCharge)
ggplot(data = calldata) + geom_point( mapping = aes(x = DataUsage, y = MonthlyCharge,  color = DataPlan, alpha = DayMins ))


corrplot::corrplot(cor(scaled.calldata), method = "number")

glm(Churn~., data = scaled.calldata, family = binomial)
summary(glm(Churn~., data = scaled.calldata, family = binomial))

library(car)
vif(glm(Churn~., data = scaled.calldata, family = binomial))

### PCA

eigen = eigen(cor(scaled.calldata[,-9]))
eigen$values

Scree = data.frame( c(1:10), eigen$values)
plot(Scree)
lines(Scree)
# 8 components

library(psych)
Unrotate=principal(scaled.calldata[,-9], nfactors=8, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(scaled.calldata[,-9],nfactors=8,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)

head(Rotate$scores,10)

newcalldata = Rotate$scores
colnames(newcalldata) = c("MonthlyCharge", "DayMins", "OverageFee", "RoamMins", "ContractRenewal", 
                          "AccountWeeks", "CustServCalls", "DayCalls")
head(newcalldata, 2)
newcalldata = cbind(scaled.calldata[,9], newcalldata)
colnames(newcalldata)[1] = "Churn"

newcall = as.data.frame(newcalldata)
newcall$Churn = as.factor(newcall$Churn)
class(newcall$Churn)

glm(Churn~., data = newcall, family = binomial)
summary(glm(Churn~., data = newcall, family = binomial))
vif(glm(Churn~., data = newcall, family = binomial))

logistic.Status = glm(Churn~., data = newcall, family = binomial)
# logistic.Status$fitted.values

plot(newcall$Churn, logistic.Status$fitted.values)
newcall$Churn.predicted = ifelse(logistic.Status$fitted.values < 0.5, 0, 1)

table(newcall$Churn, newcall$Churn.predicted)
summary(newcall$Churn)

#sensitivity
2782/2850

#specificity
91/483

# install.packages("pROC")
library(pROC)
roc(newcall$Churn, logistic.Status$fitted.values)
plot.roc(newcall$Churn, logistic.Status$fitted.values)

#########

library(class)
set.seed(100)
index = sample(3333, 2833)
call.train = scaled.calldata[index, ]
call.test = scaled.calldata[-index, ]

knn.Churn = knn(call.train, call.test, call.train$Churn, k=11)
table(call.test$Churn, knn.Churn)
summary(as.factor(call.test$Churn))

roc(as.numeric(call.test$Churn), as.numeric(knn.Churn))

#########

# install.packages("e1071")
library(e1071)

naiveBayes(Churn~.,data=scaled.calldata)
baiyes.Churn=naiveBayes(Churn~.,data=scaled.calldata)
predict(baiyes.Churn,type="raw",newdata=scaled.calldata)
plot(scaled.calldata$Churn,predict(baiyes.Churn,type="raw",newdata=scaled.calldata)[,2])
roc(scaled.calldata$Churn,predict(baiyes.Churn,type="raw",newdata=scaled.calldata)[,2])

######