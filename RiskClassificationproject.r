rm(list=ls())
getwd()
setwd("/Users/shubhangijain")
#setwd("C:/Users/ukm/Documents/Ujjal teaching/Supply Chain Analytics/Bankruptsy Risk Assessment")

d=read.csv("Supplier_Bankruptcy_Risk.csv", header=TRUE);
View(d)

install.packages("pROC")
library(pROC)
install.packages("e1071")
install.packages("randomForest")
library(randomForest)
library("e1071")

colnames(d)

d1=d[,-1]

n=dim(d1)[1];
ind=sample.int(n,size=floor(0.70*n))

train=d1[ind,];
test=d1[-ind,];

#GLM model for step 1
m1=glm(Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers, data=train, family="binomial"); 
summary(m1)
p=predict(m1, newdata=test, type="response")
p
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#RF for step 1
F1= as.formula ("Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers")
r1<- randomForest(F1,data=train,ntree=500)
p<-predict(r1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#SVM for step 1
s1<- svm (F1, data=train, kernel="radial")
s1
p<-predict(s1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#GLM model for step 2
m2=glm(Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers + NumberofProducts + NumberofNewProducts + TechnologyInvestment, data=train, family="binomial"); 
summary(m2)
p=predict(m2, newdata=test, type="response")
p
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#RF for step 2
F2= as.formula ("Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers + NumberofProducts + NumberofNewProducts + TechnologyInvestment")
r1<- randomForest (F2,data=train,ntree=500)
r1
p<-predict(r1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#SVM for step 2
s1<- svm (F2, data=train, kernel="radial")
s1
p<-predict(s1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)


#GLM model for step 3
m3=glm(Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers + NumberofProducts + NumberofNewProducts + TechnologyInvestment + CAGRBusiness + ProfitabilityLast5Years, data=train, family="binomial"); 
summary(m3)
p=predict(m3, newdata=test, type="response")
p
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#RF for step 3
F3= as.formula ("Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers + NumberofProducts + NumberofNewProducts + TechnologyInvestment +CAGRBusiness + ProfitabilityLast5Years")
r1<- randomForest (F3,data=train,ntree=500)
r1
p<-predict(r1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#SVM for step 3
s1<- svm (F3, data=train, kernel="radial")
s1
p<-predict(s1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)


#GLM model for step 4
m4=glm(Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers + NumberofProducts + NumberofNewProducts + TechnologyInvestment + CAGRBusiness + ProfitabilityLast5Years + CountryGDP + CountryGDPGrowth + IndexOfPoliticalTurmoil + IndexOfSocialTurmoil, data=train, family="binomial"); 
summary(m4)
p=predict(m4, newdata=test, type="response")
p
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#Find Optimal Threshold Value
dist = c()
for(i in 1:length(roc$thresholds)){
  m <- sqrt((1-roc$sensitivities[i])^2+(1-roc$specificities[i])^2)
  dist <- c(dist,m)
}

ind <- which(dist == min(dist))
opt_theta <- roc$thresholds[ind]
opt_theta

roc$sensitivities[ind]
roc$specificities[ind]

#RF for step 4
F4= as.formula ("Bankruptcy~NumberOfCustomers + AverageShareofBusiness + AverageSizeofCustomers + AverageSizeofSuppliers + NumberofProducts + NumberofNewProducts + TechnologyInvestment +CAGRBusiness + ProfitabilityLast5Years +CountryGDP + CountryGDPGrowth + IndexOfPoliticalTurmoil+ IndexOfSocialTurmoil")
r1<- randomForest (F4,data=train,ntree=500)
r1
p<-predict(r1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#Find Optimal Threshold Value
dist = c()
for(i in 1:length(roc$thresholds)){
  m <- sqrt((1-roc$sensitivities[i])^2+(1-roc$specificities[i])^2)
  dist <- c(dist,m)
}

ind <- which(dist == min(dist))
opt_theta <- roc$thresholds[ind]
opt_theta

roc$sensitivities[ind]
roc$specificities[ind]


#SVM for step 4
s1<- svm (F4, data=train, kernel="radial")
s1
p<-predict(s1,newdata=test, type="response")
roc=roc(test$Bankruptcy, p)
plot(roc)
auc=auc(roc)
print(auc)

#Find Optimal Threshold Value
dist = c()
for(i in 1:length(roc$thresholds)){
  m <- sqrt((1-roc$sensitivities[i])^2+(1-roc$specificities[i])^2)
  dist <- c(dist,m)
}

ind <- which(dist == min(dist))
opt_theta <- roc$thresholds[ind]
opt_theta

roc$sensitivities[ind]
roc$specificities[ind]






