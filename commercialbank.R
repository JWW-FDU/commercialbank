# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# set the working directory
setwd("E:/SVM")

# install and load packages
libraries = c("caret","LogicReg","e1071","pROC","MASS","ROCit")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Read in data
data = read.csv("E:/大四上/海外学者专题/Smart Data Analytics/project/bankdata.csv")
data = data[-1]
data$roe = factor(data$roe)
data$type1 = factor(data$type1)
data$type2 = factor(data$type2)
data$type3 = factor(data$type3)
data$type4 = factor(data$type4)
data$type5 = factor(data$type5)

# Split into training and test data
set.seed(1234)
ind = createDataPartition(y=data$roe, p=0.75, list=F)
train = data[ind,]
test = data[-ind,]

# Step AIC
fit1 = glm(roe ~ ., data=train,family=binomial())
summary(fit1)
step = stepAIC(fit1, direction  = "both")
step$anova

# SVM
ctrl = trainControl(classProbs = T,method = "repeatedcv", number = 10, repeats = 1)
fit_svm_prob = train(roe~ratio1 + ratio2 + ratio3 + ratio5 + ratio6,data=train,method="svmRadial",trConrol=ctrl,prob.model=TRUE)
fit_svm_class = train(roe~ratio1 + ratio2 + ratio3 + ratio5 + ratio6,data=train,method="svmPoly",trConrol=ctrl)
class_svm = predict(fit_svm_class,newdata=test)
pre_svm = predict(fit_svm_prob,newdata=test,type="prob")

# Logistic regression
fit_log = glm(roe~ratio1 + ratio2 + ratio3 + ratio5 + ratio6,family="binomial"(),data=train)
pre_log = as.numeric(predict(fit_log,newdata=test,type="response"))
class_log = factor(ifelse(pre_log>0.5,1,0))

#Probit regression
fit_pro = glm(roe~ratio1 + ratio2 + ratio3 + ratio5 + ratio6,family="binomial"(link = "probit"),data=train)
pre_pro = as.numeric(predict(fit_pro,newdata=test,type="response"))
class_pro = factor(ifelse(pre_pro>0.5,1,0))

# Confusion matrix
confusionMatrix(class_svm,test$roe)
confusionMatrix(class_log,test$roe)
confusionMatrix(class_pro,test$roe)

# ROC curve
jpeg("svm.jpg",width=600,height=600)
plot(rocit(pre_svm[,2],as.numeric(test$roe)),YIndex=F)
dev.off()
jpeg("logit.jpg",width=600,height=600)
plot(rocit(pre_log,as.numeric(test$roe)),YIndex=F)
dev.off()
jpeg("probit.jpg",width=600,height=600)
plot(rocit(pre_pro,as.numeric(test$roe)),YIndex=F)
dev.off()

# AUC
rocit(pre_svm[,2],as.numeric(test$roe))$AUC
rocit(pre_log,as.numeric(test$roe))$AUC
rocit(pre_pro,as.numeric(test$roe))$AUC

# Brier Score
mean((pre_svm[,2]-(as.numeric(test$roe)-1))^2)
mean((pre_log-(as.numeric(test$roe)-1))^2)
mean((pre_pro-(as.numeric(test$roe)-1))^2)
