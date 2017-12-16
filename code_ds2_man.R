install.packages("corrplot")
install.packages("ggplot2")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("caTools")
install.packages("rattle")
install.packages("gridExtra")
install.packages("ROCR")
install.packages("randomForest")
install.packages("randomForestSRC")
install.packages("reshape2")
install.packages("RColorBrewer")
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caTools)
library(ROCR)
library(ipred)
library(gbm)
library(C50)


data<- read.csv("Employeeattrition.csv")
head(data)

str(data)
sum(is.na(data))
data$EmployeeNumber=data$Over18=data$EmployeeCount=data$StandardHours = NULL

p1= qplot(BusinessTravel,data = data,geom="auto")
p2 = qplot(Gender, data=data,geom="auto")
grid.arrange(p1,p2,nrow=1,ncol=2)

plottable1=table(data$Attrition,data$JobLevel)
plottable2=table(data$Attrition,data$Education)
plottable3=table(data$Attrition,data$EnvironmentSatisfaction)
plottable4=table(data$Attrition,data$JobInvolvement)
plottable5=table(data$Attrition,data$PercentSalaryHike)
plottable6=table(data$Attrition,data$PerformanceRating)
plottable7=table(data$Attrition,data$StockOptionLevel)
plottable8=table(data$Attrition,data$YearsAtCompany)
plottable9=table(data$Attrition,data$YearsInCurrentRole)

barplot(plottable1, main="Employees left vs Job Level", xlab="JobLevel",col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable2, main="Employees left vs Education", xlab="Education",col=c("Blue","Yellow"),legend=rownames(plottable2),beside = TRUE)
barplot(plottable3, main="Employees left vs Environment Satisfaction", xlab="JobLevel", col=c("Blue","Yellow"),beside = TRUE)
barplot(plottable4, main="Employees left vs Job Involvement", xlab="Job Involvement", col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable5, main="Employees left vs salary hike", xlab="salary hike in %", col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable6, main="Employees left vs Performance Rating", xlab="PerformanceRating",col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable7, main="Employees left vs stock option level", xlab="Stock Option Level", col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable8, main="Employees left vs Num of Years at Company", xlab="Num of Years", col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable9, main="Employees left vs Years in current Role", xlab="Years In Current Role ", col=c("Blue","Yellow"),legend=rownames(plottable1),beside = TRUE)

dummy = data
dummy$Attrition=as.numeric(dummy$Attrition)
dummy$BusinessTravel=as.numeric(dummy$BusinessTravel)
dummy$Department=as.numeric(dummy$Department)
dummy$EducationField=as.numeric(dummy$EducationField)
dummy$Gender=as.numeric(dummy$Gender)
dummy$JobRole=as.numeric(dummy$JobRole)
dummy$MaritalStatus=as.numeric(dummy$MaritalStatus)
dummy$OverTime=as.numeric(dummy$OverTime)

corTable=cor(dummy)
corr=melt(corTable)
corTable
corr

corrplot( cor(as.matrix(dummy), method = "pearson", use = "complete.obs") ,is.corr = FALSE, type = "lower", order = "hclust", tl.col = "black", tl.srt = 360)

data$MaritalStatus=data$MonthlyIncome=data$PerformanceRating= NULL

set.seed(3000)
split=sample.split(data$Attrition,SplitRatio = .7)
train=subset(data,split==T)
test=subset(data,split==F)

attLog=glm(Attrition~.,data=train,family = binomial)
predGlm=predict(attLog,type="response",newdata=test)
table(test$Attrition,predGlm>.5)


decisionTreeModel= rpart(Attrition~.,data=train,method="class",minbucket = 25)
fancyRpartPlot(decisionTreeModel)
predDT=predict(decisionTreeModel,newdata = test,type = "class")
table(test$Attrition,predDT)

randomForestModel=randomForest(Attrition~.,data=train,ntree=250,nodesize=12)
predictRF=predict(randomForestModel,newdata=test)
table(test$Attrition,predictRF)

glm_ROC=predict(attLog,test,type="response")
pred_glm=prediction(glm_ROC,test$Attrition)
perf_glm=performance(pred_glm,"tpr","fpr")

dt_ROC=predict(decisionTreeModel,test)
pred_dt=prediction(dt_ROC[,2],test$Attrition)
perf_dt=performance(pred_dt,"tpr","fpr")

RF_ROC=predict(randomForestModel,test,type="prob")
pred_RF=prediction(RF_ROC[,2],test$Attrition)
perf_RF=performance(pred_RF,"tpr","fpr")

auc_glm <- performance(pred_glm,"auc")
auc_glm <- round(as.numeric(auc_glm@y.values),3)
auc_dt <- performance(pred_dt,"auc")
auc_dt <- round(as.numeric(auc_dt@y.values),3)
auc_RF <- performance(pred_RF,"auc")
auc_RF <- round(as.numeric(auc_RF@y.values),3)
print(paste('AUC of Logistic Regression:',auc_glm))
print(paste('AUC of Decision Tree:',auc_dt))
print(paste('AUC of Random Forest:',auc_RF))

plot(perf_glm, main = "ROC curves for the models", col='blue')
plot(perf_dt,add=TRUE, col='red')
plot(perf_RF, add=TRUE, col='green3')
legend('bottom', c("Logistic Regression", "Decision Tree", "Random Forest"), fill = c('blue','red','green3'), bty='n')

baggingmodel = bagging(Attrition~.,
                       data=train,control=rpart.control(cp=.00001))
predprobs = predict(baggingmodel,newdata = test,type="prob")
testwithprobs = cbind(test,predprobs[,2])
print(testwithprobs)
print(baggingmodel)

# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(Attrition~., data=test, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
fit.gbm <- train(Attrition~., data=test, method="gbm", metric=metric, trControl=control, verbose=FALSE)
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)
gbm_dev <- predict(fit.gbm, train,type= "prob")[,2] 

auc_gbm <- performance(gbm_dev,"auc")
auc_gbm <- round(as.numeric(auc_gbm@y.values),3)    