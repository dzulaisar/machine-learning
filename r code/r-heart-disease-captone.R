library(DataExplorer)
library(dplyr)

'''Reading the dataset'''
ds <- read.csv("C:/Users/User/Documents/MASTER APU/3RD SEM/CAPSTONE 2/heart_disease_uci.csv")
str(ds)

'''Data preprocessing'''
#Change data type
ds$sex <- as.factor(ds$sex)
ds$dataset <- as.factor(ds$dataset)
ds$cp <- as.factor(ds$cp)
ds$fbs <- factor(ds$fbs, levels=c('FALSE','TRUE'), labels=c("0", "1")) #change from binary to factor
ds$restecg <- as.factor(ds$restecg)
ds$exang <- factor(ds$exang, levels=c('FALSE','TRUE'), labels=c("0", "1")) #change from binary to factor
ds$oldpeak <- as.integer(ds$oldpeak)
ds$slope <-as.factor(ds$slope)
ds$ca <-as.factor(ds$ca)
ds$thal <- as.factor(ds$thal)
ds$num <- as.factor(ds$num)

#Renaming the column
names(ds)[16] <- "severity" #rename column from num to severity of heart disease

#Dropping insignificant column
ds$id <- NULL #not important

#Binning
ds$severity <- factor(ds$severity, levels=c('0','1','2','3','4'), labels=c("0", "1","1","1","1")) 
#change severity 1-4 to 1 - to standardize the existence of heart disease

summary(ds)
str(ds)
dim(ds)

'''Missing data treatment'''
library(VIM)
library(mice) #package to introduce x% of missing values (NA) into the dataset

p <- function(x){sum(is.na(x))}
apply(ds,2,p) #7 columns got missing data found in trestbps, chol, fbs, thalch, exang, oldpeak, ca

impute <- mice(ds[,1:15], m=3, seed=123) #got 15 col in dataset. m is Number of multiple imputations. 
                                        #The default is m=5. set m=3
print(impute)
dq <- complete(impute,2)

impute$imp$trestbps #pick imp 2

sum(is.na(dq))
colSums(is.na(dq)) #no missing values anymore
plot_missing(dq) 

table(dq$severity)
prop.table(table(dq$severity)) #no need resampling


par(mfrow = c(1, 2)) #column arrangement
boxplot(dq$age, ylab = "Age", xlab = "Severity" , col = 'red', 
        main = 'Boxplot of Severity vs. Age' ) 
boxplot(dq$trestbps, ylab = "Trestbps", xlab = "Severity" , col = 'blue',
        main = 'Boxplot of Severity vs. Trestbps')
boxplot(dq$chol, ylab = "Chol", xlab = "Severity" , col = 'purple',
        main = 'Boxplot of Severity vs. Chol')
boxplot(dq$thalch, ylab = "thalch", xlab = "Severity" , col = 'green', 
        main = 'Boxplot of Severity vs. thalch' ) 
boxplot(dq$oldpeak, ylab = "oldpeak", xlab = "Severity" , col = 'yellow',
        main = 'Boxplot of Severity vs. oldpeak')
boxplot(dq$ca, ylab = "ca", xlab = "Severity" , col = 'orange',
        main = 'Boxplot of Severity vs. ca')


'''Outliers treatment (Capping)''' 
#http://r-statistics.co/Outlier-Treatment-With-R.html
##trestbps, chol, thalch, oldpeak got outliers

outlier_values_trestbps <- boxplot.stats(dq$trestbps)$out #outlier values.
boxplot(dq$trestbps, main="trestbps", col = 'red')
mtext(paste("Outliers: ", paste(outlier_values_trestbps, collapse=", ")), cex=0.6)
summary(dq$trestbps)

x1 <- dq$trestbps
qnt <- quantile(x1, probs=c(.25, .75), na.rm = T)
caps <- quantile(x1, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x1, na.rm = T)
x1[x1 < (qnt[1] - H)] <- caps[1]
x1[x1 > (qnt[2] + H)] <- caps[2]

boxplot(x1 , main = 'Trestbps', col = 'red') #no more outliers in trestbps


outlier_values_chol <- boxplot.stats(dq$chol)$out  # outlier values.
boxplot(dq$chol, main="chol", col = 'green')
mtext(paste("Outliers: ", paste(outlier_values_chol, collapse=", ")), cex=0.6)
summary(dq$chol)

x2 <- dq$chol
qnt <- quantile(x2, probs=c(.25, .75), na.rm = T)
caps <- quantile(x2, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x2, na.rm = T)
x2[x2 < (qnt[1] - H)] <- caps[1]
x2[x2 > (qnt[2] + H)] <- caps[2]

boxplot(x2 , main = 'Chol', col = 'green') #no more outliers in Chol


outlier_values_thalch <- boxplot.stats(dq$thalch)$out  # outlier values.
boxplot(dq$thalch, main="thalch", col = 'blue')
mtext(paste("Outliers: ", paste(outlier_values_thalch, collapse=", ")), cex=0.6)
summary(dq$thalch)

x3 <- dq$thalch
qnt <- quantile(x3, probs=c(.25, .75), na.rm = T)
caps <- quantile(x3, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x3, na.rm = T)
x3[x3 < (qnt[1] - H)] <- caps[1]
x3[x3 > (qnt[2] + H)] <- caps[2]

boxplot(x3 , main = 'Thalch', col = 'blue') #no more outliers in thalch


outlier_values_oldpeak <- boxplot.stats(dq$oldpeak)$out  # outlier values.
boxplot(dq$oldpeak, main="oldpeak", col = 'purple')
mtext(paste("Outliers: ", paste(outlier_values_oldpeak, collapse=", ")), cex=0.6)
summary(dq$oldpeak)

x4 <- dq$oldpeak
qnt <- quantile(x4, probs=c(.25, .85), na.rm = T)
caps <- quantile(x4, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x4, na.rm = T)
x4[x4 < (qnt[1] - H)] <- caps[1]
x4[x4 > (qnt[2] + H)] <- caps[2]

boxplot(x4 , main = 'Oldpeak', col = 'purple') #no more outliers in oldpeak

#Updating the value into the dataset
dq$trestbps <- x1
dq$chol <- x2
dq$thalch <- x3
dq$oldpeak <- x4

'''Feature Selection''' 
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
#ensure results are repeatable

'''BORUTA FS'''
library(Boruta)

set.seed(111)
boruta <- Boruta(severity~., data=dq, doTrace=2, maxRuns=100)
print(boruta)  #all deemed to be important
plot(boruta, las=2, cex.axis=0.8)

#bor <- TentativeRoughFix(boruta)
#print(bor)

library(mlbench)
library(caret)
set.seed(123)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(severity~., data=dq, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance) # cp, thalch, exang, oldpeak, ca , age, dataset n sex r important

#dropping unimportant columns
dq$restecg <- NULL #not important
dq$fbs <- NULL #not important
dq$slope <- NULL #not important
dq$thal <- NULL #not important
dq$trestbps <- NULL #not important
dq$chol <- NULL #not important

'''Data partition'''
set.seed(3456)
ind <-sample(2,nrow(dq), replace = T, prob = c(0.75,0.25))

train <- dq[ind==1,]
test <- dq[ind==2,]

dim(train)
dim(test)

'''NB'''
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(ROCR)

#NB model creation
model_NB <- naive_bayes(severity~., data = train)

#need to apply laplace smoothing
model_NB_ls <- naive_bayes(severity~., data = train, laplace=1, usekernel=TRUE)
model_NB_ls

#Predict and confusion matrix for train and test set
pred_test_NB <-predict(model_NB_ls,newdata = test)
pred_test_NB

cm_test_NB = table(Predicted = pred_test_NB, Actual = test$severity)
cm_test_NB
confusionMatrix(cm_test_NB, positive = '1') #ACC 0.8273

misclassification = (1-sum(diag(cm_test_NB))/sum(cm_test_NB))
misclassification #0.1727

pred_train_NB <-predict(model_NB_ls,newdata = train)
pred_train_NB

cm_train_NB = table(Predicted = pred_train_NB, Actual = train$severity)
cm_train_NB
confusionMatrix(cm_train_NB, positive = '1') #ACC 0.8571

misclassification = (1-sum(diag(cm_train_NB))/sum(cm_train_NB))
misclassification #0.1429


#Model Performance Evaluation (ROC)
predROC <- predict(model_NB_ls,test)
predROC
pred <- prediction(as.numeric(predROC),(as.numeric(test$severity)))
perf <- performance(pred, "tpr", "fpr")
perf

plot(perf, colorize = T) #default version
plot(perf, colorize=T,
     main = "ROC curve NB",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.838
legend(0.5,0.3,auc,title="AUC", fill="red")


#NB (hyperparameter tuning)
library(caret)
library(klaR)
x <- test[,-15]
y <- test$severity

model_valid_NB <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))
model_valid_NB

#Tuned NB
model_NB_tuned_ls <- naive_bayes(severity~., data = train, usekernel=FALSE, adjust=1,laplace=1, type='prob')
model_NB_tuned_ls

#Predict and confusion matrix for train and test set
pred_test_NB_tuned <-predict(model_NB_tuned_ls,newdata = test)
pred_test_NB_tuned

cm_test_NB_tuned = table(Predicted = pred_test_NB_tuned, Actual = test$severity)
cm_test_NB_tuned
confusionMatrix(cm_test_NB_tuned, positive = '1') #ACC 0.8318

misclassification = (1-sum(diag(cm_test_NB_tuned))/sum(cm_test_NB_tuned))
misclassification #0.1682

pred_train_NB_tuned <-predict(model_NB_tuned_ls,newdata = train)
pred_train_NB_tuned

cm_train_NB_tuned = table(Predicted = pred_train_NB_tuned, Actual = train$severity)
cm_train_NB_tuned

confusionMatrix(cm_train_NB_tuned, positive = '1') #ACC 0.85
misclassification = (1-sum(diag(cm_train_NB_tuned))/sum(cm_train_NB_tuned))
misclassification #0.15

predROC <- predict(model_NB_tuned_ls,test)
predROC
pred <- prediction(as.numeric(predROC),(as.numeric(test$severity)))
perf <- performance(pred, "tpr", "fpr")
perf

plot(perf, colorize = T) #default version
plot(perf, colorize=T,
     main = "ROC curve-Tuned NB",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.839
legend(0.5,0.3,auc,title="AUC", fill="blue")


'''SVM'''
library(ggplot2)
library(e1071) #svm package
library(kernlab)
library(caret)
library(tidyverse)
library(caTools)
library(ROCR)


#Default SVM Model using the RBF kernel
svm_rbf <- svm(severity~., data = train)
summary(svm_rbf)
svm_rbf$gamma #0.037

pred_test_rbf = predict (svm_rbf, test)
pred_test_rbf

cm_test_rbf = table(Predicted = pred_test_rbf, Actual = test$severity)
cm_test_rbf
confusionMatrix(cm_test_rbf, positive = '1') #ACC 0.8227

misclassification = (1-sum(diag(cm_test_rbf))/sum(cm_test_rbf))
misclassification #0.1773

pred_train_rbf = predict (svm_rbf, train)
pred_train_rbf

cm_train_rbf = table(Predicted = pred_train_rbf, Actual = train$severity)
cm_train_rbf
confusionMatrix(cm_train_rbf, positive = '1') #ACC 0.8629

misclassification = (1-sum(diag(cm_train_rbf))/sum(cm_train_rbf))
misclassification #0.1371


Predict_ROC = predict(svm_rbf, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$severity)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-RBF)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))


# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.824
legend(0.5,0.3,auc,title="AUC", fill="red")

#SVM model using the Linear model
svm_linear = svm (severity~., data = train, kernel = "linear")
summary(svm_linear)
svm_linear$gamma #0.037

# Confusion Matrix
pred_test_linear= predict (svm_linear, test)
pred_test_linear

cm_test_linear = table(Predicted = pred_test_linear, Actual = test$severity)
cm_test_linear
confusionMatrix(cm_test_linear, positive = '1') #ACC 0.8182

misclassification = (1-sum(diag(cm_test_linear))/sum(cm_test_linear))
misclassification #0.1818

pred_train_linear = predict (svm_linear, train)
pred_train_linear

cm_train_linear = table(Predicted = pred_train_linear, Actual = train$severity)
cm_train_linear
confusionMatrix(cm_train_linear, positive = '1') #ACC 0.8586

misclassification = (1-sum(diag(cm_train_linear))/sum(cm_train_linear))
misclassification #0.1414

Predict_ROC = predict(svm_linear, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$severity)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-Linear)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.824
legend(0.5,0.3,auc,title="AUC", fill="red")


#SVM Model Tuning 
set.seed(12345)
# tune function tunes the hyperparameters of the model using grid search method
tuned_svm_model = tune(svm, severity~., data = train,
                       ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)))
plot(tuned_svm_model)
summary (tuned_svm_model)

opt_svm_model = tuned_svm_model$best.model #optimum model
summary(opt_svm_model)

# Building the best model based on opt_svm_model
svm_best <- svm (severity~., data = train, method='svmRadial', epsilon = 0, cost = 4, trControl=ctrl)
summary(svm_best)

# Confusion Matrix
pred_test_bestsvm= predict (svm_best, test)
pred_test_bestsvm

cm_test_bestsvm = table(Predicted = pred_test_bestsvm, Actual = test$severity)
cm_test_bestsvm
confusionMatrix(cm_test_bestsvm, positive = '1') #ACC 0.8136

misclassification = (1-sum(diag(cm_test_bestsvm))/sum(cm_test_bestsvm))
misclassification #0.1864

pred_train_bestsvm = predict (svm_best, train)
pred_train_bestsvm

cm_train_bestsvm = table(Predicted = pred_train_bestsvm, Actual = train$severity)
cm_train_bestsvm
confusionMatrix(cm_train_bestsvm, positive = '1') #ACC 0.8986

misclassification = (1-sum(diag(cm_train_bestsvm))/sum(cm_train_bestsvm))
misclassification #0.1014

Predict_ROC = predict(svm_best, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$severity)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-Tuned)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))


# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.818
legend(0.5,0.3,auc,title="AUC", fill="blue")
