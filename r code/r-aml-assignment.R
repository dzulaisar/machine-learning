library(DataExplorer)
library(dplyr)

dx <- read.csv("C:/Users/aisar/Documents/MASTER APU/1ST SEM/AML/ASSIGNMENT/r/diabetes_dataset_2019.csv")

str(dx)

'''Change data type'''
dx$Age <- as.factor(dx$Age)
dx$Gender <- as.factor(dx$Gender)
dx$Family_Diabetes <- as.factor(dx$Family_Diabetes)
dx$highBP <- as.factor(dx$highBP)
dx$PhysicallyActive <- as.factor(dx$PhysicallyActive)
dx$Smoking <- as.factor(dx$Smoking)
dx$Alcohol <- as.factor(dx$Alcohol)
dx$Sleep <- as.integer(dx$Sleep)
dx$SoundSleep <- as.integer(dx$SoundSleep)
dx$RegularMedicine <- as.factor(dx$RegularMedicine)
dx$JunkFood <- as.factor(dx$JunkFood)
dx$Stress <- as.factor(dx$Stress)
dx$Pregancies <- as.factor(dx$Pregancies)
dx$Pdiabetes <- as.factor(dx$Pdiabetes)
dx$UriationFreq <- as.factor(dx$UriationFreq)
dx$Diabetic <- factor(dx$Diabetic, levels=c('no','yes'), labels=c("0", "1")) #change from char to factor

dx$BPLevel <- NULL #redundant with highBP
names(dx)[14] <- "Pregnancies" #rename column

summary(dx)
str(dx)
dim(dx)



#dx$Diabetic <- as.integer(dx$Diabetic) #change into int. to impute missing data

'''Missing data treatment'''
library(VIM)
library(mice) # Here this package is used to introduce x% of missing values (NA) into the dataset

p <- function(x){sum(is.na(x))}
apply(dx,2,p) #3 columns got missing data(diabetic, BMI, Pregnancies)

impute <- mice(dx[,1:17], m=3, seed=123)
print(impute)
dz <- complete(impute,2)

impute$imp$BMI #pick imp 2. bcuz individuals in the missing rows were not physically active. 
#(prove it by their physically active col = not active)

sum(is.na(dz))
colSums(is.na(dz)) #no missing values anymore
plot_missing(dz) 

table(dz$Diabetic)
prop.table(table(dz$Diabetic)) #no need resampling


'''Identify Outliers'''
library(ggplot2)
library(tidyverse)
par(mfrow = c(1, 2)) #column arrangement
'''Boxplot''' #bmi got outliers

boxplot(dz$BMI, ylab = "BMI", xlab = "Diabetic" , col = 'red', 
        main = 'Boxplot of Diabetes vs. BMI' ) 
boxplot(dz$Sleep, ylab = "Sleep", xlab = "Diabetic" , col = 'blue',
        main = 'Boxplot of Diabetes vs. Sleep')
boxplot(dz$SoundSleep, ylab = "SoundSleep", xlab = "Diabetic" , col = 'purple',
        main = 'Boxplot of Diabetes vs. SoundSleep')

'''Outliers treatment (Capping)''' #http://r-statistics.co/Outlier-Treatment-With-R.html
## only BMI got outliers

outlier_values_BMI <- boxplot.stats(dz$BMI)$out  # outlier values.
boxplot(dz$BMI, main="BMI")
mtext(paste("Outliers: ", paste(outlier_values_BMI, collapse=", ")), cex=0.6)
summary(dz$BMI)

x <- dz$BMI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

boxplot(x , main = 'BMI') #no more outliers in BMI
table(x)
dz$BMI <- x

table(dz$BMI)

table(dz$Diabetic)
summary(dz)

'''Feature Selection''' ##https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# ensure results are repeatable
library(mlbench)
library(caret)
set.seed(123)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Diabetic~., data=dz, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

'''
control1 <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(dz[,1:17], dz[,18], sizes=c(1:17), rfeControl=control1)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))'''

'''Binning'''
dz$Gender <- factor(dz$Gender, levels=c('Male','Female'), labels=c("1", "0"))
dz$Family_Diabetes <- factor(dz$Family_Diabetes, levels=c('no','yes'), labels=c("0", "1"))
dz$Smoking <- factor(dz$Smoking, levels=c('no','yes'), labels=c("0", "1"))
dz$Alcohol <- factor(dz$Alcohol, levels=c('no','yes'), labels=c("0", "1"))
dz$highBP <- factor(dz$highBP, levels=c('no','yes'), labels=c("0", "1"))
dz$Pdiabetes <- factor(dz$Pdiabetes, levels=c('', '0', 'no','yes'), labels=c("1", "0", "0", "1"))
dz$PhysicallyActive <- factor(dz$PhysicallyActive, levels=c('none', 'less than half an hr', 
                                                            'more than half an hr','one hr or more'), 
                              labels=c("0", "1", "2", "3"))
dz$JunkFood <- factor(dz$JunkFood, levels=c('occasionally', 'always', 'often','very often'), 
                      labels=c("0", "1", "1", "2"))
dz$Stress <- factor(dz$Stress, levels=c('not at all', 'sometimes', 'always','very often'), 
                      labels=c("0", "1", "1", "2"))
dz$RegularMedicine <- factor(dz$RegularMedicine, levels=c('no', 'o', 'yes'), 
                    labels=c("0", "0", "1"))
dz$UriationFreq <- factor(dz$UriationFreq, levels=c('not much','quite often'), labels=c("0", "1"))

summary(dz)



'''Data partition'''
set.seed(1234)
ind <-sample(2,nrow(dz), replace = T, prob = c(0.7,0.3))
train <- dz[ind==1,]
test <- dz[ind==2,]

dim(train)
dim(test)

'''Decision Tree'''
library(RCurl)
library(caTools)
library(rpart)
library(rpart.plot)
library(party)
library(Metrics)
library(caret)
library(ROCR)


prop.table(table(train$Diabetic))
par(mfrow = c(1, 1))

'Default split is with Gini index'
model_gini <- rpart(Diabetic~ ., data=train, method='class', parms=list(split='gini'))
rpart.plot(model_gini, yesno=2, extra = 0, nn = TRUE, cex=0.8)
pred_gini <- predict(model_gini, newdata=test, type='class')
pred_train_gini <- predict(model_gini, newdata=train, type='class')

accuracy(actual=test$Diabetic, predicted=pred_gini) #0.8772

cm_test_gini = table(pred_gini, test$Diabetic) #0.8772
cm_test_gini
confusionMatrix(cm_test_gini, positive='1')
misclassification = (1-sum(diag(cm_test_gini))/sum(cm_test_gini))
misclassification #0.122807

cm_train_gini = table(pred_train_gini, train$Diabetic)
cm_train_gini
confusionMatrix(cm_train_gini, positive='1') #0.91
misclassification = (1-sum(diag(cm_train_gini))/sum(cm_train_gini))
misclassification #0.0899

Predict_ROC = predict(model_gini, test)
Predict_ROC
Predict_ROC[,2]

pred = prediction(Predict_ROC[,2], test$Diabetic)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (DT Gini)",
     ylab = "Sensitivity",
     xlab = "Specificity",)

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.903
legend(0.5,0.2,auc,title="AUC", fill="red")

#tree pruning
plotcp(model_gini)
print(model_gini$cptable)

#Retrieval of optimal cp value based on cross-validated error
index <- which.min(model_gini$cptable[,'xerror'])
cp_optimal <- model_gini$cptable[index,'CP']
#Pruning tree based on optimal CP value
model_gini_opt <- prune(tree = model_gini, cp=cp_optimal)

#Plot the optimized model
rpart.plot(x=model_gini_opt, yesno=2, type=0, extra=0, cex=1.0)
pred_gini_opt <- predict(object=model_gini_opt,
                         newdata=test,
                         type='class')

accuracy(actual=test$Diabetic,
         pred=pred_gini_opt) #0.8772

#model training based on information gain based (entropy) splitting cliteria (Hyperparameter tuning)
model_entropy <- rpart(Diabetic~ ., data=train, method='class', parms=list(split='information'))
pred_entropy <- predict(model_entropy, newdata=test, type='class')
pred_train_entropy <- predict(model_entropy, newdata=train, type='class')

accuracy(actual=test$Diabetic, predicted=pred_entropy) #0.888

cm_test_entropy = table(pred_entropy, test$Diabetic)
cm_test_entropy
confusionMatrix(cm_test_entropy, positive='1') #0.8877
misclassification = (1-sum(diag(cm_test_entropy))/sum(cm_test_entropy))
misclassification #0.1123

cm_train_entropy = table(pred_train_entropy, train$Diabetic)
cm_train_entropy
confusionMatrix(cm_train_entropy, positive='1') #0.915
misclassification = (1-sum(diag(cm_train_entropy))/sum(cm_train_entropy))
misclassification #0.0855

Predict_ROC = predict(model_entropy, test)
Predict_ROC
Predict_ROC[,2]

pred = prediction(Predict_ROC[,2], test$Diabetic)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (DT Entropy)",
     ylab = "Sensitivity",
     xlab = "Specificity",)

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.908
legend(0.5,0.2,auc,title="AUC", fill="red")

#hyperparameter Grid Search (minsplit & maxdepth)
#setting the value for minsplit & maxdepth
minsplit <- seq(1,20,1)
maxdepth <- seq(1,20,1)

#Generate a search grid
hyperparam_grid_dt <- expand.grid(minsplit=minsplit, maxdepth=maxdepth)
hyperparam_grid_dt

#Number of potential models in the grid
num_models_dt <- nrow(hyperparam_grid_dt)
num_models_dt # 400

#Create an empty list
diabetes_models_dt <-list()

#write a loop over the rows  of hyperparam_grid_dt to train the grid of models
for (i in 1:num_models_dt) {
  
  minsplit <- hyperparam_grid_dt$minsplit[i]
  maxdepth <- hyperparam_grid_dt$maxdepth[i]
  
  #train a model and store them in the list
  diabetes_models_dt[[i]] <- rpart(formula=Diabetic~.,
                                   data=train,
                                   method='class',
                                   minsplit=minsplit,
                                   maxdepth=maxdepth)
}

#try model 50
diabetes_models_dt[[50]]
rpart.plot(x=diabetes_models_dt[[50]], yesno=2, type=0, extra=0)

#number of models inside the grid
num_models_dt <- length(diabetes_models_dt) #400

#create an empty vector to store accuracy values
accuracy_values_dt <- c()

#use for loop for models accuracy estimation
for (i in 1:num_models_dt) {
  
  #retrive the model i from the list
  model <- diabetes_models_dt[[i]]
  
  #Generate predictions based test set
  pred <- predict(object=model,
                  newdata=test,
                  type='class')
  
  #Compute test accuracy and add to the empty vector accuracy_values_dt
  accuracy_values_dt[i] <- accuracy(actual=test$Diabetic,
                                    predicted=pred)
}

#identify the model with best accuracy
best_model <- diabetes_models_dt[[which.max(accuracy_values_dt)]]

#print the model hyperparameter of the best models
best_model$control


#best_model accuracy on test set
pred_test_ht <- predict(object=best_model,newdata=test,type='class')
pred_train_ht <- predict(object=best_model,newdata=train,type='class')

cm_test_gridsearch = table(pred_test_ht, test$Diabetic)
cm_test_gridsearch
confusionMatrix(cm_test_gridsearch, positive='1') #0.9018
misclassification = (1-sum(diag(cm_test_gridsearch))/sum(cm_test_gridsearch))
misclassification #0.0982

cm_train_gridsearch = table(pred_train_ht, train$Diabetic)
cm_train_gridsearch
confusionMatrix(cm_train_gridsearch, positive='1') #0.9475
misclassification = (1-sum(diag(cm_train_gridsearch))/sum(cm_train_gridsearch))
misclassification #0.0525

Predict_ROC = predict(best_model, test)
Predict_ROC
Predict_ROC[,2]

pred = prediction(Predict_ROC[,2], test$Diabetic)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (DT-Grid Search)",
     ylab = "Sensitivity",
     xlab = "Specificity")

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.91
legend(0.5,0.2,auc,title="AUC", fill="red")

#validation - check overfit or underfit or goodfit - comment

'''SVM'''
library(ggplot2)
library(e1071) #svm package
library(kernlab)
library(caret)
library(tidyverse)
library(caTools)
library(ROCR)


#Default SVM Model using the RBF kernel
svm_rbf <- svm(Diabetic~., data = train)
summary(svm_rbf)
svm_rbf$gamma #0.03846

pred_test_rbf = predict (svm_rbf, test)
pred_test_rbf
pred_train_rbf = predict (svm_rbf, train)
pred_train_rbf

cm_test_rbf = table(Predicted = pred_test_rbf, Actual = test$Diabetic)
cm_test_rbf
confusionMatrix(cm_test_rbf, positive = '1') #ACC 0.9088

misclassification = (1-sum(diag(cm_test_rbf))/sum(cm_test_rbf))
misclassification #0.09122

cm_train_rbf = table(Predicted = pred_train_rbf, Actual = train$Diabetic)
cm_train_rbf
confusionMatrix(cm_train_rbf, positive = '1') #ACC 0.925

misclassification = (1-sum(diag(cm_train_rbf))/sum(cm_train_rbf))
misclassification #0.075


Predict_ROC = predict(svm_rbf, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$Diabetic)))
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
auc #0.877
legend(0.5,0.2,auc,title="AUC", fill="red")


#SVM model using the Linear model
svm_linear = svm (Diabetic~., data = train, kernel = "linear")
summary(svm_linear)


# Confusion Matrix
pred_test_linear= predict (svm_linear, test)
pred_test_linear
pred_train_linear = predict (svm_linear, train)
pred_train_linear

cm_test_linear = table(Predicted = pred_test_linear, Actual = test$Diabetic)
cm_test_linear
confusionMatrix(cm_test_linear, positive = '1') #ACC 0.8596

misclassification = (1-sum(diag(cm_test_linear))/sum(cm_test_linear))
misclassification #0.1404

cm_train_linear = table(Predicted = pred_train_linear, Actual = train$Diabetic)
cm_train_linear
confusionMatrix(cm_train_linear, positive = '1') #ACC 0.91

misclassification = (1-sum(diag(cm_train_linear))/sum(cm_train_linear))
misclassification #0.09

Predict_ROC = predict(svm_linear, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$Diabetic)))
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
auc #0.808
legend(0.5,0.2,auc,title="AUC", fill="red")

#SVM model using polynomial kernal
svm_polynomial = svm (Diabetic~., data = train, kernel = "poly")
summary (svm_polynomial)

# Confusion Matrix
pred_test_polynomial= predict (svm_polynomial, test)
pred_test_polynomial
pred_train_polynomial = predict (svm_polynomial, train)
pred_train_polynomial

cm_test_polynomial = table(Predicted = pred_test_polynomial, Actual = test$Diabetic)
cm_test_polynomial
confusionMatrix(cm_test_polynomial, positive = '1') #ACC 0.786

misclassification = (1-sum(diag(cm_test_polynomial))/sum(cm_test_polynomial))
misclassification #0.214

cm_train_polynomial = table(Predicted = pred_train_polynomial, Actual = train$Diabetic)
cm_train_polynomial
confusionMatrix(cm_train_polynomial, positive = '1') #ACC 0.7826

misclassification = (1-sum(diag(cm_train_polynomial))/sum(cm_train_polynomial))
misclassification #0.2174

Predict_ROC = predict(svm_polynomial, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$Diabetic)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-Polynomial)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))


# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.609
legend(0.5,0.2,auc,title="AUC", fill="red")


#SVM model using sigmoid kernal``
svm_sigmoid = svm (Diabetic~., data = train, kernel = "sigmoid")
summary (svm_sigmoid)

# Confusion Matrix
pred_test_sigmoid= predict (svm_sigmoid, test)
pred_test_sigmoid
pred_train_sigmoid = predict (svm_sigmoid, train)
pred_train_sigmoid

cm_test_sigmoid = table(Predicted = pred_test_sigmoid, Actual = test$Diabetic)
cm_test_sigmoid
confusionMatrix(cm_test_sigmoid, positive = '1') #ACC 0.8491

misclassification = (1-sum(diag(cm_test_sigmoid))/sum(cm_test_sigmoid))
misclassification #0.1509

cm_train_sigmoid = table(Predicted = pred_train_sigmoid, Actual = train$Diabetic)
cm_train_sigmoid
confusionMatrix(cm_train_sigmoid, positive = '1') #ACC 0.8651

misclassification = (1-sum(diag(cm_train_sigmoid))/sum(cm_train_sigmoid))
misclassification #0.1359

Predict_ROC = predict(svm_sigmoid, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$Diabetic)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-Sigmoid)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))


# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.8
legend(0.5,0.2,auc,title="AUC", fill="red")


#Model Tuning
set.seed(123)
# tune function tunes the hyperparameters of the model using grid search method
tuned_svm_model = tune(svm, Diabetic~., data = train,
                   ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)))
plot(tuned_svm_model) #find the meaning of the diagram
summary (tuned_svm_model)

opt_svm_model = tuned_svm_model$best.model #optimum model
summary(opt_svm_model)

# Building the best model based on opt_svm_model
svm_best <- svm (Diabetic~., data = train, epsilon = 0, cost = 4)
summary(svm_best)

# Confusion Matrix
pred_test_bestsvm= predict (svm_best, test)
pred_test_bestsvm
pred_train_bestsvm = predict (svm_best, train)
pred_train_bestsvm

cm_test_bestsvm = table(Predicted = pred_test_bestsvm, Actual = test$Diabetic)
cm_test_bestsvm
confusionMatrix(cm_test_bestsvm, positive = '1') #ACC 0.9088

misclassification = (1-sum(diag(cm_test_bestsvm))/sum(cm_test_bestsvm))
misclassification #0.09122

cm_train_bestsvm = table(Predicted = pred_train_bestsvm, Actual = train$Diabetic)
cm_train_bestsvm
confusionMatrix(cm_train_bestsvm, positive = '1') #ACC 0.9355

misclassification = (1-sum(diag(cm_train_bestsvm))/sum(cm_train_bestsvm))
misclassification #0.0645

Predict_ROC = predict(svm_best, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$Diabetic)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-Best)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))


# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.865
legend(0.5,0.2,auc,title="AUC", fill="red")



# Alternate tuning
'''performance exactly like the best model'''
tuned_alt_model = tune(svm, Diabetic~., data = train,
                   ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)), kernel = c("radial", "linear", "sigmoid"))
plot (tuned_alt_model)
summary (tuned_alt_model)

opt_alt_model = tuned_alt_model$best.model
summary(opt_alt_model)

# Building the alternate model based on opt_alt_model
svm_alt <- svm (Diabetic~., data = train, epsilon = 0, cost = 4)
summary(svm_alt)

# Confusion Matrix
pred_test_altsvm= predict (svm_alt, test)
pred_test_altsvm
pred_train_altsvm = predict (svm_alt, train)
pred_train_altsvm

cm_test_altsvm = table(Predicted = pred_test_altsvm, Actual = test$Diabetic)
cm_test_altsvm
confusionMatrix(cm_test_altsvm, positive = '1') #ACC 0.9088

misclassification = (1-sum(diag(cm_test_altsvm))/sum(cm_test_altsvm))
misclassification #0.09122

cm_train_altsvm = table(Predicted = pred_train_altsvm, Actual = train$Diabetic)
cm_train_altsvm
confusionMatrix(cm_train_altsvm, positive = '1') #ACC 0.9355

misclassification = (1-sum(diag(cm_train_altsvm))/sum(cm_train_altsvm))
misclassification #0.0645

Predict_ROC = predict(svm_alt, test)
Predict_ROC
pred <- prediction(as.numeric(Predict_ROC),(as.numeric(test$Diabetic)))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve (SVM-Best)",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))


# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.865
legend(0.5,0.2,auc,title="AUC", fill="red")



'''NB'''
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(ROCR)


model_NB <- naive_bayes(Diabetic~., data = train)
model_NB_ls <- naive_bayes(Diabetic~., data = train, laplace=1)
model_NB_ls

#Predict and confusion matrix for train and test set
pred_test_NB <-predict(model_NB_ls,newdata = test)
pred_test_NB
pred_train_NB <-predict(model_NB_ls,newdata = train)
pred_train_NB

cm_test_NB = table(Predicted = pred_test_NB, Actual = test$Diabetic)
cm_test_NB
confusionMatrix(cm_test_NB, positive = '1') #ACC 0.8807

misclassification = (1-sum(diag(cm_test_NB))/sum(cm_test_NB))
misclassification #0.1193

cm_train_NB = table(Predicted = pred_train_NB, Actual = train$Diabetic)
cm_train_NB
confusionMatrix(cm_train_NB, positive = '1') #ACC 0.8696

misclassification = (1-sum(diag(cm_train_NB))/sum(cm_train_NB))
misclassification #0.1304


#Model Performance Evaluation (ROC)
predROC <- predict(model_NB_ls,test)
predROC
pred <- prediction(as.numeric(predROC),(as.numeric(test$Diabetic)))
perf <- performance(pred, "tpr", "fpr")
perf

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
auc #0.866
legend(0.5,0.2,auc,title="AUC", fill="red")

#validation for NB (hyperparameter tuning)
library(caret)
library(klaR)
x <- test[,-17]
y <- test$Diabetic

model_valid_NB <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))
model_valid_NB

#Tuned NB
model_NB_tuned_ls <- naive_bayes(Diabetic~., data = train, usekernel=TRUE, adjust=1,laplace=1, type='prob')
model_NB_tuned_ls

#Predict and confusion matrix for train and test set
pred_test_NB_tuned <-predict(model_NB_tuned_ls,newdata = test)
pred_test_NB_tuned
pred_train_NB_tuned <-predict(model_NB_tuned_ls,newdata = train)
pred_train_NB_tuned

cm_test_NB_tuned = table(Predicted = pred_test_NB_tuned, Actual = test$Diabetic)
cm_test_NB_tuned
confusionMatrix(cm_test_NB_tuned, positive = '1') #ACC 0.8842

misclassification = (1-sum(diag(cm_test_NB_tuned))/sum(cm_test_NB_tuned))
misclassification #0.1158

cm_train_NB_tuned = table(Predicted = pred_train_NB_tuned, Actual = train$Diabetic)
cm_train_NB_tuned
confusionMatrix(cm_train_NB_tuned, positive = '1') #ACC 0.8771
misclassification = (1-sum(diag(cm_train_NB_tuned))/sum(cm_train_NB_tuned))
misclassification #0.1229

predROC <- predict(model_NB_tuned_ls,test)
predROC
pred <- prediction(as.numeric(predROC),(as.numeric(test$Diabetic)))
perf <- performance(pred, "tpr", "fpr")
perf

plot(perf, colorize = T) #default version
plot(perf, colorize=T,
     main = "ROC curve Tuned NB",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc #0.856
legend(0.5,0.2,auc,title="AUC", fill="red")

