## Machine Learning
## Homework 5
## Mark Hovsepyan

## libraries (please install before running with install.packages())
library(caret)
library(MASS)
library(ROCR)
library(e1071)
library(ggplot2)
library(plotROC)
library(rpart.plot)


## getting data
german_credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) <- c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

german_credit$response <- as.factor(german_credit$response)

## Divide into train and test (80 : 20)
set.seed(123)
train_ind <- createDataPartition(german_credit$response, p = 0.8, list = F)

train <- german_credit[train_ind, ]
test <- german_credit[-train_ind, ]



## Problem 1
## Decision Trees
my_grid <- expand.grid(cp = seq(0, 0.4, by = 0.01))

train_control <- trainControl(method = "repeatedcv", 
                             number = 10,
                             repeats = 5,
                             verbose = T)

tree.fit <- train(response ~ ., 
                   data = train, 
                   method = "rpart",
                   tuneLength = 30,
                   trControl = train_control,
                   tuneGrid = my_grid)

ggplot(tree.fit)

## Confusion matrix 
tree.pred_raw <- predict(tree.fit, newdata = test, type = "raw")
tree.pred_prob <- predict(tree.fit, newdata = test, type = "prob")

tree.pred_raw_train <- predict(tree.fit, newdata = train, type = "raw")

cm_test1 <- confusionMatrix(tree.pred_raw, data = test$response, positive = '2')
cm_train1 <- confusionMatrix(tree.pred_raw_train, data = train$response, positive = '2')

cm_test1
cm_train1

## Test:  Pos Pred Value : 0.3667, Accuracy : 0.765 
## Train: Pos Pred Value : 0.4375, Accuracy : 0.7875 

## ROC & AUC 
roc_frame <- data.frame(response = test$response, 
                        prob = tree.pred_prob[,2])

tt_smooth <- ggplot(roc_frame, aes(m = prob, d = response)) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth

calc_auc(tt_smooth) #AUC is 0.721131

## Visualize the tree
rpart.plot(tree.fit$finalModel)
prp(tree.fit$finalModel, digits = 5, roundint = F)


## Problem 2
## Random Forests
rf.fit <- train(response ~ ., 
                 data = train, 
                 method = "rf",
                 ntree = 50 )

## Confusion matrix 
rf.pred_raw <- predict(rf.fit, newdata = test, type = "raw")
rf.pred_prob <- predict(rf.fit, newdata = test, type = "prob")

rf.pred_raw_train <- predict(rf.fit, newdata = train, type = "raw")

cm_test2 <- confusionMatrix(rf.pred_raw, data = test$response, positive = '2')
cm_train2 <- confusionMatrix(rf.pred_raw_train, data = train$response, positive = '2')

cm_test2
cm_train2

## Test:  Pos Pred Value : 0.4833, Accuracy : 0.79 
## Train: Pos Pred Value : 1.0, Accuracy : 1 

## ROC & AUC 
roc_frame_2 <- data.frame(response = test$response, 
                          prob = rf.pred_prob[,2])

tt_smooth2 <- ggplot(roc_frame_2, aes(m = prob, d = response)) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth2

calc_auc(tt_smooth2) #AUC is 0.8210714


## Problem 3
## Boosting
adaboost.fit <- train(response ~ ., 
                        data = train,
                        method = 'adaboost',
                        tuneGrid=expand.grid(nIter=10, method='adaboost'))


## Confusion matrix 
adaboost.pred_raw <- predict(adaboost.fit, newdata = test, type = "raw")
adaboost.pred_prob <- predict(adaboost.fit, newdata = test, type = "prob")

adaboost.pred_raw_train <- predict(adaboost.fit, newdata = train, type = "raw")

cm_test3 <- confusionMatrix(adaboost.pred_raw, data = test$response, positive = '2')
cm_train3 <- confusionMatrix(adaboost.pred_raw_train, data = train$response, positive = '2')

cm_test3
cm_train3

## Test:  Pos Pred Value : 0.5167, Accuracy : 0.74
## Train: Pos Pred Value : 1.0, Accuracy : 1


## ROC & AUC 
roc_frame_3 <- data.frame(response = test$response, 
                          prob = adaboost.pred_prob[,2])

tt_smooth3 <- ggplot(roc_frame_3, aes(m = prob, d = response)) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth3

calc_auc(tt_smooth3) #AUC is 0.6313095


## Problem 4
## Compare the precisions

cm_test1
## Decision Tree Model
## Precision : 0.3667
## Accuracy : 0.765

cm_test2
## Random Forest Model:
## Precision : 0.4833
## Accuracy : 0.79 

cm_test3
## Boosting Model:
## Precision : 0.5167 
## Accuracy : 0.74

## The winner is the boosting model with precision 0.5167












