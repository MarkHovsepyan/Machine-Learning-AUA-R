## Machine Learning
## Homework 4
## Mark Hovsepyan

## libraries (please install before running with install.packages())
library(caret)
library(MASS)
library(ROCR)
library(e1071)

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
## LDA
lda.fit <- lda(response ~ ., data = train)
lda.fit$prior
plot(lda.fit)
lda.pred <- predict(lda.fit, test)
lda_class <- lda.pred$class
table(lda_class, test$response)

## ROC & AUC
lda.pred <- prediction(lda.pred$posterior[, 2], test$response) 
performance(lda.pred, "auc")@y.values # AUC is 0.7903571
plot(performance(lda.pred, "tpr", "fpr"), colorize=TRUE)


## Problem 2
## QDA
qda.fit <- qda(response ~ ., data=train)
qda.pred <- predict(qda.fit, test)
qda_class <- qda.pred$class
table(qda_class, test$response)

## ROC & AUC
qda.pred <- prediction(qda.pred$posterior[, 2], test$response) 
performance(qda.pred, "auc")@y.values # AUC is 0.7514286
plot(performance(qda.pred, "tpr", "fpr"), colorize=TRUE)


## Problem 3
## Naive Bayes
naive.fit <- naiveBayes(response ~ ., data = train)
naive.pred <- predict(naive.fit, test, type = "raw")
naive.pred <- prediction(naive.pred[, 2], test$response)

## ROC & AUC
performance(naive.pred, "auc")@y.values # AUC is 0.7766667
plot(performance(naive.pred, "tpr", "fpr"), colorize=TRUE)


## Problem 4
## Logistic Regression
logistic.fit <- glm(response ~ ., data = train, family = 'binomial')
logistic.pred <- predict(logistic.fit, test, type = "response")
logistic.pred <- prediction(logistic.pred, test$response)

## ROC & AUC
performance(logistic.pred, "auc")@y.values # AUC is 0.785119
plot(performance(logistic.pred,"tpr","fpr"),colorize=TRUE)


## Problem 5
my_grid <- expand.grid(k = 1 : 100)

train_control = trainControl(method = "repeatedcv", 
                             number = 10,
                             repeats = 5,
                             verbose = T)

## KNN
knn.fit <- train(response ~ ., 
                  data = train, 
                  trControl = train_control, 
                  method = 'knn', 
                  tuneGrid = my_grid)

plot(knn.fit)
knn.fit$bestTune # best k is 55 for this setting
knn.pred <- predict(knn.fit, test, type = "prob")
knn.pred <- prediction(knn.pred[, 2], test$response)

## ROC & AUC
performance(knn.pred, "auc")@y.values # AUC is 0.6239286
plot(performance(knn.pred,"tpr","fpr"),colorize=TRUE)


## Problem 6
performance(lda.pred, "auc")@y.values
performance(qda.pred, "auc")@y.values
performance(naive.pred, "auc")@y.values
performance(logistic.pred, "auc")@y.values
performance(knn.pred, "auc")@y.values        

## Performances (AUC)
## LDA: 0.7903571 (Best)
## QDA: 0.7514286
## Naive Bayes: 0.7766667
## Logistic Regression: 0.785119
## KNN: 0.6239286 (Worst)

## So in terms of AUC, it is clear that KNN did badly,
## even with repetitions and thorough selection of k.
## QDA and Naive Bayes are pretty good performers but not best.
## Logistic Regression is on the 2nd place with ~ 0.785.
## Finally, LDA gave the best results with ~ 0.79


## Problem 7
lda.pred <- predict(lda.fit, test, type = "response")

confusionMatrix(data = lda.pred$class, 
                reference = test$response, 
                positive = '2')

## Accuracy: 0.78
## Balanced Accuracy: 0.7143
## Sensitivity: 0.5500
## Specificity/Precision: 0.8786

