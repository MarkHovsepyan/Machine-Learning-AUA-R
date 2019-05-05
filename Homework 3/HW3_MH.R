## Machine Learning
## Homework 3
## Mark Hovsepyan

## libraries (please install before running with install.packages())
library(glmnet)


## Problem 1
set.seed(1)

x = rnorm(100, 2, 2)
e = rnorm(100, 0, 1)

beta = sample(1:100, 4, replace=TRUE)
y = beta[1] + beta[2] * x + beta[3] * x^2 + beta[4] *x^3 + e


## Problem 2
data.full <- data.frame(y = y, x = x)

set.seed(1)
xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[,-1]

set.seed(1)
cv.ridge <- cv.glmnet(xmat, y, alpha = 0, nfolds = 10)
plot(cv.ridge)

bestlam_ridge <- cv.ridge$lambda.min
bestlam_ridge

fit.ridge <- glmnet(xmat, y, alpha = 0, lambda = bestlam_ridge)

coef(fit.ridge)[, 1]

## Here we see that all of the coefficients are non-zero


## Problem 3
set.seed(1)
cv.lasso <- cv.glmnet(xmat, y, alpha = 1, nfolds = 10)
plot(cv.lasso)

bestlam_lasso <- cv.lasso$lambda.min
bestlam_lasso

fit.lasso <- glmnet(xmat, y, alpha = 1, lambda = bestlam_lasso)

coef(fit.lasso)[,1]

## Here only coefficients for x^2 and x^3 are non-zero


## Problem 4

## With the value of λ giving the minimum cv error, 
## the Lasso shrinks most of the predictors to zero, and only leaves X^2 and X^3 nozero.
## While, with ridge model our λ is a much higher numeric value 
## and the model leaves all of the predictors.

