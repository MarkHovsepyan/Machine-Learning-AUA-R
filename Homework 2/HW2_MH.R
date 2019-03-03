## Machine Learning
## Homework 2
## Mark Hovsepyan

## libraries (please install before running with install.packages())
library(ggplot2)
library(ISLR)


## Problem 1

set.seed(1)

## a)
x <- rnorm(100, mean = 0, sd = 1)


## b)
e <- rnorm(100, mean = 0, sd = 0.25)


## c)
y = -1 + 0.5 * x + e
length(y)

## The length of y is 100. β0 = −1 and β1 = 0.5


## d)
plot(x ~ y)

## We have a clear positive linear relationship between x and y, 
## and a presence of the variance, here var(e).


## e)
lm_fit1 <- lm(y ~ x)
summary(lm_fit1)

## β^0 and β^1 are almost equal to βo and β1. 
## As expected, β^o and β^1 are statistically signficant 
## and R-square = 0.84 shows that the model is a really good fit for the data.


## f)
plot(x, y)
abline(lm_fit1, col="red", lwd=2)
legend("bottomright", c("Linear Regression Line"), lwd=1, col="red",bty ="n")


## g)
lm_fit2 <- lm(y ~ poly(x,2))
summary(lm_fit2)

## The regression coefficient of the quadratic term is not statistically significant,
## so, we can't claim that the quadratic term improves the model.

anova(lm_fit1, lm_fit2)

## Also, here p-value is 0.16 and there’s no strong evidence of difference of the models.


## h)

## let's make more noise
x <- rnorm(100)
e <- rnorm(100, 0, 0.5)
y = -1 + 0.5 * x + e

lm_fit_morenoise <- lm(y ~ x)
summary(lm_fit_morenoise)$coefficients

## let's make less noise
x <- rnorm(100)
e <- rnorm(100, 0, 0.125)
y = -1 + 0.5 * x + e

lm_fit_lessnoise <- lm(y ~ x)

summary(lm_fit_lessnoise)$coefficients

## now comparison
confint(lm_fit1) # original

confint(lm_fit_morenoise) # more noisy

confint(lm_fit_lessnoise) # less noisy

## We can observe almost the bsame confidence intervals for the parameters β0 and β1 in each model,
## but obviously the less noisy data has the biggest one (it can be ovefit as well).



## Problem 2

data(Auto)

## a)
lm_fit_auto1 <- lm(mpg ~ horsepower, data=Auto)
summary(lm_fit_auto1)

## i.
## The p-values for the regression coefficients are nearly zero. 
## That means that there is statistical significance, and consequently, there is a relationship.

## ii.
## The R-square value indicates that about 61% of the variation 
## in the response variable (mpg) is due to the predictor variable (horsepower).

## iii.
predict(lm_fit_auto1, data.frame("horsepower" = 98), interval = "prediction")
## Predicted "mpg" for horspower of 98 is 24.46708

## iv.
## The 95% confidence interval:
predict(lm_fit_auto1, data.frame("horsepower" = 98), interval = "confidence")

## The 95% prediction interval:
predict(lm_fit_auto1, data.frame("horsepower" = 98), interval = "prediction")

## Naturally, the prediction interval is wider than the confidence interval.


## b)
plot(Auto$mpg ~ Auto$horsepower, main = "MPG vs Horsepower", xlab = "Horsepower", ylab ="MPG")
abline(coef = coef(lm_fit_auto1), lwd = 3, col ="red")


## c)
par(mfrow = c(2, 2))
plot(lm_fit_auto1)

## 1. The first plot shows a U-shaped pattern between the residuals and the fitted values.
## This indicates a non-linear relationship between the response variables and the predictor. 
## 2. The second plot shows that the residuals have normal distribution. 
## 3. The third plot implies that we have a constant variance of errors. 
## 4. The fourth plot indicates that there are few leverage points in the data.

## d)
## The plot of standardized residuals versus leverage
## indicates the presence of a few outliers (higher than 2 or lower than -2) 
## and some high leverage points.



## Problem 3

data(Carseats)

## a)
lm_fit_carseats1 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm_fit_carseats1)


## b)
## Null hypothesis can be rejected for the “Price” and “US” variables.


## c)
lm_fit_carseats2 <- lm(Sales ~ Price + US, data = Carseats)
summary(lm_fit_carseats2)


## d)
anova(lm_fit_carseats1, lm_fit_carseats2)

## The R-square value for the smaller model is noticably better than for the bigger model. 
## With fewer degrees of freedom and it has a very good (quite unadjusted) R-square.
## This smaller model is better for us and it is less likely to be overfit.


## e)
confint(lm_fit_carseats2)

## f)
par(mfrow = c(2, 2))
plot(lm_fit_carseats2)

## g)
## The plot of standardized residuals versus leverage indicates the presence of a few outliers (higher than 2 or lower than -2),
## but none of those are very highlited (outstanding) outliers.
## The Residuals vs Leverage graph shows that there are some high leverage observations
## and one that is particularly high, 0.04333766.



## Problem 4

## a)
pairs(Auto)


## b)
cor(Auto[1:8]) # excluding 9 (name)


## c)
lm_fit_auto2 <- lm(mpg ~ . - name, data = Auto)
summary(lm_fit_auto2)

## i.
## Yes, the F-statistic is quite significant with a very small p-value.

## ii.
## The cylinders, the origin and the year and are among significant relationships with mpg.

## iii.
## The coefficient of the “year” variable suggests that increase of 1 year in average is an increase of 0.7507727 in “mpg”. 
## Therefore, cars get more fuel efficient every year by a rate of almost 1 mpg / year.


## d)
par(mfrow = c(2, 2))
plot(lm_fit_auto2)


## e)
## The plot of residuals versus fitted values indicates the presence of some mild non linearity in the data. 
## The plot of standardized residuals versus leverage indicates the presence of a few outliers (higher than 2 or lower than -2) 
## and one high leverage point (point > 14).


## f)
lm_fit_auto_inter1 = lm(mpg ~.-name-cylinders-acceleration+year:origin+displacement:weight+
             displacement:weight+acceleration:horsepower+acceleration:weight, data=Auto)
summary(lm_fit_auto_inter1)

lm_fit_auto_inter_all = lm(mpg ~ (.-name)*(.-name), data = Auto)
summary(lm_fit_auto_inter_all)

## I tried several variants and left the two most improved once.
## The one that includes all of the variables seems to be the most improved.
## The overall model had an improvement in R-square from 0.82 to almost 0.89, 
## maybe it can be overfitting, though the most significant interactive term
## was acceleration:origin with a good coefficient in comparison 
## to the main terms and a small p-value, which validates the coefficient.


## g)

## let's plot some transformations and observe the results
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)

par(mfrow = c(2, 2))
plot(log(Auto$displacement), Auto$mpg)
plot(sqrt(Auto$displacement), Auto$mpg)
plot((Auto$displacement)^2, Auto$mpg)

par(mfrow = c(2, 2))
plot(log(Auto$weight), Auto$mpg)
plot(sqrt(Auto$weight), Auto$mpg)
plot((Auto$weight)^2, Auto$mpg)


## let's make a model
lm_fit_auto_trnsf <- lm(mpg ~ . + sqrt(acceleration) 
               + log(displacement) 
               + cylinders
               + sqrt(weight)
               + log(horsepower)
               + sqrt(year)
               + origin
               - name,
               data = Auto)

summary(lm_fit_auto_trnsf)

## the combination of transformations above seems to be the best out of my experiments.
## I used the plots to determine the best transformation for each variable.
## Our R-squared is almost 0.88, which can possibly be improved.
## Some of the variables are left unchanged cause they may be qualitative (such as origin).



