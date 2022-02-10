#(a) Perform polynomial regression to predict wage using age. Use
# cross-validation to select the optimal degree d for the polynomial
library(ISLR)
library(boot)
data("Wage")
summary(Wage)
set.seed(1)
degree <- 10
cv.errors <- rep(NA, degree)
for (i in 1:degree){
  fit <- glm(wage~poly(age,i), data = Wage)
  cv.errors[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(1:degree, cv.errors, xlab = "Degree", ylab = "Test MSE", type = "l")
deg.min <- which.min(cv.errors)
points(deg.min, cv.errors[deg.min], col = "red", cex = 2, pch = 19)
# What degree was chosen, and how does this compare to
# the results of hypothesis testing using ANOVA? Make a plot of
# the resulting polynomial fit to the data.
plot(wage ~ age, data = Wage, col = "darkgrey")
model <- glm(wage~poly(age,4), data = Wage)
age.range <- range(Wage$age)
age.grid <- seq(from = age.range[1], to=age.range[2])
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)
# Anova
fit1 <- lm(wage~poly(age,1), data = Wage)
fit2 <- lm(wage~poly(age,2), data = Wage)
fit3 <- lm(wage~poly(age,3), data = Wage)
fit4 <- lm(wage~poly(age,4), data = Wage)
fit5 <- lm(wage~poly(age,5), data = Wage)
fit6 <- lm(wage~poly(age,6), data = Wage)
fit7 <- lm(wage~poly(age,7), data = Wage)
fit8 <- lm(wage~poly(age,8), data = Wage)
fit9 <- lm(wage~poly(age,9), data = Wage)
fit10 <- lm(wage~poly(age,10), data = Wage)
anova <- anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)
anova

# (b) Fit a step function to predict wage using age, and perform cross validation to choose the optimal number of cuts. Make a plot of the fit 
set.seed(1)
cv.errors <- rep(NA, degree)
for(i in 2:degree){
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cv.errors[i] <- cv.glm(Wage, fit, K = 10 )$delta[1]
}
plot(2:degree, cv.errors[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
deg.min <- which.min(cv.errors)
points(deg.min, cv.errors[deg.min], col = "red", cex = 2, pch = 19)
plot(wage ~ age, data = Wage, col = "darkgrey")
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)
