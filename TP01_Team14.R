# Libraries
library(ISLR)
library(boot)

# Data
data("Wage")
summary(Wage)

# (a)
# Perform polynomial regression to predict wage using age.
# Use cross-validation to select the optimal degree d for the polynomial.
set.seed(1)
degree <- 10
cv.errors <- rep(NA, degree)
for (i in 1:degree){
  fit <- glm(wage~poly(age,i), data = Wage)
  cv.errors[i] <- cv.glm(Wage, fit, K = 10)$delta[1] #Chose to use K-fold over LOOCV (faster)
}
summary(fit)

#Plot of Test MSEs
plot(1:degree, cv.errors, xlab = "Degree", ylab = "Test MSE", type = "l")
abline(h = min(cv.errors) + sd(cv.errors) , lty = "dotted")           #shows 1se
deg.min <- which.min(cv.errors)
points(deg.min, cv.errors[deg.min], col = "red", cex = 2, pch = 19)

# Degree Chosen by CV: 9
  # It is unusual to use d greater than 3 or 4 because for large values of d, the polynomial
  # curve can become overly flexible so instead we follow the rule of parsimony and choose
  # the smallest value of d within 1 standard error
# Most parsimonious model within 1se of the minimum: 2


# Plot of polynomial fit to the data
d <- 2     #when changed from 4 to 2, the curve becomes less flexible
  #When d = 9, you can see that the curve starts getting too flexible as age increases and n decreases

plot(wage ~ age, data = Wage, col = "darkgrey",  bty = "n")
agelims <-  range(Wage$age)
age.grid <-  seq(from = agelims[1], to = agelims[2])
lm.fit <-  lm(wage ~ poly(age, d), data = Wage)
lm.pred <-  predict(lm.fit, data.frame(age = age.grid), se = TRUE)
# mean prediction
lines(x = age.grid , y = lm.pred$fit, col = "blue", lwd = 2)
# uncertainty bands
matlines( x = age.grid, y = cbind( lm.pred$fit + 2*lm.pred$se.fit, lm.pred$fit - 2*lm.pred$se.fit),
          lty = "dashed", col = "blue")


# ANOVA
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

# Degree Chosen by ANOVA: 4
#
  # The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero, indicating that a linear fit is not sufficient. 
  # Similarly the p-value comparing the quadratic Model 2 to the cubic Model 3 is very low, so the quadratic fit is also not a significant improvement. 
  # The p-value comparing the cubic and degree-4 polynomials, Model 3 and Model 4, is approximately 5 % 
      ###This indicates that there is about a 95% probability that model 4 is a significant improvement over model 3
  # The degree-5 polynomial Model 5 seems unnecessary because its p-value is 0.37. 
  # Hence, either a cubic or a quartic polynomial appear to provide a reasonable fit to the data
  # Lower- or higher-order models are not justified.
# Comparison to CV: CV chose a higher dimensional model than ANOVA although with the rule of parsimony it is lower than ANOVA.




# (b)
# Fit a step function to predict wage using age and
# perform cross validation to choose the optimal number of cuts.
set.seed(1)
cv.errors <- rep(NA, degree)
for(i in 2:degree){
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cv.errors[i] <- cv.glm(Wage, fit, K = 10 )$delta[1]
}
summary(fit)

# Plot
plot(2:10, cv.errors[-1], xlab = "Number of cuts", ylab = "CV error", 
     type = "b", pch = 19, lwd = 2, bty ="n")
# horizontal line for 1se to less complexity
abline(h = min(cv.errors, na.rm = TRUE) + sd(cv.errors, na.rm = TRUE) , lty = "dotted")
# highlight minimum
points( x = which.min(cv.errors), y = min(cv.errors, na.rm = TRUE), col = "red", pch = "X", cex = 1.5 )


# Degree Chosen by CV: 8
# Most parsimonious model within 1se of the minimum: 4

# Plot of polynomial fit to the data
plot(2:degree, cv.errors[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
deg.min <- which.min(cv.errors)
points(deg.min, cv.errors[deg.min], col = "red", cex = 2, pch = 19)
plot(wage ~ age, data = Wage, col = "darkgrey")
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)
