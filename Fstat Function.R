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

#F-statistic Function
  #calculates the F-statistic of model0 from an anova output
    #model0 must be nested within model1
fstat <- function(model0,model1,anova){
RSS.model0 <- anova$RSS[model0]
RSS.model1 <- anova$RSS[model1]
SS.model1 <- RSS.model0 - RSS.model1
DF.model0 <- anova$Res.Df[model0]
DF.model1 <- anova$Res.Df[model1]
res <- RSS.model1/DF.model1
diff <- SS.model1/(DF.model0-DF.model1)
F.stat <- diff/res
return(F.stat)
}

fstat(2,3,anova)
