# lm command

mymodel <- lm(y ~ x) 
# if y is a matrix, lm fits several regressions, column by column
mymodel <- lm(y ~ x - 1) 
# no constant term

mymodel$fitted # or mymodel$fitted.values 
# y_hat
mymodel$coef # or coef(mymodel)
# alpha_hat, beta_hat
mymodel$residuals
# e = y - alpha_hat - X * beta_hat
vcov(mymodel)
# s^2 * (X'X)^(-1) covariance matrix a,b
confint(mymodel, level=0.95)
# confidence intervals on a,b

mysummary <- summary(mymodel)
# coefficients,	standard errors, t-stats, F-statistic, R-square
mysummary$residuals
mysummary$coefficients
mysummary$sigma
# s
mysummary$fstatistic
mysummary$cov # or mysummary$cov.unscaled
# (X'X)^(-1)



