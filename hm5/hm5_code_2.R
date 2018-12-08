setwd("C:/Users/47494/GitHub/MF793/data")
daily_rets <- read.csv("stk-day.csv",header=T)
log_daily_rets <- cbind(daily_rets$date,log(1 + daily_rets[,2:13]))
log_daily_rets[,14] <- rowMeans(log_daily_rets[,2:12])
names(log_daily_rets)[c(1,14)] <- c('date','ewret')

APPL <- log_daily_rets[,2]
mkt <- log_daily_rets[,13]
mod1<-lm(APPL~mkt)
mod1res<-rstandard(mod1)
plot(mod1res, mkt, xlab="standardized residuals", ylab="market return",
     main="Standardized residuals vs the market return")
plot(abs(mod1res), mkt, xlab="abs(standardized residuals)", ylab="market return",
     main="Abs(standardized residuals) vs the market return")
# install.packages('forecast',repos='http://cran.us.r-project.org')
library("forecast")
Acf(mod1res)
library("sandwich")
library("lmtest")
coeftest(mod1,vcov=vcov(mod1))
coeftest(mod1,vcov=vcovHC(mod1))
vcov(mod1)[2,2]
vcovHC(mod1)[2,2]
coefci(mod1,vcov.=vcov(mod1),level=0.90)[2,]
coefci(mod1,vcov.=vcovHC(mod1),level=0.90)[2,]