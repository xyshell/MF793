# problem 1
setwd("C:/Users/47494/GitHub/MF793/data")
monthly_rets <- read.csv("stk-mon.csv",header=T)
daily_rets <- read.csv("stk-day.csv",header=T)
log_monthly_rets <- cbind(monthly_rets$date,log(1 + monthly_rets[,2:13]))
names(log_monthly_rets)[c(1,14)] <- c('date','ewret')
log_daily_rets <- cbind(daily_rets$date,log(1 + daily_rets[,2:13]))
names(log_daily_rets)[c(1,14)] <- c('date','ewret')

ticker_list <- c("AAPL", "BIIB", "PEP", "VZ", "vwret")
sub_daily_rets <- log_daily_rets[ticker_list]
sub_monthly_rets <- log_monthly_rets[ticker_list]

daily_ret_sd <- apply(sub_daily_rets, 2, sd)
monthly_ret_sd <- apply(sub_monthly_rets, 2, sd)
daily_ret_sd2 <- sqrt(colMeans(sub_daily_rets^2))
monthly_ret_sd2 <- sqrt(colMeans(sub_monthly_rets^2))
round(daily_ret_sd, 3)
round(daily_ret_sd2, 3)
round(monthly_ret_sd, 3)
round(monthly_ret_sd2, 3)

# problem 3
vix <- read.csv("vix-mon.csv",header=T)
vix$Date <- format(vix$Date, format="%m/%d/%y")
sub_vix <- vix[vix$Date >= 2007.01,]

library(forecast)
library(tseries)
par(mfrow=c(3,1),mgp=c(1.5,0.5,0),mar=c(3,3,2,0.5)) 
ts.plot(ts(sub_vix[,2],frequency=12,start=c(2007,1)),
        ylab="VIX",xlab="Day")
title("Time Series of VIX")
Acf(sub_vix[,2]);title("ACF of VIX")
Acf(sub_vix[,2],type="partial");title("PACF of VIX")

mod <- arma(sub_vix[,2],order=c(1,0))
names(mod)
residuals(mod)
fitted.values(mod)

valid_resi <- residuals(mod)[2:length(residuals(mod))]
valid_fit_val <- fitted.values(mod)[2:length(fitted.values(mod))]
abs_resi <- abs(valid_resi)
fit_val <- valid_fit_val

par(mfrow=c(2,1))
plot(fit_val, abs_resi, 
    main='Abs(residuals) vs Fitted Value')
abline(lsfit(fit_val, abs_resi))
qqnorm(residuals(mod),main="Normal Q-Q Plot for Residuals")
qqline(residuals(mod))
library("moments")
skew_resi <- skewness(valid_resi)

par(mfrow=c(2,1))
qqnorm(sub_vix[,2],main="Normal Q-Q Plot for VIX")
qqline(sub_vix[,2])
qqnorm(log(sub_vix[,2]),main="Normal Q-Q Plot for logVIX")
qqline(log(sub_vix[,2]))
skew_vix <- skewness(sub_vix[,2])
skew_logvix <- skewness(log(sub_vix[,2]))

mod2 <- arma(log(sub_vix[,2]),order=c(1,0))
valid_resi2 <- residuals(mod2)[2:length(residuals(mod2))]
valid_fit_val2 <- fitted.values(mod2)[2:length(fitted.values(mod2))]
abs_resi2 <- abs(valid_resi2)
fit_val2 <- valid_fit_val2

par(mfrow=c(2,1))
plot(fit_val2, abs_resi2, 
     main='Abs(residuals) vs Fitted Value')
abline(lsfit(fit_val2, abs_resi2))
qqnorm(residuals(mod2),main="Normal Q-Q Plot for Residuals")
qqline(residuals(mod2))
skew_resi2 <- skewness(valid_resi2)

sd(valid_resi)
sd(valid_resi2)
bound <- quantile(valid_resi, c(0.25,0.75))
bound2 <- quantile(valid_resi2, c(0.25,0.75))
alpha <- coef(mod)[2]
beta <- coef(mod)[1]
alpha2 <- coef(mod2)[2]
beta2 <- coef(mod2)[1]
LB <- alpha + beta * tail(sub_vix[,2],1) + bound[1]
UB <- alpha + beta * tail(sub_vix[,2],1) + bound[2]
LB2 <- exp(alpha2 + beta2 * tail(log(sub_vix[,2]),1) + bound2[1])
UB2 <- exp(alpha2 + beta2 * tail(log(sub_vix[,2]),1) + bound2[2])

# problem 4 
poundeuro <- read.csv("poundeuro-day.csv",header=T)
names(poundeuro) <- c("date","pound","euro")
poundeuro$date <- format(poundeuro$date, format="%y%m%d")
pound <- poundeuro[poundeuro$date>=2012.01 & poundeuro$date<2018.01,1:2]
Acf(pound[,2]);title("ACF of Pound")

pound_logret <- diff(log(pound[,2]))
library(zoo)
roll_pound_sd <- rollapply(pound_logret,88,sd)
roll_pound_sd2 <- sqrt(rollapply(pound_logret^2,88,mean))
annu_roll_pound_sd <- roll_pound_sd * sqrt(252)
annu_roll_pound_sd2 <- roll_pound_sd2 * sqrt(252)
par(mfrow=c(3,1))
ts.plot(ts(pound[88:nrow(pound),2],frequency=252,start=c(2012,5,7)),
     main ='Pound Price',xlab='day',ylab='Price')
ts.plot(ts(pound_logret[88:length(pound_logret)],frequency=252,
           start=c(2012,5,7)), main ='Pound Logret',xlab='day',ylab='ret')
ts.plot(ts(roll_pound_sd,frequency=252,start=c(2012,5,7)),
        ts(roll_pound_sd2,frequency=252,start=c(2012,5,7)),
        col=c("red","black"),main ='Rolling Sd of Pound',xlab='day',ylab='sd')

lam <- 0.96
rm=array(dim=length(roll_pound_sd2))
rm[1] <- roll_pound_sd2[1]^2
for (i in 2:length(rm)){
  rm[i]=lam * rm[i-1]  + (1-lam) * pound_logret[i+87]^2
}
annu_rm <- sqrt(rm) * sqrt(252)
par(mfrow=c(1,1))
ts.plot(ts(annu_rm,frequency=252,start=c(2012,5,7)),
        ts(annu_roll_pound_sd2,frequency=252,start=c(2012,5,7)),
        col=c("black","red"),main ='RW and RM',xlab='day',ylab='sd')