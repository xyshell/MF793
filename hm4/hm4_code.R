# problem 1
setwd("C:/Users/47494/GitHub/MF793/data")
monthly_rets <- read.csv("stk-mon.csv",header=T)
daily_rets <- read.csv("stk-day.csv",header=T)
# (a)
log_monthly_rets <- cbind(monthly_rets$date,log(1 + monthly_rets[,2:13]))
log_monthly_rets[,14] <- rowMeans(log_monthly_rets[,2:12])
names(log_monthly_rets)[c(1,14)] <- c('date','ewret')
log_monthly_rets_a <- log_monthly_rets[log_monthly_rets$date<='20151231',]
log_monthly_rets_b <- log_monthly_rets[log_monthly_rets$date>'20151231',]
monthly_sd_a <- apply(log_monthly_rets_a[,2:14], 2, sd)
monthly_sd_b <- apply(log_monthly_rets_b[,2:14], 2, sd)
ann_monthly_sd_a <- monthly_sd_a * sqrt(12)
ann_monthly_sd_b <- monthly_sd_b * sqrt(12)
round(ann_monthly_sd_a, 3)
round(ann_monthly_sd_b, 3)
monthly_vr <- ann_monthly_sd_a^2 / ann_monthly_sd_b^2
round(monthly_vr, 3)
# (b)
montly_cutoffs <- qf(c(0.05, 0.95), 71, 23)
round(montly_cutoffs, 3)
barplot(monthly_vr, main="barplot of VRs", ylim=c(0,3))
abline(h=1,lwd=3)
abline(h=montly_cutoffs[2],lwd=3)
round(sqrt(1/monthly_vr[12]) - 1, 3) 
# (c)
log_daily_rets <- cbind(daily_rets$date,log(1 + daily_rets[,2:13]))
log_daily_rets[,14] <- rowMeans(log_daily_rets[,2:12])
names(log_daily_rets)[c(1,14)] <- c('date','ewret')
log_daily_rets_a <- log_daily_rets[log_daily_rets$date<='20151231',]
log_daily_rets_b <- log_daily_rets[log_daily_rets$date>'20151231',]
daily_sd_a <- apply(log_daily_rets_a[,2:14], 2, sd)
daily_sd_b <- apply(log_daily_rets_b[,2:14], 2, sd)
ann_daily_sd_a <- daily_sd_a * sqrt(252)
ann_daily_sd_b <- daily_sd_b * sqrt(252)
round(ann_daily_sd_a, 3)
round(ann_daily_sd_b, 3)
daily_vr <- ann_daily_sd_a^2 / ann_daily_sd_b^2
round(daily_vr, 3)
daily_cutoffs <- qf(c(0.05, 0.95), 1509, 502)
round(daily_cutoffs, 3)
# (d)
vw_monthly_mean <- mean(log_monthly_rets[,13])
vw_monthly_sd <- sd(log_monthly_rets[,13])
sim_monthly_ret <- matrix(
  rnorm(96*20000, mean=vw_monthly_mean, sd=vw_monthly_sd),
  ncol=20000)
sim_monthly_ret_a <- sim_monthly_ret[1:72,]
sim_monthly_ret_b <- sim_monthly_ret[73:96,]
sim_monthly_sd_a <-apply(sim_monthly_ret_a, 2, sd)
sim_monthly_sd_b <-apply(sim_monthly_ret_b, 2, sd)
sim_monthly_vr <- sim_monthly_sd_a^2 / sim_monthly_sd_b^2
round(quantile(sim_monthly_vr,0.95), 3)
round(mean(sim_monthly_vr),3)
monthly_frac <- sum(sim_monthly_vr > qf(0.95, 71, 23))/length(sim_monthly_vr)

sim_daily_ret <- matrix(rt(2016*20000,6),ncol=20000)
sim_daily_ret_a <- sim_daily_ret[1:1512,]
sim_daily_ret_b <- sim_daily_ret[1513:2016,]
sim_daily_sd_a <-apply(sim_daily_ret_a, 2, sd)
sim_daily_sd_b <-apply(sim_daily_ret_b, 2, sd)
sim_daily_vr <- sim_daily_sd_a^2 / sim_daily_sd_b^2
round(quantile(sim_daily_vr,0.95), 3)
round(mean(sim_daily_vr),3)
daily_frac <- sum(sim_daily_vr > qf(0.95, 1511, 503))/length(sim_daily_vr)

qqplot(qf(ppoints(20000),71,23),sim_monthly_vr,
  main="F-probability plot of VRs against theoretical F\nmonthly case",
  xlab="theoretical quantiles", ylab="sample quantiles")
abline(0,1)
qqplot(qf(ppoints(20000),1511,503),sim_daily_vr,
       main="F-probability plot of VRs against theoretical F\ndaily case",
       xlab="theoretical quantiles", ylab="sample quantiles")
abline(0,1)

# problem 2
# (a)
C_mothly_ret <- log_monthly_rets[,5]
STT_mothly_ret <- log_monthly_rets[,9]
C_mothly_sd <- sd(C_mothly_ret)
STT_mothly_sd <- sd(STT_mothly_ret)
monthly_vr_CS <- C_mothly_sd^2 / STT_mothly_sd^2
round(monthly_vr_CS,3)
round(qf(c(0.05,0.95),95,95),3)
# (b)
sim_monthly_ret2_a <- matrix(
  rnorm(96*20000),ncol=20000)
sim_monthly_ret2_b <- 0.3*sim_monthly_ret2_a + 
  sqrt(1-0.3^2)*matrix(rnorm(96*20000),ncol=20000)
sim_monthly_sd2_a <- apply(sim_monthly_ret2_a, 2, sd)
sim_monthly_sd2_b <- apply(sim_monthly_ret2_b, 2, sd)
sim_monthly_vr2 <- sim_monthly_sd2_a^2 / sim_monthly_sd2_b^2
round(quantile(sim_monthly_vr2, 0.95), 3)
round(mean(sim_monthly_vr2),3)
monthly_frac2 <- sum(sim_monthly_vr2 > qf(0.95, 95, 95))/length(sim_monthly_vr2)

rho <- cor(C_mothly_ret, STT_mothly_ret)
sim_monthly_ret3_a <- matrix(
  rnorm(96*20000),ncol=20000)
sim_monthly_ret3_b <- rho*sim_monthly_ret3_a + 
  sqrt(1-rho^2)*matrix(rnorm(96*20000),ncol=20000)
sim_monthly_sd3_a <- apply(sim_monthly_ret3_a, 2, sd)
sim_monthly_sd3_b <- apply(sim_monthly_ret3_b, 2, sd)
sim_monthly_vr3 <- sim_monthly_sd3_a^2 / sim_monthly_sd3_b^2
round(quantile(sim_monthly_vr3, 0.95), 3)
round(mean(sim_monthly_vr3),3)
monthly_frac3 <- sum(sim_monthly_vr3 > qf(0.95, 95, 95))/length(sim_monthly_vr3)
qqplot(qf(ppoints(20000),95,95),sim_monthly_vr3,
       main="F-probability plot of VRs against theoretical F\nC / STT monthly case",
       xlab="theoretical quantiles", ylab="sample quantiles")
abline(0,1)
qqplot(qt(ppoints(20000),95,95),sim_monthly_vr3,
       main="t-probability plot of VRs with\ndegree of freedom=95\nnon-centrality parameter=95",
       xlab="theoretical quantiles", ylab="sample quantiles")
