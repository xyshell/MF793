### problem1:Properties of Stock Returns ###
library("moments")

# A.Preliminary Work
setwd("C:/Users/47494/GitHub/MF793/data")
daily_rets <- read.csv("stk-day.csv",header=T)
monthly_rets <- read.csv("stk-mon.csv",header=T)

# (a)
log_monthly_rets <- cbind(monthly_rets[,1],log(1 + monthly_rets[,2:13]))
log_daily_rets <- cbind(daily_rets[,1],log(1 + daily_rets[,2:13]))
x <- unlist(log_monthly_rets[,2:12])
y <- unlist(monthly_rets[,2:12])
plot(x,y,xlab="log_return",ylab="return",
     main="return vs log-return")
abline(a=0, b=1)

# (b)
# daily statistics
T <- round(dim(daily_rets)[1] / 8, 2)
daily_mean <- colMeans(log_daily_rets[,2:12])
daily_ann_mean <- T * daily_mean
daily_sd <- vector(mode="numeric",length=length(daily_mean))
daily_ann_sd <- vector(mode="numeric",length=length(daily_ann_mean))
names(daily_sd) <- names(daily_mean)
names(daily_ann_sd) <- names(daily_ann_mean)
for (i in 2:12) {
  daily_sd[i-1] <- sd(log_daily_rets[,i])
  daily_ann_sd[i-1] <- sqrt(T) * daily_sd[i-1]
}
# table 1 

round(daily_mean,4)
round(daily_ann_mean,4)
round(daily_sd,4)
round(daily_ann_sd,3)
daily_acf <- vector(mode="numeric",length=length(daily_mean))
daily_skewness <- vector(mode="numeric",length=length(daily_mean))
daily_kurtosis <- vector(mode="numeric",length=length(daily_mean))
daily_jbtest <- vector(mode="numeric",length=length(daily_mean))

names(daily_acf) <- names(daily_mean)
names(daily_skewness) <- names(daily_mean)
names(daily_kurtosis) <- names(daily_mean)
names(daily_jbtest) <- names(daily_mean)

for (i in 2:12) {
  daily_acf[i-1] <- acf(log_daily_rets[,i])[["acf"]][2]
  daily_skewness[i-1] <- skewness(log_daily_rets[,i])
  daily_kurtosis[i-1] <- kurtosis(log_daily_rets[,i])
  daily_jbtest[i-1] <- jarque.test(log_daily_rets[,i])[["statistic"]]
}
round(daily_acf,4)
round(daily_skewness,2)
round(daily_kurtosis,2)
round(daily_jbtest,2)


round(mean(daily_mean),4)
round(mean(daily_ann_mean),4)
round(mean(daily_sd),4)
round(mean(daily_ann_sd),3)
round(mean(daily_acf),4)
round(mean(daily_skewness),2)
round(mean(daily_kurtosis),2)
round(mean(daily_jbtest),2)

ew_daily_ret <- rowMeans(log_daily_rets[,2:12])
ew_daily_mean <- mean(ew_daily_ret)
ew_daily_ann_mean <- T * ew_daily_mean
ew_daily_sd <- sd(ew_daily_ret)
ew_daily_ann_sd <- sqrt(T) * ew_daily_sd
ew_daily_acf <- acf(ew_daily_ret)[["acf"]][2]
ew_daily_skewness <- skewness(ew_daily_ret)
ew_daily_kurtosis <- kurtosis(ew_daily_ret)
ew_daily_jbtest <- jarque.test(ew_daily_ret)[["statistic"]]

round(ew_daily_mean,4)
round(ew_daily_ann_mean,4)
round(ew_daily_sd,4)
round(ew_daily_ann_sd,3)
round(ew_daily_acf,4)
round(ew_daily_skewness,2)
round(ew_daily_kurtosis,2)
round(ew_daily_jbtest,2)

vw_daily_ret <- log_daily_rets[,13]
vw_daily_mean <- mean(vw_daily_ret)
vw_daily_ann_mean <- T * vw_daily_mean
vw_daily_sd <- sd(vw_daily_ret)
vw_daily_ann_sd <- sqrt(T) * vw_daily_sd
vw_daily_acf <- acf(vw_daily_ret)[["acf"]][2]
vw_daily_skewness <- skewness(vw_daily_ret)
vw_daily_kurtosis <- kurtosis(vw_daily_ret)
vw_daily_jbtest <- jarque.test(vw_daily_ret)[["statistic"]]

round(vw_daily_mean,4)
round(vw_daily_ann_mean,4)
round(vw_daily_sd,4)
round(vw_daily_ann_sd,3)
round(vw_daily_acf,4)
round(vw_daily_skewness,2)
round(vw_daily_kurtosis,2)
round(vw_daily_jbtest,2)

# B.Mean and Variance
# monthly statistics
N = 12
monthly_mean <- colMeans(log_monthly_rets[,2:12])
monthly_ann_mean <- N * monthly_mean
monthly_sd <- vector(mode="numeric",length=length(monthly_mean))
monthly_ann_sd <- vector(mode="numeric",length=length(monthly_ann_mean))
names(monthly_sd) <- names(monthly_mean)
names(monthly_ann_sd) <- names(monthly_ann_mean)
for (i in 2:12) {
  monthly_sd[i-1] <- sd(log_monthly_rets[,i])
  monthly_ann_sd[i-1] <- sqrt(N) * monthly_sd[i-1]
}

# table 2 

round(monthly_mean,4)
round(monthly_ann_mean,4)
round(monthly_sd,4)
round(monthly_ann_sd,3)
monthly_acf <- vector(mode="numeric",length=length(monthly_mean))
monthly_skewness <- vector(mode="numeric",length=length(monthly_mean))
monthly_kurtosis <- vector(mode="numeric",length=length(monthly_mean))
monthly_jbtest <- vector(mode="numeric",length=length(monthly_mean))

names(monthly_acf) <- names(monthly_mean)
names(monthly_skewness) <- names(monthly_mean)
names(monthly_kurtosis) <- names(monthly_mean)
names(monthly_jbtest) <- names(monthly_mean)

for (i in 2:12) {
  monthly_acf[i-1] <- acf(log_monthly_rets[,i])[["acf"]][2]
  monthly_skewness[i-1] <- skewness(log_monthly_rets[,i])
  monthly_kurtosis[i-1] <- kurtosis(log_monthly_rets[,i])
  monthly_jbtest[i-1] <- jarque.test(log_monthly_rets[,i])[["statistic"]]
}
round(monthly_acf,4)
round(monthly_skewness,2)
round(monthly_kurtosis,2)
round(monthly_jbtest,2)


round(mean(monthly_mean),4)
round(mean(monthly_ann_mean),4)
round(mean(monthly_sd),4)
round(mean(monthly_ann_sd),3)
round(mean(monthly_acf),4)
round(mean(monthly_skewness),2)
round(mean(monthly_kurtosis),2)
round(mean(monthly_jbtest),2)

ew_monthly_ret <- rowMeans(log_monthly_rets[,2:12])
ew_monthly_mean <- mean(ew_monthly_ret)
ew_monthly_ann_mean <- N * ew_monthly_mean
ew_monthly_sd <- sd(ew_monthly_ret)
ew_monthly_ann_sd <- sqrt(N) * ew_monthly_sd
ew_monthly_acf <- acf(ew_monthly_ret)[["acf"]][2]
ew_monthly_skewness <- skewness(ew_monthly_ret)
ew_monthly_kurtosis <- kurtosis(ew_monthly_ret)
ew_monthly_jbtest <- jarque.test(ew_monthly_ret)[["statistic"]]

round(ew_monthly_mean,4)
round(ew_monthly_ann_mean,4)
round(ew_monthly_sd,4)
round(ew_monthly_ann_sd,3)
round(ew_monthly_acf,4)
round(ew_monthly_skewness,2)
round(ew_monthly_kurtosis,2)
round(ew_monthly_jbtest,2)

vw_monthly_ret <- log_monthly_rets[,13]
vw_monthly_mean <- mean(vw_monthly_ret)
vw_monthly_ann_mean <- N * vw_monthly_mean
vw_monthly_sd <- sd(vw_monthly_ret)
vw_monthly_ann_sd <- sqrt(N) * vw_monthly_sd
vw_monthly_acf <- acf(vw_monthly_ret)[["acf"]][2]
vw_monthly_skewness <- skewness(vw_monthly_ret)
vw_monthly_kurtosis <- kurtosis(vw_monthly_ret)
vw_monthly_jbtest <- jarque.test(vw_monthly_ret)[["statistic"]]

round(vw_monthly_mean,4)
round(vw_monthly_ann_mean,4)
round(vw_monthly_sd,4)
round(vw_monthly_ann_sd,3)
round(vw_monthly_acf,4)
round(vw_monthly_skewness,2)
round(vw_monthly_kurtosis,2)
round(vw_monthly_jbtest,2)

#C.Precision of Estimates
#(a)
APPL_monthly_ret <- log_monthly_rets[,2]
AMZN_monthly_ret <- log_monthly_rets[,3]
t_monthly_result <- t.test(APPL_monthly_ret, AMZN_monthly_ret)
t_monthly_stat <- t_monthly_result$statistic
round(t_monthly_stat,4)

#(b)
APPL_daily_ret <- log_daily_rets[,2]
AMZN_daily_ret <- log_daily_rets[,3]
t_daily_result <- t.test(APPL_daily_ret, AMZN_daily_ret)
t_daily_stat <- t_daily_result$statistic
round(t_daily_stat,4)

#(c)
NIKE_monthly_ret <- log_monthly_rets[,7]
NIKE_monthly_result <- t.test(NIKE_monthly_ret)
NIKE_daily_ret <- log_daily_rets[,7]
NIKE_daily_result <- t.test(NIKE_daily_ret)
n <- round(dim(daily_rets)[1] / 96, 2)
n * NIKE_daily_result$conf.int

#(d)
monthly_sd <- sd(log_monthly_rets[,7])
monthly_conf_int <- c(0,0)
monthly_conf_int[1] <- monthly_sd-1.96*monthly_sd/sqrt(96)
monthly_conf_int[2] <- monthly_sd+1.96*monthly_sd/sqrt(96)
daily_sd <- sd(log_daily_rets[,7])
daily_conf_int <- c(0,0)
daily_conf_int[1] <- daily_sd-1.96*daily_sd/sqrt(n*96)
daily_conf_int[2] <- daily_sd+1.96*daily_sd/sqrt(n*96)

#D.Normality
par(mfrow=c(2,1))
qqnorm(log_daily_rets[,4],main="Normal Q-Q Plot for BIIB daily returns")
qqline(log_daily_rets[,4])
qqnorm(log_monthly_rets[,4],main="Normal Q-Q Plot for BIIB monthly returns")
qqline(log_monthly_rets[,4])

par(mfrow=c(2,1))
qqnorm(log_daily_rets[,14],main="Normal Q-Q Plot for portfolio daily returns")
qqline(log_daily_rets[,14])
qqnorm(log_monthly_rets[,14],main="Normal Q-Q Plot for portfolio monthly returns")
qqline(log_monthly_rets[,14])

# Problem 2: The Lognormal distribution
# pepsi
mean(monthly_rets[,8]) * 12
sd(monthly_rets[,8]) * sqrt(12)
quantile(monthly_rets[,8], probs=c(0.05,0.95))
quantile(log_monthly_rets[,8], probs=c(0.05,0.95))
# valero
mean(monthly_rets[,11]) * 12
sd(monthly_rets[,11]) * sqrt(12)
quantile(monthly_rets[,11], probs=c(0.05,0.95))
quantile(log_monthly_rets[,11], probs=c(0.05,0.95))


# Problem 4: Correlations and Normality of log-returns
# (1)
daily_matrix <- cor(log_daily_rets[,2:12])
daily_cor <- unlist(daily_matrix[daily_matrix<1])
quantile(daily_cor, prob=c(0,0.25,0.5,0.75,1))
monthly_matrix <- cor(log_monthly_rets[,2:12])
monthly_cor <- unlist(monthly_matrix[monthly_matrix<1])
quantile(monthly_cor,prob=c(0,0.25,0.5,0.75,1))
# (2)
quantile(ew_monthly_ret,0.05)
qnorm(0.05, mean(ew_monthly_ret),sd(ew_monthly_ret))
quantile(ew_daily_ret,0.05)
qnorm(0.05, mean(ew_daily_ret),sd(ew_daily_ret))
