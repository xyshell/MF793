setwd("C:/Users/47494/GitHub/MF793/data")
monthly_rets <- read.csv("stk-mon.csv",header=T)
daily_rets <- read.csv("stk-day.csv",header=T)
log_monthly_rets <- cbind(monthly_rets$date,log(1 + monthly_rets[,2:13]))
log_monthly_rets[,14] <- rowMeans(log_monthly_rets[,2:12])
names(log_monthly_rets)[c(1,14)] <- c('date','ewret')
log_daily_rets <- cbind(daily_rets$date,log(1 + daily_rets[,2:13]))
log_daily_rets[,14] <- rowMeans(log_daily_rets[,2:12])
names(log_daily_rets)[c(1,14)] <- c('date','ewret')

# monthly regression
y_monthly <- log_monthly_rets[,13]
res_monthly_itcpt <- vector(mode="numeric",length=12)
res_monthly_x <- vector(mode="numeric",length=12)
res_monthly_itcpt_s <- vector(mode="numeric",length=12)
res_monthly_x_s <- vector(mode="numeric",length=12)
res_monthly_itcpt_t <- vector(mode="numeric",length=12)
res_monthly_x_t <- vector(mode="numeric",length=12)
res_monthly_resi <- matrix(0,96,11)
names(res_monthly_itcpt) <- names(log_monthly_rets[,c(2:12,14)])
names(res_monthly_x) <- names(log_monthly_rets[,c(2:12,14)])
names(res_monthly_itcpt_s) <- names(log_monthly_rets[,c(2:12,14)])
names(res_monthly_x_s) <- names(log_monthly_rets[,c(2:12,14)])
names(res_monthly_itcpt_t) <- names(log_monthly_rets[,c(2:12,14)])
names(res_monthly_x_t) <- names(log_monthly_rets[,c(2:12,14)])
j = 1
for (i in c(2:12,14)) {
  res_monthly <- lsfit(log_monthly_rets[,i], y_monthly)
  res_monthly_itcpt[j] <- coef(res_monthly)[1]
  res_monthly_x[j] <- coef(res_monthly)[2]
  res_monthly_itcpt_s[j] <- ls.print(res_monthly)[2][["coef.table"]][[1]][,2][1]
  res_monthly_x_s[j] <- ls.print(res_monthly)[2][["coef.table"]][[1]][,2][2]
  res_monthly_itcpt_t[j] <- ls.print(res_monthly)[2][["coef.table"]][[1]][,3][1]
  res_monthly_x_t[j] <- ls.print(res_monthly)[2][["coef.table"]][[1]][,3][2]
  res_monthly_resi[,j] <- residuals(res_monthly)
  j = j + 1
}
round(res_monthly_itcpt,4)
round(res_monthly_x,2)
round(res_monthly_itcpt_s,4)
round(res_monthly_x_s,2)

# daily regression
y_daily <- log_daily_rets[,13]
res_daily_itcpt <- vector(mode="numeric",length=12)
res_daily_x <- vector(mode="numeric",length=12)
res_daily_itcpt_s <- vector(mode="numeric",length=12)
res_daily_x_s <- vector(mode="numeric",length=12)
res_daily_itcpt_t <- vector(mode="numeric",length=12)
res_daily_x_t <- vector(mode="numeric",length=12)
names(res_daily_itcpt) <- names(log_daily_rets[,c(2:12,14)])
names(res_daily_x) <- names(log_daily_rets[,c(2:12,14)])
names(res_daily_itcpt_s) <- names(log_daily_rets[,c(2:12,14)])
names(res_daily_x_s) <- names(log_daily_rets[,c(2:12,14)])
names(res_daily_itcpt_t) <- names(log_daily_rets[,c(2:12,14)])
names(res_daily_x_t) <- names(log_daily_rets[,c(2:12,14)])

j = 1
for (i in c(2:12,14)) {
  res_daily <- lsfit(log_daily_rets[,i], y_daily)
  res_daily_itcpt[j] <- coef(res_daily)[1]
  res_daily_x[j] <- coef(res_daily)[2]
  res_daily_itcpt_s[j] <- ls.print(res_daily)[2][["coef.table"]][[1]][,2][1]
  res_daily_x_s[j] <- ls.print(res_daily)[2][["coef.table"]][[1]][,2][2]
  res_daily_itcpt_t[j] <- ls.print(res_daily)[2][["coef.table"]][[1]][,3][1]
  res_daily_x_t[j] <- ls.print(res_daily)[2][["coef.table"]][[1]][,3][2]
  j = j + 1
}
round(res_daily_itcpt,5)
round(res_daily_x,2)
round(res_daily_itcpt_s,5)
round(res_daily_x_s,2)

j = 1
for (i in c(2:12)) {
  res_monthly <- lsfit(log_monthly_rets[,i], y_monthly)
  res_monthly_resi[,j] <- residuals(res_monthly)
  j = j + 1
}
cor_monthly_resi <- cor(res_monthly_resi)
(sum(cor_monthly_resi) - 11)/(11*11-11)