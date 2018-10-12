### problem1:Properties of Stock Returns ###

setwd("C:/Users/47494/GitHub/MF793/data")
daily_rets <- read.csv("stk-day.csv",header=T)
monthly_rets <- read.csv("stk-mon.csv",header=T)

#(a)
log_monthly_rets = log(1 + monthly_rets[,2:12])