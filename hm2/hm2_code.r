### ---problem 2---
#(a)
x <- runif(10000, min=-2, max=2)
y <- runif(10000,x,2)
plot(density(y), xlab="y", main="marginal univariate density of y")
library("MASS")
persp(kde2d(x, y),theta = 15, ticktype = "detailed",
      xlab = "X", ylab = "Y", main="bivariate density of (X,Y)")
#(b)
mean(y)

### ---problem 4---
# calculate std through 1584funds
tbl_funds <- read.csv('funds-1584g-mon.csv')
std <- sapply(tbl_funds[,2:ncol(tbl_funds)], sd)
avg_std <- mean(std)

# simulate normal random variables

tbl_tstat <- matrix(0,nrow=1,ncol=100)
tbl_high_tstat <- matrix(0,nrow=10000,ncol=5)
tbl_rand_tstat <- matrix(0,nrow=10000,ncol=5)
for (j in 1:10000){
  tbl_norm <- rnorm(6000,0.01,avg_std)
  dim(tbl_norm) <- c(60,100)
  for (i in 1:100) {
    tbl_tstat[i] <- t.test(tbl_norm[,i], mu=0.01, conf.level = 0.95)$statistic
  }
  high_tstat <- sort(abs(tbl_tstat),decreasing = TRUE)[1:5]
  rand_tstat <- sample(tbl_tstat)[1:5]
  tbl_high_tstat[j,] <- high_tstat
  tbl_rand_tstat[j,] <- rand_tstat
}

# info required for Table1
round(colMeans(tbl_high_tstat),4)
round(colMeans(tbl_rand_tstat),4)
round(colSums(abs(tbl_high_tstat)>=2)/nrow(tbl_high_tstat),3)
round(colSums(abs(tbl_rand_tstat)>=2)/nrow(tbl_rand_tstat),3)
quantile_high <- matrix(0,nrow=1,ncol=5)
quantile_rand <- matrix(0,nrow=1,ncol=5)
for (i in 1:5) {quantile_high[i] <- round(quantile(tbl_high_tstat[,i], 0.975),4)}
for (i in 1:5) {quantile_rand[i] <- round(quantile(tbl_rand_tstat[,i], 0.975),4)}

# plot figure3
hist(tbl_high_tstat[,5], nclass=150, 
     xlab="the fifth highest t-statistic",
     main="histogram of the fifth highest t-statistic")	
abline(v=2, col="black",lwd=3)
abline(v=quantile_high[5], col="black",lwd=3)


