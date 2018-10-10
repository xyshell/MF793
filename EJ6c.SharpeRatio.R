# Understanding functions of random variables
# Mean of the function vs function of the mean etc..
#
# Ratio of uncorrelated Normals
# Is a ratio of normal densities normal?

mu1<- 1; 	sig1<- 1
mu2<- 1;		sig2<- 0.1
nsim<-20000

ratiosim <-rnorm(nsim,mu1,sig1)/rnorm(nsim,mu2,sig2)
mean(ratiosim)

par(mfrow=c(2,1),mar=c(5,3,2,1),mgp=c(1.5,0.5,0))
hist(ratiosim,nclass=100,prob=T);box()
abline(v=mean(ratiosim),col="red")
qqnorm(ratiosim);qqline(ratiosim)

#
# SHARPE RATIO
#

nobs<-12*5; nsim<-20000
mu <- 0.08  # Annual Mean
sig<- 0.20  # Annual Stdev 
Sharpe<-mu/sd; Sharpe

# Are the estimators of the mean and sd independent ?
freq <-12
rets <-matrix(rnorm(nsim*nobs,mu/freq,sig/sqrt(freq)),ncol=nsim)
plot(apply(rets,2,mean),apply(rets,2,sd),xlab="Mean",
			ylab="Std.Dev")


# Simulate the empirical distribution of the 
# Estimator of the Sharpe Ratio. 
# Is it unbiased ?
# How precise is it?
# Is it normally distributed?
# Do higher frequency data help?
# If you observed data infinitely often, how precise could
# the Sharpe ratio estimate be?
 
library(moments)

# Monthly returns, freq=12

nyear<-5; freq<-12; nsim<-20000;	 nu<-40

# rets <-matrix(rnorm(nsim*nobs,mu,sd),ncol=nsim)

nobs<-nyear*freq
rets <- matrix(rt(nsim*nobs,nu),ncol=nsim)*sqrt((nu-2)/nu)*
			sig/sqrt(freq) + mu/freq

Sharpesim<-sqrt(freq)*apply(rets,2,mean)/apply(rets,2,sd)
mean(Sharpesim)
sd(Sharpesim)

hist(Sharpesim,nclass=100,prob=T);box()
#lines(density(Sharpesim,adjust=0.5))
abline(v=Sharpe,lwd=2,col="red")
abline(v=mean(Sharpesim),col="blue",lwd=2)
lines(sort(Sharpesim),
dnorm(sort(Sharpesim),mean(Sharpesim),sd(Sharpesim)),col="blue")

qqnorm(Sharpesim);qqline(Sharpesim)



