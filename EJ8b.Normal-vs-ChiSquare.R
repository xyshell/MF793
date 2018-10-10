#
# Chi-square vs its normal approximation
# Effect on distribution of estimator of Std.Dev.


par(mfrow=c(1,1))

dof<-100

# Find range for values of x
lb<-max(min(qchisq(0.001,dof),qnorm(0.001,dof,sqrt(2*dof))),0)
ub<-max(qchisq(0.999,dof),qnorm(0.999,dof,sqrt(2*dof)))
xvals<-seq(lb,ub,by=0.1)

plot(xvals,dchisq(xvals,dof),col="blue",type="l",lwd=2,
	ylab="Density",xlab="")
lines(xvals,dnorm(xvals,dof,sqrt(2*dof)),lwd=2,col="red")
title("Chi-square vs Normal(nu,2nu)")
legend("topright",c("normal",paste("nu=",dof)),
	col=c("red","blue"),lwd=c(2,2),bty="n")
abline(v=c(dof,qchisq(c(0.05,0.95),dof)))

#########################################################
#	Confidence interval for the standard deviation
#	Chi-square vs Normal approximation

# 95% confidence interval:
# s^2 ~ sig^2 x chisq(60) / nu

# We obtain an estimate sigma = 0.2, with nobs observations
# Confidence interval for s using exact or asymptotic ?

nobs <-20;		dof<-nobs-1

# Exact distribution of s assuming normal returns 
# s ~ sig sqrt(chisq(dof)/dof) 
# confidence interval with s=0.2 

0.2*sqrt(dof/qchisq(c(0.975,0.025),dof))

# large sample, approximation with s=0.2

qnorm(c(0.025,0.975),0.2,0.2/sqrt(2*nobs))	

# Maybe the large sample approximation is wrong 
# But the small sample distribution requires normal returns.
# If returns are very fat tailed, it may be wrong too?
# Then everything is wrong! Statistics is fun

sig <-0.20
nyear<-5; freq<-252; nsim<-10000;	nu<-10
nobs <- nyear*freq
rets <- matrix(rt(nsim*nobs,nu),ncol=nsim)*sqrt((nu-2)/nu)*
			sig/sqrt(freq)  
stdevs <-apply(rets,2,sd)*sqrt(freq)

quantile(stdevs,c(0.025,0.975))		# Simulated

qqnorm(stdevs);qqline(stdevs)

par(mfrow=c(2,1))
hist(stdevs,nclass=100,prob=T,main="");box()
title("Sample Std. Dev: Checking Asymptotic Normal Approximation")
lines(sort(stdevs),dnorm(sort(stdevs),sig,sig/sqrt(2*nobs)),
	col="blue",lwd=2)
abline(v=0.2,lwd=3,col="blue")

hist((nobs-1)*(stdevs/sig)^2,nclass=100,prob=T,main="");box()
abline(v=(nobs-1),col="red",lwd=2)
xvals<-sort( (nobs-1)*(stdevs/sig)^2    )
lines(xvals,dchisq(xvals,(nobs-1)),col="red",lwd=2)
title("Sample Variance: Checking Chi-square with fat-tailed Returns")

