#
#	Illustrating the Inverse Transform method.
#

# 1) Simulate Uniforms
uu<-runif(10000)
# 2) Compute F-1(U) for all these uniforms

xx<-qnorm(uu)
hist(xx,nclass=100,prob=T)
#points(xx,dnorm(xx),col="red")
lines(xx,dnorm(xx),col="red",lwd=2)


#
# Some Commands to illustrate convergence of Estimators 
# CLT with various Student-t Data
# Convergence of other estimators (stdev, kurtosis)
# to a normal distribution?

par(mfrow=c(2,1))

###################################################
# Simulating the sample mean
#	Is is asymptotically normal as per the CLT ? 
#

nobs<-2000; nsimul<-5000
returns<-matrix(rt(nsimul*nobs,df=2),nrow=nobs)

qqnorm(returns[1,],main="The data"); qqline(returns[1,])

means<-apply(returns,2,mean)
qqnorm(means,main="The sample means"); qqline(means)

####################################################
# Simulating the sample standard deviation		 
# In small sample:
# 1) Is it biased ?
# 2) Is its Std.Dev. equal to the theoretical value (sig/sqrt(2T))
# 3) Is it normally distributed?					 

nobs<-1000; nsimul<-10000

sdtrue<-1
rets<-matrix(rnorm(nsimul*nobs,sd=sdtrue),ncol=nsimul)

dof<-30
rets<-matrix(rt(nsimul*nobs,df=dof),ncol=nsimul)
sdtrue<-sqrt(dof/(dof-2))

stds <-apply(rets,2,sd)

mean(stds);		sdtrue				
sd(stds); 		sdtrue/sqrt(2*nobs)

theory<-qnorm(c(0.025,0.975),1,sdtrue/sqrt(2*nobs))
actual<-quantile(stds,c(0.025,0.975))
round(matrix(c(theory,actual),ncol=2,byrow=T,
dimnames=list(c("theory","actual"),c("2.5%","97.5%")))
,3)

qqnorm(stds);qqline(stds)
hist(stds,nclass=100,prob=T);box()
abline(v=theory)
abline(v=actual,col="red")

# Is the asymptotic Confidence Intervals "correct"?
# Do we reject 5% of the time if we use them?

round(
sum(stds>theory[1]&stds<theory[2])/nsimul
,3)

########################################################
# Estimator of Skewness
# Is it consistent? 
# Does its variance equal the asymptotic approximation?
# Is it normal? 
# Do we reject 5% of the time under H0?

library(moments)

nobs<-4000;	nsimul<-10000; dof<-1000
returns<-matrix(rt(nsimul*nobs,dof),ncol=nsimul)

skews  <-apply(returns,2,skewness)
mean(skews)
sd(skews);		sqrt(6/nobs)

hist(skews,prob=T,nclass=100)
qqnorm(skews);qqline(skews)

theo<-qnorm(c(0.025,0.975),0,sqrt(6/nobs))
actu<-quantile(skews,c(0.025,0.975))
round(matrix(c(theo,actu),ncol=2,byrow=T,
dimnames=list(c("theory","actual"),c("2.5%","97.5%"))),3)

# Should reject 5% of the time under the null
sum(skews>theo[1]&skews<theo[2])/nsimul	

########################################################
# Estimator of the kurtosis
# Is it consistent? Is it normal? 
#
# Do we reject 5% of the time under H0?

library(moments)

nobs<-1000;	nsimul<-10000; dof<-60
#returns<-matrix(rnorm(nsimul*nobs),ncol=nsimul)
returns<-matrix(rt(nsimul*nobs,dof),ncol=nsimul)

kurts  <-apply(returns,2,kurtosis)
mean(kurts)
sd(kurts);		sqrt(24/nobs)

hist(kurts,prob=T,nclass=50)
qqnorm(kurts);qqline(kurts)

theo<-qnorm(0.95,3,sqrt(24/nobs))
actu<-quantile(kurts,0.95)
round(c(theo,actu),3)

sum(kurts>theo)/nsimul	# Should reject 5% of the time
						# under the null
