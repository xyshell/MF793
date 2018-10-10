# GRAPHING DISTRIBUTION
# HISTOGRAM / DENSITY PLOT / NORMAL PROBABILITY PLOT
# Some commands to use on Monday Oct. 3rd 
#

rets<-rnorm(10000,0,1)
hist(rets,prob=T,nclass=80,xlim=c(-4,4));box()

# pick numbers in the range and plot the normal
# density on top of the histogram

xx<-seq(-4,4,by=0.01);   lines(xx,dnorm(xx),lwd=2)

# Alternative to the histogram: The density command
# like a continuous histogram - an empirical pdf
# important subcommand:   adjust=1

plot(density(rets ,adjust=1,cut=0))
lines(xx,dnorm(xx),col="red")

hist(rets,prob=T,nclass=80,xlim=c(-4,4));box()
lines(density(rets ,adjust=1,cut=0),lwd=2)

#
# IMPORTANCE of cut, or from, to: density does not know
# the RV may have bounds.
#
df<-2
xx<-rchisq(100,df)
plot(density(xx),ylim=c(0,0.5))				# cut=0,ylim=c(0,0.5)
lines(density(xx,from=0),col="blue",lwd=3)
lines(density(xx,cut=0),col="blue",lwd=3)
#abline(v=0,lty=2)
lines(seq(0,10,length=200),dchisq(seq(0,10,length=200),df),col="red",lwd=2)

# Density smoothes things over. If you don't control the smoothing
# you may get non-sensical plots. 
# Play with adjust=1 and use your common sense / build your intuition.
# Here the density is uniform and should fall sharply at 0 and 1. 
# Read the help file and compare:
xx<-runif(100)
plot(density(xx,adjust=1))
plot(density(xx,from=0,to=1,adjust=1))	 
lines(density(xx,adjust=1,cut=0),col="red")	# what does cut=0 do

# 1) from/to and cut do not scale density to integrate to 1 on the domain!
# Easy to rescale,:
xx<-rchisq(1000,df)
denx <-density(xx,cut=0)
step <-diff(denx$x)[1]			# What does diff do?
error<-step* sum(density(xx)$y)	# Integrate
lines(denx$x,denx$y/error,col="blue",lwd=2)

# But this easy rescaling does not remove spurious mode
# The real problem comes from a bias in kernel estimation at the bound.

# Bound corrected kernel density plot in package: evmix
# bckden:
# Estimates density at user given points using sample data
# Only assumes a lower bound at 0 
# To use, one must shift sample data so lower bound is at zero.

library(evmix)
hist(xx,nclass=100,prob=T,main="Fig 1: Histogram - 100,000 draws of Y")
plot(denx,ylim=c(0,0.5))
lines(seq(0,15,by=0.01),dbckden(seq(0,15,by=0.01),xx,bw=denx$bw),lwd=2)











#
# we can plot the normal density for
# the simulated numbers themselves
# instead of a separate sequence of numbers

rets<-rnorm(500)

hist(rets,prob=T,nclass=40,xlim=c(-4,4));box()
lines(sort(rets),dnorm(sort(rets)))

# What if the data are Student-t
rets<-rt(1000,10)*sqrt(8/10)		# Make simulated data variance 1
hist(rets,prob=T,nclass=50);box()
lines(sort(rets),dnorm(sort(rets)))

# Graphing tool:
# Normal Probability Plot: qqnorm and qqline
#
rets<-rt(1000,10) 
hist(rets,prob=T,nclass=40)
lines(sort(rets),dnorm(sort(rets),mean(rets),sd(rets)))

qqnorm(rets) 
qqline(rets,lwd=2) 

#
# Simulation experiment 
# Simulating the Sample mean of a sample of nobs returns
# Function faster than a loop

nobs <-200; nsimul<-5000
avgsd<-function(xlen,ylen){
	simdat<-matrix(rt(xlen*ylen,100),nrow=xlen)
	sd(apply(simdat,2,mean))
}
avgsd(200,50000)

nobs <-60; nsimul<-10000
mystocks<-matrix(rt(nsimul*nobs,100),nrow=nobs,)
themeans<-apply(mystocks,2,mean)
thesds  <-apply(mystocks,2,sd)
qqnorm(themeans); qqline(themeans)

qqnorm(thesds); qqline(thesds)

#
# APPLY TO ACTUAL MONTHLY RETURNS
#

rets<-read.csv("countries-mon.csv",header=T)

qqnorm(rets[,6]); qqline(rets[,6])
hist(rets[,6])
plot(density(rets[,6],adjust=1.5))
hist(rets[rets[,2]<1,2],nclass=30)

xx<-seq(-0.3,0.2,length=200)
lines(xx,dnorm(xx,mean=median(rets[,6]),sd=sd(rets[,6])),col="red")

ts.plot(ts(rets[,2],freq=12,start=c(1994,7)))

################################## 
# 
# BIVARIATE DATA: correlation, contour, density
#

mux<-0; alphay<-0.5; rho <-0.25 
xx   <- rnorm(2000)
yy	  <- alphay + rho * xx + sqrt(1-rho^2) * rnorm(2000)
xydat<-data.frame(xx,yy)
apply(xydat,2,mean);apply(xydat,2,sd)
cov(xydat)
cor(xydat)

plot(xx,yy)
abline(lsfit(xx,yy))

#
# 3D PLOTS: commands contour and persp
# See also persp3d in package rgl

# Example, plotting a function of (x,y)

# Need a grid of values for (x,y)
xx  <-seq(-3.5,3.5,length=200);   yy<-xx
#zmat<- matrix(1,ncol=200,nrow=200)

ff  <-function(x,y){
	(1/sqrt(2*pi*(1-0.36)))*exp(-(x^2+y^2-2*0.6*x*y)/2)}
zmat<-outer(xx,yy,ff)

# Why can't we just use this:
# zz<-(1/sqrt(2*pi))*exp(-(xx^2+yy^2)/2) ?

contour(xx,yy,zmat,nlevels=40)
persp(xx,yy,zmat, theta=40,phi=15,zlab="bivariate normal pdf",
ticktype="detailed",col="green3",lwd=0.5)

# THAT WAS TOO EASY !
# Often we don't have zmat! we have pairs of (x,y) data.
# we first need to transform them into a density on a grid,
# a bivariate histogram or density
# Then only we can plot using persp and contour
#
library(MASS)

mux<-0; muy<-0.5; rho <-0.5 
xx  <- rnorm(20000)
yy	<- muy + rho * (xx - mux) + sqrt(1-rho^2) * rnorm(20000)

bivden<-kde2d(xx,yy,n=60,lims=c(-3,3,-2.5,3.5))

contour(bivden,nlevels=40,xlab="x",ylab="y")
persp(bivden, theta=20,phi=15,col="pink",xlab="x",ylab="y",ticktype="detailed")


#
# Rotating objects in R
# You also need the package xquartz for Mac OS (free download)
# 1) make sure you to load the free xquartz app on your mac
#	It is just the new name of the old Unix X11 windowing system 
#	which used to be on every MAC.
# 2) Outside of R, start xquartz. It will open an X11 window. Just
#	 ignore it. Then go to R and do:

library(rgl)
persp3d(bivden,  xlab="x",ylab="y")
,col=terrain.colors)

# You can put your cursor on the 3d plot and rotate it around
# 
#




