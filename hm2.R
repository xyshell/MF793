#
# SOLUTION PROBLEM SET 2
#
# PROBLEM 2
# 

xx<-runif(10000,-2,2)
yy<-runif(10000,xx,2)


# Density smoothes things over. If you don't control the smoothing
# you get non-sensical plots if you don't control it. 
# Play with adjust=1 and use your common sense / build your intuition.
# Here the density is uniform and should fall sharply at 0 and 1. 
# Read the help file and compare:

plot(density(xx,adjust=1))
plot(density(xx,from=-2,to=2,adjust=1))	 
lines(density(xx,adjust=1,cut=0),col="red")	# what does cut=0 do

# 1) from/to and cut do not scale density to integrate to 1 on the domain!
# See how to rescale and the problems with bounds in the graphing data 
# lecture note.

# Due to the smoothing, density introduces possibly
# unrealistic patterns  - see the boundaries.
# For y again not the unrealistic pattern at y = 1
par(mgp=c(1.5,0.5,0))

plot(density(yy,from=-2,to=2,adjust=1),xlab="Y",main="Fig 1: Smoothed density of Y") # Is the mode real?

hist(yy,nclass=100,prob=T,main="")
title("Fig 1: Histogram - 10,000 draws of Y",line=0.5)
denyy<-density(yy,from=-2,to=2,adjust=1)
lines(denyy,col="red",lwd=2);box()

# Try to narrow the bandwidth, what do you see?
# Density produces a spurious mode close to the bound of 2
# This comes from a bias in smoothed kernel estimation at the bound.
# Here the histogram serves as a reality check.

#	3D plots

library(MASS)	# for 
denxy<-kde2d(xx,yy,n=100)
persp(denxy,theta=325,phi=30,xlab="X",ylab="Y",expand=0.8,col=5,ticktype="detailed")
title("Fig 2: Estimate of the joint density of (x,y) - 10,000 draws",
line=0,cex.main=1)

#
mean(yy)

# theoretical p(y)

hist(yy,nclass=100,prob=T,main="Fig 3: Histogram and Analytical Density of y")
lines(sort(yy),0.25*(log(4)-log(2-sort(yy))),col="blue",lwd=2)
box()
# Why do we need to sort the values when we use lines() ?

# theoretical joint density

xx<-seq(-2,1.90,length=100)
yy<-xx
zz<-function(x,y){0.25*(y>x)/(2-x)}
zmat<-outer(xx,yy,zz)
persp(xx,yy,zmat,theta=125,phi=25,xlab="X",ylab="Y",zlab="p(x,y)",
col=8,ticktype="detailed")
title("Analytical Density p(x,y)",line=-0.5)

#########################################
# PROBLEM 3

# we can simulate the t-statistics directly since we know
# that once we standardize each simulated returnsample mean, 
# by its mean under the null and its standard deviation, 
# we have a Student-t with 59 dofs.

top5tiz  <-rep(0,5)
nsim     <- 10000
nfunds   <- 100		
dof      <- 59
alltiz  <- matrix(rt(nfunds*nsim,dof),nrow=nsim)
randomt <- alltiz[,1:5]
top5t   <- apply(alltiz,1,sort,decreasing=T)[1:5,]

murant  <- colMeans(randomt)
prejrant<- apply((randomt>2)*1,2,sum)*100/10000
q975rant<- apply(randomt,2,quantile,0.975)

round(rbind(murant,prejrant,q975rant),3)

mumaxt  <- rowMeans(top5t)
prejmaxt<- apply((top5t > 2)*1,1,sum)*100/10000
q975maxt<- apply(top5t,1,quantile,0.975)

round(rbind(mumaxt,prejmaxt,q975maxt),3)

par(mfrow=c(2,1),mar=c(2,3,1,1),mgp=c(1.5,0.5,0),oma=c(0,0,3,0))
hist(top5t[1,],nclass=100,prob=T,xlab="",main="",xlim=range(top5t),ylim=c(0,2))
title("Highest of 100 Student-t(59)",line=0.2);box()
abline(v=c(2,quantile(top5t[1,],0.975)),col=c(2,4),lwd=2)

hist(top5t[5,],nclass=80,prob=T,xlab="",main="",xlim=range(top5t))
title("Fifth highest of 100 Student-t(59)",line=0.2);box()
abline(v=c(2,quantile(top5t[5,],0.975)),col=c(2,4),lwd=2)
lines(seq(1,3,length=1000),dt(seq(1,3,length=1000),59))

mtext("Figure 3: Distribution of Data-Mined-Student-t under H0",
outer=T,cex=1.2)

hist(randomt,nclass=100,prob=T)
lines(seq(-4,4,length=1000),dt(seq(-4,4,length=1000),59))
lines(density(top5t[5,]))




