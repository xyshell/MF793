#
# HOMEWOR 1
#
# PROBLEM 1
setwd("/Users/xieyou/GitHub/MF793/data")

choose(230,4)		# OK
factorial(365)		# No
factorial(365-226-1)# No
prod((365-226):365)	# No !

logprob<-log(choose(230,4))+lfactorial(365)-lfactorial(365-226)-230*log(365)
exp(logprob)

round(1-exp(lfactorial(365)-lfactorial(365-230)-230*log(365)),5)

#
# PROBLEM 4
#

fundret<- read.csv("funds-1584g-mon.csv",header=T)    
rmrf   <- read.csv("Fama_French_Three_Factors_Monthly.csv",header=T)

rmret<-(rmrf[,2]+rmrf[,3])/100 	
		# add back the Tbill, and scale Rm to match the fund returns data base

#
# Question a)
#

murm1<-mean(rmret[1:48])  
murm2<-mean(rmret[49:96])  

mufu1<-apply(fundret[1:48,2:1585],2,mean)
mufu2<-apply(fundret[49:96,2:1585],2,mean)


hist(mufu1*12,xlab="Mean Return",freq=T,nclass=100,main="")
title("Figure 1: Histogram of 1584 Growth Fund Returns, 2010-13",line=0.2)
box()
abline(v=mean(mufu1)*12,col="red",lwd=2)
abline(v=murm1*12,col="blue",lwd=2)
legend("topleft",c("Avg. Fund","Market"),bty="n",
col=c("red","blue"),lwd=c(1,2),lty=1)

murm1			# Mean market return 
quantile(mufu1)
length(mufu1[mufu1>murm1])/1584	# 0.25

#
# Question b)
#

plot(mufu1,mufu2,xlab="2010-13 return",ylab="2014-17 return")
title("1584 fund returns, second vs first period returns")

axisrange<-range(c(mufu1,mufu2))*12		# total range of mean returns

par(mgp=c(1.5,0.5,0))			# What does this do? Look it up.
plot(mufu1*12,mufu2*12,xlab="2010-13 Mean Return",ylab="2014-17 Mean Return")
# ,xlim=axisrange,ylim=axisrange)		# To have the same range on x and y
title("Figure 2: Fund mean returns, second vs first period returns",line=0.2)
abline(lsfit(mufu1*12,mufu2*12),lty=2,lwd=2)
abline(lsfit(mufu1[mufu1>0]*12,mufu2[mufu1>0]*12),lty=3,lwd=2,col="red")
abline(v=murm1*12);abline(h=murm2*12)

# Similar plot but only with the ranks

rank1<-rank(mufu1)			# See what rank does
rank2<-rank(mufu2)
plot(rank1,rank2,xlab="Period 1 rank",ylab="Period 2 rank")
title("Figure 2b: Funds rank persistence from 10-13 to 14-17",line=0.5)
abline(lsfit(rank1,rank2))

# Two-way counts

brs<-cbind(quantile(mufu1,c(0.2,0.8)),quantile(mufu2,c(0.2,0.8))) 


# many ways to do this, can just count:
cmat<-matrix(0,ncol=3,nrow=3)
cmat[1,1]<-length(mufu1[mufu1<brs[1,1]&mufu2<brs[1,2]])	# Lose Lose
cmat[1,3]<-length(mufu1[mufu1<brs[1,1]&mufu2>brs[2,2]])	# Lose Win
cmat[1,2]<-317-cmat[1,1]-cmat[1,3] # Lose Middle
cmat[3,1]<-length(mufu1[mufu1>brs[2,1]&mufu2<brs[1,2]])
cmat[3,3]<-length(mufu1[mufu1>brs[2,1]&mufu2>brs[2,2]])
cmat[3,2]<-317-cmat[3,1]-cmat[3,3]

for (i in c(1:3)){cmat[2,i]<-317-cmat[1,i]-c11[3,i]}	# middle row
cmat[2,2]<-1584-2*317-cmat[1,2]-cmat[3,2]

# Check what you did !
rowSums(cmat)
colSums(cmat)
sum(cmat)

# The joint prob. table is easy!
probmat<-cmat/1584
probmat
sum(probmat)	# check it makes sense

# So is the conditional table

prob1  <- c(0.2,0.6,0.2)
condmat<- probmat/prob1	# What does this do?
rowSums(condmat)		# check it makes sense:


# question c)

xmufu1 <- mufu1-murm1
xmufu2 <- mufu2-murm2
length(xmufu1[xmufu1>0])
length(xmufu2[xmufu2>0])

# Also this way:

beat1<- xmufu1>0		# make indicator variable based on logical expression
beat2<- xmufu2>0

beatct<-matrix(			# Careful, R fills in matrices by column
c( sum((1-beat1)*(1-beat2)) ,
sum(beat1 * (1-beat2)) ,
sum((1-beat1)*beat2 ) ,
sum(beat1*beat2) ) 
,ncol=2)
beatct

beatprob<-beatct / 1584
beatprob

prob1 <- c(1584-sum(beat1),sum(beat1))/1584
beatcond<-beatprob/prob1
beatcond



# 

# The time series command works only for well-defined
# frequency, weekly data don't work. But monthly data work:

tsrets<-ts(retmat,frequency=12,start=c(1994,7))

# Allows to make time series plots with proper x-axis labels

Rtmin1<-retmat[1:257,2:6]		# t-1 Returns
Rt    <-retmat[2:258,2:6]		# t   Returns

PofU<-rep(0,6)
for (i in 2:6){
	PofU[i]	<-length(retmat[retmat[,i]>0,i])	
}
round(PofU/258,2)

# Question 3)
# conditional p(Rt > 0 | Rt-1 > 0)

i<-5
pair<-cbind(Rtmin1[,i],Rt[,i])

#.  & symbol is "and" in R

length(pair[pair[,1]<0 & pair[,2]<0 ,1])  
length(pair[pair[,1]<0 & pair[,2]>0 ,1])  
length(pair[pair[,1]>0 & pair[,2]<0 ,1])  
length(pair[pair[,1]>0 & pair[,2]>0 ,1])  
length(pair[pair[,1]<0 & pair[,2]<0 ,1])/257  
length(pair[pair[,1]<0 & pair[,2]>0 ,1])/257  
length(pair[pair[,1]>0 & pair[,2]<0 ,1])/257  
length(pair[pair[,1]>0 & pair[,2]>0 ,1])/257  

#
# Problem 5
#
#a)

-1/(0.5*(-1/13000)+0.5*(-1/9000))
BB<-seq(0,25000,length=10000)
CE<--1/(-0.5/((10000+BB)*1.3-BB)-0.5/((10000+BB)*0.9-BB))

BB[CE==max(CE)]			# Borrowing at which CE is maximized
CE[1]					# CE at zero borrowing

plot(BB,CE,xlab="$ Borrowing",ylab="$ CE",type="l")
title("Figure 4: CE vs Borrowing",line=0.2)

# Add points to the graph

pointsY<-cbind(max(CE),CE[4485],CE[9333])
pointsX<-cbind(BB[CE==max(CE)],BB[4485],BB[9333])

points(pointsX,pointsY,pch=19,col=c("blue","red","red"))
text(pointsX,pointsY,c("MAX(CE)","CE=CE[B=$0]","CE=$10K"),pos=c(1,4,2))

# Use abline or segments to show interesting features

abline(h=CE[1],lty=3,col="blue")
segments(0,10000,BB[9333],CE[9333],lty=2,lwd=1,col="red")


