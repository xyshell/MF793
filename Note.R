# One/Many random variables
hist(runif(10000,0,10),freq=F,nclass=40)
nvar <- 5
many <- matrix(runif(10000*nvar,0,10),ncol=nvar)
hist(apply(many,1,mean),freq=F,nclass=40)
