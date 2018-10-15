                     # functions
# Factorial: Γ(n)=Factorial(n-1)
gamma()
lgamma()
# Descriptive stats
mean(ret)
sd(ret) #	standard deviation
var(ret) # variance
cor(ret1,ret2) # correlation
quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7, ...)
skewness(ret) # skewness library("moments")
kurtosis(ret) # kurtosis library("moments")
jarque.test(ret)[["statistic"]] # jbtest library("moments")
acf(ret, lag.max = 1)
                     # datault
setwd("C:/x/xxx/xxx")
read.csv("xxx.csv")
write.csv(array,"xxx.csv")

                     # vector & matrix
# initialize
vec <- vector(mode="numeric",length=n)

# subsetting/combining
matrix[101:200, c(2,4)]
matrix[matrix[,1] > 20171231 & matrix[,1]>0, ]
cbind(mat1, mat2)
rbind(mat1, mat2)
c(vec1, vec2)
length(vec)
dim(matrix)
# calculation
prod(1+ret)-1

                     # data.frame
# data.frame to 1-dimension vector
vec = unlist(df)

                     # visulization
plot(x, y, xlab="", xlim=c(low,high), main="")
hist(x, nclass=40, prob=T)

points(x0, y0, col="red", pch=16, cex=2)
abline(v=2, col="black", lwd=3) # vertical
abline(h=2) # horizontal
abline(a=1, b=2) # intercept a, slope b
abline(lsfit(x,y)) # regression	line to scatter

                     # syntax
for (i in 4:10) {}

# One/Many random variables
hist(runif(10000,0,10),freq=F,nclass=40)
nvar <- 5
many <- matrix(runif(10000*nvar,0,10),ncol=nvar)
hist(apply(many,1,mean),freq=F,nclass=40)
