                    # functions
# Factorial: Γ(n)=Factorial(n-1)
gamma()
lgamma()
# Descriptive stats
mean(ret)
sd(ret) #	standard deviation
var(ret) # variance
cor(ret1,ret2) # correlation

                    # datault
setwd("C:/x/xxx/xxx")
read.csv("xxx.csv")
write.csv(array,"xxx.csv")

                    # vector & matrix
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

                    # visulization
plot(x, y, xlab="", xlim=c(low,high), main="")
points(x0, y0, col="red", pch=16, cex=2)
abline(v=2, col="black", lwd=3) # vertical
abline(h=2) # horizontal
abline(a=1, b=2) # intercept a, slope b
abline(lsfit(x,y)) # regression	line to scatter

hist(x, nclass=40, prob=T)

# syntax
for (i in 4:10) {}

