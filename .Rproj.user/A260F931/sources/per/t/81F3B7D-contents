### ---problem 4---
 
tbl_funds = read.csv('funds-1584g-mon.csv')
tbl_factors = read.csv('Fama_French_Three_Factors_Monthly.CSV')
tbl_mkt = data.frame(cbind(tbl_factors[1], tbl_factors[2] + tbl_factors[5])) / 100
names(tbl_mkt) = c('Date','Mkt')
tbl_funds[1] = tbl_mkt[1]

# (a)
funds_mean0 = colMeans(tbl_funds[tbl_funds$Date < 2014.01,2:ncol(tbl_funds)])
mkt_mean0 = mean(tbl_mkt[tbl_mkt$Date < 2014.01, 2:ncol(tbl_mkt)])
funds_mean_annual = funds_mean0*12
mkt_mean_annual = mkt_mean0*12
hist(funds_mean_annual, nclass=40)	
abline(v=mean(funds_mean_annual), col="black",lwd=5)
abline(v=mkt_mean_annual, col="red",lwd=5)
percentage = sum(funds_mean_annual - mkt_mean_annual > 0) / length(funds_mean_annual)

# (b)
funds_mean1 = colMeans(tbl_funds[tbl_funds$Date >= 2014.01,2:ncol(tbl_funds)])
mkt_mean1 = mean(tbl_mkt[tbl_mkt$Date >= 2014.01, 2:ncol(tbl_mkt)])
plot(funds_mean0, funds_mean1, main='R1 vs R0')
points(mkt_mean0, mkt_mean1, col='red', pch=16, cex=2)

quantile0 = quantile(funds_mean0, c(.2, .8))
L0 = funds_mean0[funds_mean0 <= quantile0[1]]
M0 = funds_mean0[funds_mean0 > quantile0[1] & funds_mean0 < quantile0[2]]
W0 = funds_mean0[funds_mean0 >= quantile0[2]]
quantile1 = quantile(funds_mean1, c(.2, .8))
L1 = funds_mean1[funds_mean1 <= quantile1[1]]
M1 = funds_mean1[funds_mean1 > quantile1[1] & funds_mean1 < quantile1[2]]
W1 = funds_mean1[funds_mean1 >= quantile1[2]]

tbl_1a = matrix(0, 
           nrow=3, ncol=3, dimnames=list(c("L0","M0","W0"), c("L1","M1","W1")))
tbl_1a[1] = length(intersect(names(L0),names(L1)))
tbl_1a[2] = length(intersect(names(M0),names(L1)))
tbl_1a[3] = length(intersect(names(W0),names(L1)))
tbl_1a[4] = length(intersect(names(L0),names(M1)))
tbl_1a[5] = length(intersect(names(M0),names(M1)))
tbl_1a[6] = length(intersect(names(W0),names(M1)))
tbl_1a[7] = length(intersect(names(L0),names(W1)))
tbl_1a[8] = length(intersect(names(M0),names(W1)))
tbl_1a[9] = length(intersect(names(W0),names(W1)))

tbl_1b = round(tbl_1a / sum(tbl_1a),2)

tbl_1c = matrix(0, 
                nrow=3, ncol=3, dimnames=list(c("L0","M0","W0"), c("L1","M1","W1")))
tbl_1c[1,] = tbl_1a[1,] / sum(tbl_1a[1,])
tbl_1c[2,] = tbl_1a[2,] / sum(tbl_1a[2,])
tbl_1c[3,] = tbl_1a[3,] / sum(tbl_1a[3,])
tbl_1c = round(tbl_1c, 2)

tbl_1d = rbind(c(0.20, 0.60, 0.20),c(0.20, 0.60, 0.20),c(0.20, 0.60, 0.20))
colnames(tbl_1d) = c("L1","M1","W1")
rownames(tbl_1d) = c("L0","M0","W0")

# (c)

L0 = funds_mean0[funds_mean0 <= mkt_mean0]
W0 = funds_mean0[funds_mean0 > mkt_mean0]
L1 = funds_mean0[funds_mean1 <= mkt_mean1]
W1 = funds_mean0[funds_mean1 > mkt_mean1]

tbl_2a = matrix(0, 
                nrow=2, ncol=2, dimnames=list(c("L0","W0"), c("L1","W1")))
tbl_2a[1] = length(intersect(names(L0),names(L1)))
tbl_2a[2] = length(intersect(names(W0),names(L1)))
tbl_2a[3] = length(intersect(names(L0),names(W1)))
tbl_2a[4] = length(intersect(names(W0),names(W1)))

tbl_2b = round(tbl_2a / sum(tbl_2a),2)

tbl_2c = matrix(0, 
                nrow=2, ncol=2, dimnames=list(c("L0","W0"), c("L1","W1")))
tbl_2c[1,] = tbl_2a[1,] / sum(tbl_2a[1,])
tbl_2c[2,] = tbl_2a[2,] / sum(tbl_2a[2,])
tbl_2c = round(tbl_2c, 2)

pL1 = round(sum(funds_mean1 <= mkt_mean1)/length(funds_mean1),2)
pW1 = round(sum(funds_mean1 > mkt_mean1)/length(funds_mean1),2)
tbl_2d = rbind(c(pL1,pW1),c(pL1,pW1))
colnames(tbl_2d) = c("L1","W1")
rownames(tbl_2d) = c("L0","W0")

### ----problem 5----

B = seq(0,90000,1)
EU = 0.5*(-1/((10000+B)*1.3)) + 0.5*(-1/((10000+B)*0.9))
plot(B, EU, main='EU vs B')
CE = 117*(10000+B)/110
plot(B, CE, main='CE vs B')
points(B[which(CE==max(CE))], CE[which(CE==max(CE))],col='red',pch=16, cex=2)
