ans<-read.csv("stk-day.csv")

x1<-ans[,2];y1<-ans[,3]

m1<-lsfit(x1,y1)

#
# lsfit	 	has values coef, residuals
# ls.print  	           summary, coef.table
# ls.diag	tons of stuff   
# lm  more complete command for LS

coef(m1)
residuals(m1)
ls.print(m1)$summary
ls.diag(m1)
