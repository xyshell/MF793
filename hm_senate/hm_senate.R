mu <- c(
  0.47, 0.48, 0.52, 0.45, 0.47,
  0.57, 0.52, 0.47, 0.58, 0.53,
  0.47, 0.41, 0.54, 0.45, 0.52,
  0.53, 0.47, 0.42
)
sigma1 <- c(
  0.013, 0.009, 0.013, 0.017, 0.018,
  0.022, 0.012, 0.014, 0.019, 0.015,
  0.019, 0.013, 0.013, 0.013, 0.014,
  0.015, 0.014, 0.014
)
draw1 <- matrix(
  data = rnorm(18*100000, mu, sigma1),
  nrow = 18, ncol = 100000
)
wins1 <- apply(draw1 > 0.5, 2, sum)
R1 <- wins1 + 45 # R [	45	,	64	]
f1 <- hist(R1, breaks=c(44.5:64.5), xlim=c(45,64), 
     main = "Number of Republican Senators in the 2019 Senate",
     xlab = "Republic Senate Seats",
     ylab = "Probability",
     col = c(rep("blue",1),rep("red",5)),axes=FALSE, prob=T)
axis(1, xaxp = c(45, 64, 19))
axis(2)
text(seq(1,2,3), f1$density+0.05, as.character(f1$probs))


smax <- c(
  0.026, 0.026, 0.026, 0.034, 0.035,
  0.043, 0.026, 0.028, 0.037, 0.029,
  0.038, 0.026, 0.026, 0.026, 0.028,
  0.030, 0.028, 0.028
)
sigma2 <- smax / 1.96
draw2 <- matrix(
  data = rnorm(18*100000, mu, sigma2),
  nrow = 18, ncol = 100000
)
wins2 <- apply(draw2 > 0.5, 2, sum)
R2 <- wins2 + 45 # R [	45	,	64	]
hist(R2, breaks=c(44.5:64.5), xlim=c(45,64), 
     main = "Number of Republican Senators in the 2019 Senate",
     xlab = "Republic Senate Seats",
     ylab = "Probability",
     col = c(rep("blue",1),rep("red",5)),axes=FALSE, prob=T)
axis(1, xaxp = c(45, 64, 19))
axis(2)