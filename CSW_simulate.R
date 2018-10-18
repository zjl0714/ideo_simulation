################################################################################################
# Program: CSW_simulate
# Author:	Ye Wang (NYU)
# Aim: Test the performance of the compliance score weighting method
# Revised: Oct. 2. 2018
################################################################################################
rm(list=ls())
require(AER)

MM <- 1000

s <- rep(0,MM)
for (l in (1 : MM)){
NN <- 1000

X <- rnorm(NN, 2, 4)
X_min <- min(X)
X_max <- max(X)
X_range <- X_max - X_min
X_positive <- X - X_min
type <- c(1, 2, 3) ## here 1 is alway-taker, 2 is complier, 3 is never-taker
X_type <- rep(0, NN)
for (i in 1:NN){
 # X_type[i] <- sample(type, 1, prob = c((X_range - X_positive[i]), 2*X_positive[i] + 1, (X_range - X_positive[i])))
  X_type[i] <- sample(type, 1, prob = c((X_range - X_positive[i])/X_range, 0.5 + as.numeric(X[i]>0), (X_range - X_positive[i])/X_range))
  
}
summary(lm((X_type == 2)~X))
#the prob of being complier is positively correlated with X
summary(X_type == 2)

Z <- rbinom(NN, 1, 0.4)
# D1 <- as.numeric(X_type == 1) + as.numeric(X_type == 2)
D1 <-  as.numeric(X_type == 2)
D0 <- as.numeric(X_type == 3)
# D <- as.numeric(X_type == 1) + Z * D1 + (1-Z) * D0
D <- as.numeric(X_type == 1) + Z * D1 
X2 <- rnorm(NN, 1, 2)
Y0 <- rnorm(NN, 3, 6) + 2 * X2
Y1 <- 4 + Y0
Y <- Y0 * (1 - D) + Y1 * D
ZX <- Z * X
XM <- X - mean(X)
##traditional 2sls
result1 <- ivreg(Y~D | Z)
summary(result1)

##new method
result2 <- ivreg(Y~D | ZX)
summary(result2)

result3 <- ivreg(Y~D*X+D*X2 | Z*X + Z*X2)
summary(result3)

result4 <- ivreg(Y~D  |  Z*XM   )
summary(result4)


result5 <- ivreg(Y~D  |  Z*X    )
summary(result5)

s[l] <- abs(result4$coefficients[2]-4)<abs(result1$coefficients[2]-4)
}
summary(s)

sum(s)
