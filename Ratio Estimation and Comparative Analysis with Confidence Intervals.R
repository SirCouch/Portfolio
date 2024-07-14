########Week 12########

###Estimating the ratio
###Using Y to be 2002 and X to be 1994
Y <- c(1363,670,761,746,991,798,728,842,849,885,986,934,1167)
X <- c(1087,571,518,612,770,655,552,710,656,636,676,773,829)
n <- 13
N <- 47
muX <- mean(X)
###Estimating ratio
r <- sum(Y)/sum(X)
###Calculating variance
s2r <- sum((Y-r*X)^2)/(n-1)
###Variance of ratio
Vr <- (1-n/N)*(1/muX^2)*(s2r/n)
###CI
r-2*sqrt(Vr)
r+2*sqrt(Vr)


####Estimating the total
###Using Y to be sugar and X to be weight
sY <- 0.246
sX <- 4.35
n <- 10
muX <- sX/n
tauX <- 1800
###Estimating ratio
r <- sum(sY)/sum(sX)
###Estimating the total
tauY <- r*tauX
###Calculating variance
s2r <- 0.0024^2
###Variance of total
Vt <- (tauX^2)*(1/muX^2)*(s2r/n)
###CI
tauY-2*sqrt(Vt)
tauY+2*sqrt(Vt)

####Estimating mean
####Comparing both estimators
require(mvtnorm)
###Data is not correlated
MX1 <- matrix(data = NA_real_,nrow = 10000,ncol = 2)
for(i in 1:10000){
  X <- rmvnorm(n = 1000,mean = c(20,40),sigma = matrix(c(4,0,0,2),byrow = T,nrow = 2))
  s <- colSums(X)
  MX1[i,1] <- (s[1]/s[2])*40
  MX1[i,2] <- mean(X[,1])
}
plot(X)
###Data is correlated
MX2 <- matrix(data = NA_real_,nrow = 10000,ncol = 2)
for(i in 1:10000){
  X <- rmvnorm(n = 1000,mean = c(20,40),sigma = matrix(c(4,sqrt(4)*sqrt(2)*0.9,sqrt(4)*sqrt(2)*0.9,2),byrow = T,nrow = 2))
  s <- colSums(X)
  MX2[i,1] <- (s[1]/s[2])*40
  MX2[i,2] <- mean(X[,1])
}
plot(X)
###Comparing the variances
c(Ratio=var(MX1[,1]),Mean=var(MX1[,2]))
c(Ratio=var(MX2[,1]),Mean=var(MX2[,2]))

###Using Y to be present and X to be pre-study
sY <- 11458
sX <- 10103
n <- 12
N <- 500
muX <- 880
###Estimating ratio
r <- sum(sY)/sum(sX)
###Estimating the total
muY <- r*muX
###Calculating variance
s2r <- 94.0672^2
###Variance of total
Vmu <- (muX^2)*(1-n/N)*(1/muX^2)*(s2r/n)
###CI
muY-2*sqrt(Vmu)
muY+2*sqrt(Vmu)
