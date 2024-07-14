####Cluster Sampling
setwd("c:/Users/Zachary Kemp/Documents/STAT 466")
####Mean
###Budget example
x <- read.csv("Q1.csv")
plot(x = x$Households,y = x$TotalBudget)


####Proportion
z <- read.csv("renters.csv")
plot(x = z$Residents,y = z$Renters)

###Getting values
a <- z$Renters
m <- z$Residents
p <- a/m
n <- nrow(z)
N <- 415

###Estimating Proportion
phat <- sum(a)/sum(m)

###Estimating variance
s2 <- sum((a-phat*m)^2)/(n-1)
(1-n/N)*(s2/(n*mean(m)^2))

###CI
phat-2*sqrt((1-n/N)*(s2/(n*mean(m)^2)))
phat+2*sqrt((1-n/N)*(s2/(n*mean(m)^2)))



####Estimating sample size

###For mean
N <- 415
M <- 2500
B <- 500
ML <- M/N
##Using pre test for s2
s <- read.csv("income.csv")
s2 <- sum((s$Income-(sum(s$Income)/sum(s$Residents))*s$Residents)^2)/(nrow(s)-1)
##Sample size
(N*s2)/(N*B^2*ML^2/4+s2)

###For Total using population size
N <- 415
M <- 2500
B <- 1000000
ML <- M/N
##Using pre test for s2
s <- read.csv("income.csv")
s2 <- sum((s$Income-(sum(s$Income)/sum(s$Residents))*s$Residents)^2)/(nrow(s)-1)
##Sample size
(N*s2)/(B^2/(4*N)+s2)


###For Total not using population size
N <- 415
B <- 1000000
##Using pre test for s2
s <- read.csv("income.csv")
s2 <- sum((s$Income-mean(s$Income))^2)/(nrow(s)-1)
##Sample size
(N*s2)/(B^2/(4*N)+s2)


###For proportion

N <- 415
B <- 0.01
##Using pre test for s2 and mL
k <- read.csv("renters.csv")
phat <- sum(k$Renters)/sum(k$Residents)
s2 <- sum((k$Renters-phat*k$Residents)^2)/(nrow(k)-1)
mL <- mean(k$Residents)
##Sample size
(N*s2)/(N*mL^2*(B^2)/4+s2)


####Cluster Sampling with Stratification
x <- read.csv("income2.csv")
x1 <- x[which(x$Side=="Left"),]
x2 <- x[which(x$Side=="Right"),]
###Values needed
N1 <- 415
n1 <- nrow(x1)
Y1 <- x1$Income
m1 <- x1$Residents
N2 <- 168
n2 <- nrow(x2)
Y2 <- x2$Income
m2 <- x2$Residents
###Estimating the mean
Yc <- (N1*mean(Y1)+N2*mean(Y2))/(N1*mean(m1)+N2*mean(m2))
###Variance
s21 <- var(Y1-Yc*m1)
s22 <- var(Y2-Yc*m2)
###Variance of mean
(1/(N1*mean(m1)+N2*mean(m2))^2)*(N1^2*(1-n1/N1)*(s21/n1)+N2^2*(1-n2/N2)*(s22/n2))
###CI
Yc-2*sqrt((1/(N1*mean(m1)+N2*mean(m2))^2)*(N1^2*(1-n1/N1)*(s21/n1)+N2^2*(1-n2/N2)*(s22/n2)))
Yc+2*sqrt((1/(N1*mean(m1)+N2*mean(m2))^2)*(N1^2*(1-n1/N1)*(s21/n1)+N2^2*(1-n2/N2)*(s22/n2)))



