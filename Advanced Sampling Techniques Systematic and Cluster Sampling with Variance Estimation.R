###Variance Estimate Alternative

##Systematic
Trees <- read.csv("Trees.csv")
N <- nrow(Trees)
k <- 10
#Select the first term
set.seed(100)
starting <- sample(x = k,size = 1)
#Select every kth area
samples <- seq(starting,N,by=k)
#Collect number of trees data for our sample
values <- Trees[samples,1]
plot(values)
#Calculating successive differences
d <- diff(values)
plot(d)
n <- length(values)
#Mean
m <- mean(values)
m
#Variance of the total
(1-n/N)*(1/(2*n*(n-1)))*(sum(d^2))
#CI
m-2*sqrt((1-n/N)*(1/(2*n*(n-1)))*(sum(d^2)))
m+2*sqrt((1-n/N)*(1/(2*n*(n-1)))*(sum(d^2)))



####Cluster Sampling

####Mean
###Budget example
x <- read.csv("budget.csv")
N <- 400
n <- length(x$Households)
M <- 3100
Y <- x$TotalBudget
m <- x$Households
ML <- M/N
mL <- sum(m)/n
##Estimated mean
sum(Y)/sum(m)

##Estimated variance
s2 <- sum((Y-(sum(Y)/sum(m))*m)^2)/(n-1)
#Using population ML
(1-n/N)*(s2/(n*ML^2))
#Using sample mL
(1-n/N)*(s2/(n*mL^2))
#CI Using population ML
sum(Y)/sum(m)-2*sqrt((1-n/N)*(s2/(n*ML^2)))
sum(Y)/sum(m)+2*sqrt((1-n/N)*(s2/(n*ML^2)))
#CI Using sample mL
sum(Y)/sum(m)-2*sqrt((1-n/N)*(s2/(n*mL^2)))
sum(Y)/sum(m)+2*sqrt((1-n/N)*(s2/(n*mL^2)))




####Total Using M
###Budget example
x <- read.csv("budget.csv")
N <- 400
n <- length(x$Households)
M <- 3100
Y <- x$TotalBudget
m <- x$Households
ML <- M/N
mL <- sum(m)/n
##Estimated total
M*sum(Y)/sum(m)

##Estimated variance
s2 <- sum((Y-(sum(Y)/sum(m))*m)^2)/(n-1)
#Using population ML
(N^2)*(1-n/N)*(s2/(n))
#CI Using population ML
M*sum(Y)/sum(m)-2*sqrt((N^2)*(1-n/N)*(s2/(n)))
M*sum(Y)/sum(m)+2*sqrt((N^2)*(1-n/N)*(s2/(n)))


####Total not using M
###Budget example
x <- read.csv("budget.csv")
N <- 400
n <- length(x$Households)
M <- 3100
Y <- x$TotalBudget
m <- x$Households
ML <- M/N
mL <- sum(m)/n
##Estimated total
N*sum(Y)/n

##Estimated variance
s2 <- sum((Y-mean(Y))^2)/(n-1)
#Using population ML
(N^2)*(1-n/N)*(s2/(n))
#CI Using population ML
N*sum(Y)/n-2*sqrt((N^2)*(1-n/N)*(s2/(n)))
N*sum(Y)/n+2*sqrt((N^2)*(1-n/N)*(s2/(n)))


###Magazines example
z <- read.csv("magazines.csv")
Z <- as.data.frame(aggregate(x = z$Magazine,by = list(z$Cluster),FUN=function(x){
  c(Sum=sum(x),m=length(x))}))
Z
N <- 400
n <- length(Z$Group.1)
M <- 4000
Y <- Z$x[,1]
m <- Z$x[,2]
ML <- M/N
mL <- sum(m)/n

##Estimated mean
sum(Y)/sum(m)
##Estimated variance
s2 <- sum((Y-(sum(Y)/sum(m))*m)^2)/(n-1)
#Using population ML
(1-n/N)*(s2/(n*ML^2))
#Using sample mL
(1-n/N)*(s2/(n*mL^2))
#CI Using population ML
sum(Y)/sum(m)-2*sqrt((1-n/N)*(s2/(n*ML^2)))
sum(Y)/sum(m)+2*sqrt((1-n/N)*(s2/(n*ML^2)))
#CI Using sample mL
sum(Y)/sum(m)-2*sqrt((1-n/N)*(s2/(n*mL^2)))
sum(Y)/sum(m)+2*sqrt((1-n/N)*(s2/(n*mL^2)))


###Estimated total
M*sum(Y)/sum(m)
##Estimated variance
s2 <- sum((Y-(sum(Y)/sum(m))*m)^2)/(n-1)
#Using population ML
(N^2)*(1-n/N)*(s2/(n))
#CI Using population ML
M*sum(Y)/sum(m)-2*sqrt((N^2)*(1-n/N)*(s2/(n)))
M*sum(Y)/sum(m)+2*sqrt((N^2)*(1-n/N)*(s2/(n)))

##Estimated total
N*sum(Y)/n
##Estimated variance
s2 <- sum((Y-mean(Y))^2)/(n-1)
#Using population ML
(N^2)*(1-n/N)*(s2/(n))
#CI Using population ML
N*sum(Y)/n-2*sqrt((N^2)*(1-n/N)*(s2/(n)))
N*sum(Y)/n+2*sqrt((N^2)*(1-n/N)*(s2/(n)))



####Student dataset
x <- read.csv("student.csv")
X <- as.data.frame(aggregate(x = x$Student,by = list(x$Cluster),FUN=function(x){
  c(Sum=sum(x),m=length(x))}))
X
N <- 400
n <- length(X$Group.1)
Y <- X$x[,1]
m <- unique(X$x[,2])
ML <- M/N
mL <- sum(m)/n

##Estimated mean
sum(Y)/(n*m)
##Estimated variance
s2 <- m*sum((Y/m-sum(Y)/(n*m))^2)/(n-1)
#Estimated variance of mean
(1-n/N)*(s2/(n*m))
#CI
sum(Y)/(n*m)-2*sqrt((1-n/N)*(s2/(n*m)))
sum(Y)/(n*m)+2*sqrt((1-n/N)*(s2/(n*m)))
