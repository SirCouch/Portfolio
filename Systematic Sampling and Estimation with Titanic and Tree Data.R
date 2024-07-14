######Systematic sampling using Titanic data for mean
X <- read.csv("Titanic.csv")
X <- X[sample(nrow(X)),]

head(X)

##Getting a 1-in-5
k<-10
#Select the first term
starting <- sample(x = k,size = 1)
#Select every 10th passenger
samples <- seq(starting,nrow(X),by=k)
#Collect age data for our sample
values <- X[samples,]

##Checking assumptions
#Trend
plot(samples,values$Age)
#Symmetric
hist(values$Age)

##Estimating mean
m <- mean(values$Age)
N <- nrow(X)
n <- length(values$Age)
s2 <- var(values$Age)

#Mean
m
#Variance of the mean
(1-n/N)*(s2/n)
#CI
m-2*sqrt((1-n/N)*(s2/n))
m+2*sqrt((1-n/N)*(s2/n))

#Total
N*m
#Variance of the mean
N^2*(1-n/N)*(s2/n)
#CI
N*m-2*sqrt(N^2*(1-n/N)*(s2/n))
N*m+2*sqrt(N^2*(1-n/N)*(s2/n))

######Systematic sampling using Titanic data for proportion

##Estimating proportion
p <- prop.table(table(values$Survived))[2]
N <- nrow(X)
n <- length(values$Survived)

#Mean
p
#Variance of the mean
(1-n/N)*((p*(1-p))/n)
#CI
p-2*sqrt((1-n/N)*((p*(1-p))/n))
p+2*sqrt((1-n/N)*((p*(1-p))/n))

###Sample size for mean
N <- 1311
s2 <- 14.15182^2
B <- 2

(N*s2)/((N-1)*(B^2)/(4)+s2)

###Sample size for total
N <- 1311
s2 <- 14.15182^2
B <- 2500

(N*s2)/((N-1)*(B^2)/(4*N^2)+s2)


###Sample size for proportion
N <- 1311
p <- 0.358
B <- 0.05

(N*p*(1-p))/((N-1)*(B^2)/(4)+p*(1-p))

###k for proportion
N/288



###Repeated Systematic Sampling

##Systematic
Trees <- read.csv("Trees.csv")
N <- nrow(Trees)
k <- 50
#Select the first term
set.seed(100)
starting <- sample(x = k,size = 1)
#Select every kth area
samples <- seq(starting,N,by=k)
#Collect number of trees data for our sample
values <- Trees[samples,1]
plot(values)
#Estimating total
m <- mean(values)
s2 <- var(values)
n <- length(values)
#Total
m
#Variance of the total
(1-n/N)*(s2/n)
#CI
m-2*sqrt((1-n/N)*(s2/n))
m+2*sqrt((1-n/N)*(s2/n))

##Repeated Syatematic
ns <- 20
ks <- ns*k
#Select the first term
set.seed(100)
starting <- sample(x = ks,size = ns)
#Select every kth area
samples <- t(apply(X = matrix(starting),MARGIN = 1,FUN = function(x){
  seq(x,N,by=ks)
}))

#Collect number of trees data for our sample
values <- t(apply(X = samples,MARGIN = 1,FUN = function(x){
  Trees[x,1]
}))
plot(values)
#Estimating mean
m <- rowMeans(values)
n <- apply(X = values,MARGIN = 1,FUN = length)
s2 <- var(m)
#Mean
mean(m)
#Variance of the mean
(1-sum(n)/N)*(s2/ns)
#CI
mean(m)-2*sqrt((1-sum(n)/N)*(s2/ns))
mean(m)+2*sqrt((1-sum(n)/N)*(s2/ns))
#Estimating mean
m <- rowMeans(values)
n <- apply(X = values,MARGIN = 1,FUN = length)
s2 <- var(m)
#Mean
N*mean(m)
#Variance of the mean
N^2*(1-sum(n)/N)*(s2/ns)
#CI
N*mean(m)-2*sqrt(N^2*(1-sum(n)/N)*(s2/ns))
N*mean(m)+2*sqrt(N^2*(1-sum(n)/N)*(s2/ns))




