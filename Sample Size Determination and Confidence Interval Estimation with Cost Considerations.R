require(ggplot2)
x <- data.frame(S=c(1,1,1,1,1,5,12,1,1,1,
                    1,8,16,4,9,1,9,4,1,1,
                    1,4,10,5,18,12,4,5,10,4,
                    16,5,12,12,4,4,10,9,12,8,
                    16,6,4,1,10,3,16,6,10,1,
                    8,12,6,3,16,4,18,4,8,8,
                    8,3,9,1,5,10,4,12,4,18,
                    4,12,16,10,8,18,3,4,8,2,
                    15,6,2,5,8,5,8,4,12,16,
                    3,5,16,3,6,18,4,6,9,12),
                Type = c(rep("A",40),rep("B",60)))



####Sample size for mean using combines

##Pretest
set.seed(1)
A <- x$S[sample(1:40,5)]
B <- x$S[sample(41:100,5)]

##Values
N <- c(40,60)
n <- c(length(A),length(B))
m <- c(mean(A),mean(B))
s2 <- c(var(A),var(B))
##CI for the mean pretest
sum(N*m)/sum(N)-2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
sum(N*m)/sum(N)+2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
##Estimating sample size
B <- 1
cost <- c(20,10)
a <- (1/sqrt(cost))/sum(1/sqrt(cost))
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a
##CI for mean
set.seed(1)
A.full <- x$S[sample(1:40,ceiling(n*a)[1])]
B.full <- x$S[sample(41:100,ceiling(n*a)[2])]

N <- c(40,60)
n <- c(length(A.full),length(B.full))
m <- c(mean(A.full),mean(B.full))
s2 <- c(var(A.full),var(B.full))
sum(N*m)/sum(N)-2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
sum(N*m)/sum(N)+2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))



#################Sample size with budget allocation
####Sample size for mean using combines
N <- c(112,68,39)
s2 <- c(2.25^2,3.24^2,3.24^2)
B <- 1
cost <- c(9,25,36)
a <- (N*sqrt(s2)/sqrt(cost))/sum(N*sqrt(s2)/sqrt(cost))
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a
####Using budget
N <- c(112,68,39)
s2 <- c(2.25^2,3.24^2,3.24^2)
cost <- c(9,25,36)
Budget <- 1500
a <- (N*sqrt(s2)/sqrt(cost))/sum(N*sqrt(s2)/sqrt(cost))
a
n <- 1500/sum(a*cost)
n
n*a
sum(floor(n*a)*cost)

####Exercise

##All
N <- c(155,62,93)
s2 <- c(5.94^2,5.79^2,6.09^2)
B <- 1
cost <- c(9,9,16)
a <- (N*sqrt(s2)/sqrt(cost))/sum(N*sqrt(s2)/sqrt(cost))
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a
sum(floor(n*a)*cost)
####Using budget
N <- c(155,62,93)
s2 <- c(5.94^2,5.79^2,6.09^2)
cost <- c(9,9,16)
Budget <- 500
a <- (N*sqrt(s2)/sqrt(cost))/sum(N*sqrt(s2)/sqrt(cost))
a
n <- Budget/sum(a*cost)
n
n*a
sum(floor(n*a)*cost)
