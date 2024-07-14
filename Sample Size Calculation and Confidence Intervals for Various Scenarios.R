require(ggplot2)
require(samplingbook)
######TV viewing time
###Data
X <- data.frame(Town = c(rep("A",20),rep("B",8),rep("C",12)),
                Time = c(35,43,36,39,28,28,29,25,38,27,
                         26,32,29,40,35,41,37,31,45,34,
                         27,15,32,21,29,25,20,30,8,14,
                         12,15,20,22,21,20,24,7,11,24))
###Plotting the data
ggplot(data = X)+geom_boxplot(mapping = aes(x = 1,y = Time))
###Calculating the mean
mean(X$Time)
sd(X$Time)
Smean(y = X$Time,N = 310)

###Plotting the data
ggplot(data = X)+geom_boxplot(mapping = aes(x = Town,y = Time))
###Calculating the mean and standard deviation
aggregate(Time ~ Town, X, function(x) c(mean = mean(x), sd = sd(x)))

###CI
N <- c(155,62,93)
n <- c(20,8,12)
m <- c(33.2,24.875,16.5)
s2 <- c(5.94^2,5.79^2,6.09^2)

##Town A
m[1]-2*sqrt((1-n[1]/N[1])*s2[1]/n[1])
m[1]+2*sqrt((1-n[1]/N[1])*s2[1]/n[1])

##Town B and C
(m[2]-m[3])-2*sqrt((1-n[2]/N[2])*(s2[2]/n[2])+(1-n[3]/N[3])*(s2[3]/n[3]))
(m[2]-m[3])+2*sqrt((1-n[2]/N[2])*(s2[2]/n[2])+(1-n[3]/N[3])*(s2[3]/n[3]))

##Mean of all
sum(N*m)/sum(N)-2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
sum(N*m)/sum(N)+2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
#Previous method
Smean(y = X$town,N = 310)

####Restaurant example
X <- data.frame(Restaurant = factor(c(rep("Long Horn",14),
                                      rep("Olive Garden",20),
                                      rep("Bahama Breeze",16)),
                                    levels = c("Long Horn","Olive Garden","Bahama Breeze")),
                Salary = c(80,68,72,85,90,67,65,92,86,87,
                           91,81,74,83,67,48,53,65,49,72,
                           53,68,70,63,81,75,73,78,69,81,
                           59,52,63,42,42,36,55,44,53,61,
                           42,39,32,34,29,19,14,31,30,32))

###Plotting the data
ggplot(data = X)+geom_boxplot(mapping = aes(x = Restaurant,y = Salary))
###Calculating the mean and standard deviation
aggregate(Salary ~ Restaurant, X, function(x) c(mean = mean(x), sd = sd(x)))
##Values
N <- c(1376,418,1052,210)
n <- c(22,22,22,22)
m <- c(140.891,725.523,410.215,75.6714)
s2 <- c(133.608^2,518.128^2,375.215^2,63.7742^2)
##CI for the mean
sum(N*m)/sum(N)-2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
sum(N*m)/sum(N)+2*sqrt((1/(sum(N))^2)*(sum((N^2)*(1-n/N)*(s2/n))))
##CI for the total
sum(N*m)-2*sqrt((sum((N^2)*(1-n/N)*(s2/n))))
sum(N*m)+2*sqrt((sum((N^2)*(1-n/N)*(s2/n))))

###Proportion
N <- c(155,62,93)
n <- c(20,8,12)
phat <- c(16,2,6)/n

sum(N*phat)/sum(N)-2*sqrt((sum(N^2*(1-n/N)*(phat*(1-phat)/(n-1))))/((sum(N))^2))
sum(N*phat)/sum(N)+2*sqrt((sum(N^2*(1-n/N)*(phat*(1-phat)/(n-1))))/((sum(N))^2))


####Sample size for mean
N <- c(55,80,65)
s2 <- c(9.29^2,11.48^2,12.44^2)
B <- 1
a <- 1/length(N)
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a


####Sample size for total
N <- c(1376,418,1052,210)
s2 <- c(133.608^2,518.128^2,375.215^2,63.7742^2)
B <- 50000
a <- 1/length(N)
n <- sum(N^2*s2/a)/(B^2/4+sum(N*s2))
n
n*a


####Sample size for mean using population size
N <- c(55,80,65)
s2 <- c(9.29^2,11.48^2,12.44^2)
B <- 1
a <- N/sum(N)
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a



####Sample size for mean using population variability
N <- c(55,80,65)
s2 <- c(9.29^2,11.48^2,12.44^2)
B <- 1
a <- sqrt(s2)/sum(sqrt(s2))
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a

####Sample size for mean using cost
N <- c(55,80,65)
s2 <- c(9.29^2,11.48^2,12.44^2)
B <- 1
cost <- c(1,2,5)
a <- (1/sqrt(cost))/sum(1/sqrt(cost))
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a


####Sample size for mean using combines
N <- c(55,80,65)
s2 <- c(9.29^2,11.48^2,12.44^2)
B <- 1
cost <- c(1,2,5)
a <- (N*sqrt(s2)/sqrt(cost))/sum(N*sqrt(s2)/sqrt(cost))
n <- sum(N^2*s2/a)/((sum(N))^2*B^2/4+sum(N*s2))
n
n*a
