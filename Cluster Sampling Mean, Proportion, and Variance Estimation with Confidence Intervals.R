
#######
##Total size of each selected cluster
M <- c(1052,210,1376,418)
##Number of selected cluster
n <- 4
##Sample size of each selected cluster
m <- c(10,13,9,10,10,12,8,13,8,11)
##Sample mean of each cluster
x <- c(5.4,4,5.67,4.8,4.3,3.83,5,3.85,4.88,5)
##Sample variance of each cluster
s2 <- c(11.38,10.67,16.67,13.29,11.12,14.88,5.14,4.31,6.13,11.80)
##Total number of clusters
NN <- 90
##Total number of individuals
MM <- 4500
##Average per cluster
ML <- MM/NN


##Estimating the mean
mu <- NN/(MM*n)*sum(M*x)
##S2b
s2b <- sum((M*x-ML*mu)^2)/(n-1)
##Estimating the variance of the mean
s2mu <- (1-n/NN)*(1/(n*ML^2))*s2b+(1/(n*NN*ML^2))*sum(M^2*(1-m/M)*(s2/m))
##CI
mu-2*sqrt(s2mu)
mu+2*sqrt(s2mu)

##Estimating total
tot <- MM*mu
##Estimating the variance of the total
s2tot <- MM^2*((1-n/NN)*(1/(n*ML^2))*s2b+(1/(n*NN*ML^2))*sum(M^2*(1-m/M)*(s2/m)))
##CI
tot-2*sqrt(s2tot)
tot+2*sqrt(s2tot)

##Estimating mean without the total population
mur <- sum(M*x)/sum(M)
##S2r
s2r <- sum(M^2*(x-mur)^2)/(n-1)
##Estimate ML
ml <- sum(M)/n
##Estimating the variance of the mean
s2mur <- (1-n/NN)*(1/(n*ml^2))*s2r+(1/(n*NN*ml^2))*sum(M^2*(1-m/M)*(s2/m))
##CI
mur-2*sqrt(s2mur)
mur+2*sqrt(s2mur)


##Estimating proportion
##Total size of each selected cluster
M <- c(50,65,45,48,52,58,42,66,40,56)
##Sample size of each selected cluster
m <- c(10,13,9,10,10,12,8,13,8,11)
##Total number of clusters
N <- 90
##Number of selected cluster
n <- 10
##Estimate ML
ml <- sum(M)/n
##Estimated proportion for each cluster
phat <- c(0.4,0.38,0.22,0.30,0.50,0.25,0.38,0.31,0.25,0.36)

##Proportion
p <- sum(M*phat)/sum(M)
##Variance
s2r <- sum((M*phat-M*p)^2)/(n-1)
##Variance of the proportion
s2p <- (1-n/N)*(1/(n*ml^2))*s2r+1/(n*N*ml^2)*sum((M^2)*(1-m/M)*((phat*(1-phat))/(m-1)))
##CI
p-2*sqrt(s2p)
p+2*sqrt(s2p)


####Same size clusters
x <- read.csv("battery.csv")
###Number of sampled clusters
n <- 4
###Number of sampled elements per cluster
m <- 9
###Number of total clusters
N <- 100
###Total number of elements in the population
M <- 4500
###Size of clusters in the population
ML <- M/N
###Calculating the mean of each cluster
means <- aggregate(x = x$Thick,by=list(x$Battery),FUN=mean)$x
###Estimating population mean
est.m <- sum(means)/n
###Using ANOVA to estimate MSB and MSW
y <- summary(aov(Thick~factor(Battery),data = x))
MSB <- y[[1]]$`Mean Sq`[1]
MSW <- y[[1]]$`Mean Sq`[2]
###Calculating variance of the mean
v <- (1-n/N)*(MSB/(n*m))+(1-m/ML)*(MSW/(N*m))
###Create CI
est.m-2*sqrt(v)
est.m+2*sqrt(v)