######Farms Example######
###Creating the dataset
x <- c(1,1,1,1,1,5,12,1,1,1,
       1,8,16,4,9,1,9,4,1,1,
       1,4,10,5,18,12,4,5,10,4,
       16,5,12,12,4,4,10,9,12,8,
       16,6,4,1,10,3,16,6,10,1,
       8,12,6,3,16,4,18,4,8,8,
       8,3,9,1,5,10,4,12,4,18,
       4,12,16,10,8,18,3,4,8,2,
       15,6,2,5,8,5,8,4,12,16,
       3,5,16,3,6,18,4,6,9,12)

###Estimating the sample size for the mean
N<- 100
B <- 2
std <- 6.02
(N*(std^2))/((N-1)*(B^2/(4))+std^2)

###Getting a SRS
set.seed(60)
pos <- sample(x = length(x),size = 27,replace = F)
mean(x[pos])-2*sqrt((1-27/100)*(var(x[pos])/27))
mean(x[pos])+2*sqrt((1-27/100)*(var(x[pos])/27))
2*sqrt((1-27/100)*(var(x[pos])/27))


###Example
require(samplingbook)
sample.size.mean(e = 2,S = 6.02,N = 100,level = 0.95)
Smean(x[pos], N = 100, level = 0.95)


###Estimating the sample size for the mean
N<- 100
B <- 150
std <- 6.02
(N*(std^2))/((N-1)*(B^2/(4*N^2))+std^2)
##Increasing the pop size
N<- 1000
B <- 150
std <- 6.02
(N*(std^2))/((N-1)*(B^2/(4*N^2))+std^2)
##Increasing the variance
N<- 100
B <- 150
std <- 16.02
(N*(std^2))/((N-1)*(B^2/(4*N^2))+std^2)
##Increasing the error
N<- 100
B <- 300
std <- 6.02
(N*(std^2))/((N-1)*(B^2/(4*N^2))+std^2)




###Estimating proportion
n <- 375
phat <- 161/n
N <- 15664

phat-2*sqrt((1-n/N)*(phat*(1-phat)/(n-1)))
phat+2*sqrt((1-n/N)*(phat*(1-phat)/(n-1)))

##Estimating sample size
phat <- 161/n
N <- 15664
B <- 0.01
(N*phat*(1-phat))/((N-1)*(B^2/4)+phat*(1-phat))

sample.size.prop(e = 0.01,P = phat,N = 15664,level = 0.95)
