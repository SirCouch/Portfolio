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
##Calculating mean and variance
mean(x)
mean((x-mean(x))^2)

###Plotting the distribution of the mean together with the samples calculated in class
require(ggplot2)
p <- ggplot(data = data.frame(x=7.26))

##Haphazard samples
haphazard <- c(7.1,7.2,7.9,7.9,8.5,8.6,8.7,9,9.7,10,12.9)
p+stat_function(fun = dnorm,args = list(mean=7.26,sd=sqrt(25.5724/10)),xlim = c(2,13))+
  scale_x_continuous(breaks=haphazard)+
  theme_bw()+
  theme(axis.ticks.y = element_blank())+
  geom_vline(xintercept=7.26,colour="steelblue",size=1)

##SRS
srs <- c(5.4,5.6,5.8,6.1,6.7,7.2,7.5,7.7,7.6,8.4,8.4)
p+stat_function(fun = dnorm,args = list(mean=7.26,sd=sqrt(25.5724/10)),xlim = c(2,13))+
  scale_x_continuous(breaks=srs)+
  theme_bw()+
  theme(axis.ticks.y = element_blank())+
  geom_vline(xintercept=7.26,colour="steelblue",size=1)

###Getting a SRS
pos <- sample(x = length(x),size = 10,replace = F)
pos
x[pos]

######Calculating the variance of the finite sample mean######
pop <- 1:4
samples <- matrix(data = c(1,2,1,3,1,4,2,3,2,4,3,4),ncol = 2,byrow = T)
samples
rowMeans(samples)
mean.prob <- prop.table(table(rowMeans(samples)))

###Calculating mean and var of the mean
mean.mean <- sum(unique(rowMeans(samples))*mean.prob)
var.mean <- sum((unique(rowMeans(samples))-mean.mean)^2*mean.prob)

###On average, which calculation give us the correct numbers
var.prob <- prop.table(table(apply(X = samples,MARGIN = 1,FUN = var)))
mean.var <- sum(unique(apply(X = samples,MARGIN = 1,FUN = var))*var.prob)

##Traditional
mean.var/2
##New
(1-2/4)*(mean.var/2)
