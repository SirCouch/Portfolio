######Simulating Normal distribution######
####We will use the rnorm function make this simulation
####Let's plot the normal distribution and simulate results
#Creating the plot
require(ggplot2)
p <- ggplot(data = data.frame(x=170))
p+stat_function(fun = dnorm,args=list(mean=170,sd=15),xlim = c(110,230),colour = "tomato",size=1)+
  theme_bw()
#Simulating the data
X <- data.frame(x = rnorm(n = 10000,mean = 170,sd = 15))
p <- ggplot(data = X)
p+geom_histogram(mapping = aes(x = x,y = ..density..),colour="steelblue",fill="steelblue")+
  stat_function(fun = dnorm,args=list(mean=170,sd=15),xlim = c(110,230),colour = "tomato",size=1)+
  theme_bw()
#Plotting the correct distribution of the mean
p+stat_function(fun = dnorm,args=list(mean=170,sd=15/sqrt(30)),xlim = c(160,180),colour = "tomato",size=1)+
  theme_bw()
#Let's simulate 10000 samples of 30 individual 
Y <- as.data.frame(replicate(n = 10000,expr = rnorm(n = 30,mean = 170,sd = 15)))
Simul <- data.frame(Mean = apply(X = Y,MARGIN = 2,FUN = mean))
p+geom_histogram(mapping = aes(x = Mean,y = ..density..),colour="steelblue",fill="steelblue",data = Simul)+
  stat_function(fun = dnorm,args=list(mean=170,sd=15/sqrt(30)),xlim = c(160,180),colour = "tomato",size=1)+
  theme_bw()
#Combining both plots
p <- ggplot(data = X)
p+geom_histogram(mapping = aes(x = x,y = ..density..),colour="blue",fill="blue",alpha=0.3)+
  stat_function(fun = dnorm,args=list(mean=170,sd=15),xlim = c(110,230),colour = "steelblue",size=1)+
  theme_bw()+
  geom_histogram(mapping = aes(x = Mean,y = ..density..),colour="red",fill="red",data = Simul,alpha=0.3)+
  stat_function(fun = dnorm,args=list(mean=170,sd=15/sqrt(30)),xlim = c(160,180),colour = "tomato",size=1)



#########################Simulating rolling a die
Y1 <- data.frame(X = replicate(n = 100000,expr = mean(sample(x = 6,size = 1,replace = T))))
Y2 <- data.frame(X = replicate(n = 100000,expr = mean(sample(x = 6,size = 2,replace = T))))
Y10 <- data.frame(X = replicate(n = 100000,expr = mean(sample(x = 6,size = 10,replace = T))))
Y50 <- data.frame(X = replicate(n = 100000,expr = mean(sample(x = 6,size = 50,replace = T))))

ggplot()+geom_bar(mapping = aes(x = X,y = ..count../sum(..count..)),data = Y1,fill = "red")+theme_bw()
ggplot()+geom_histogram(mapping = aes(x = X,y = ..density..),data = Y2,bins = 20,fill = "blue")+theme_bw()
ggplot()+geom_histogram(mapping = aes(x = X,y = ..density..),data = Y10,bins = 20,fill = "black")+theme_bw()
ggplot()+geom_histogram(mapping = aes(x = X,y = ..density..),data = Y50,bins = 20,fill = "green")+theme_bw()

ggplot()+geom_histogram(mapping = aes(x = X,y = ..density..),data = Y50,bins = 20,fill = "green",alpha=0.4)+
  geom_histogram(mapping = aes(x = X,y = ..density..),data = Y10,bins = 20,fill = "black",alpha=0.4)+
  geom_histogram(mapping = aes(x = X,y = ..density..),data = Y2,bins = 20,fill = "blue",alpha=0.4)+
  geom_bar(mapping = aes(x = X,y = ..count../sum(..count..)),data = Y1,fill = "red",alpha=0.4)+
  theme_bw()

########################Simulating a exponential distribution
Y <- as.data.frame(replicate(n = 10000,expr = rexp(n = 5,rate = 10)))
Simul <- data.frame(Mean = apply(X = Y,MARGIN = 2,FUN = mean))
p+geom_histogram(mapping = aes(x = Mean,y = ..density..),colour="steelblue",fill="steelblue",data = Simul,alpha=0.4,bins = 60)+
  stat_function(fun = dexp,args=list(rate=50),colour = "tomato",size=1)+
  theme_bw()
########################Simulating the log version
Y <- log(as.data.frame(replicate(n = 10000,expr = rexp(n = 5,rate = 10))))
Simul <- data.frame(Mean = apply(X = Y,MARGIN = 2,FUN = mean))
p+geom_histogram(mapping = aes(x = Mean,y = ..density..),colour="steelblue",fill="steelblue",data = Simul,alpha=0.4,bins = 60)+
  geom_histogram(mapping = aes(x = X1,y = ..density..),colour="tomato",fill="tomato",data = data.frame(t(Y)),alpha=0.4,bins = 60)+
  theme_bw()


########################Simulating a beta distribution
Y <- as.data.frame(replicate(n = 10000,expr = rbeta(n = 3,shape1 = 20,shape2 = 1)))
Simul <- data.frame(Mean = apply(X = Y,MARGIN = 2,FUN = mean))
p+geom_histogram(mapping = aes(x = Mean,y = ..density..),colour="steelblue",fill="steelblue",data = Simul,alpha=0.4,bins = 60)+
  stat_function(fun = dbeta,args=list(shape1 = 20,shape2 = 1),colour = "tomato",size=1)+
  theme_bw()
########################Simulating the log version
Y <- log(as.data.frame(replicate(n = 10000,expr = rbeta(n = 5,shape1 = 20,shape2 = 1))))
Simul <- data.frame(Mean = apply(X = Y,MARGIN = 2,FUN = mean))
p+geom_histogram(mapping = aes(x = Mean,y = ..density..),colour="steelblue",fill="steelblue",data = Simul,alpha=0.4,bins = 60)+
  geom_histogram(mapping = aes(x = X1,y = ..density..),colour="tomato",fill="tomato",data = data.frame(t(Y)),alpha=0.4,bins = 60)+
  theme_bw()

