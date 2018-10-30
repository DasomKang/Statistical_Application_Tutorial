# 2018.10.27 recap
#####################################
# MCMC
#####################################

#1. M-H algorithm

#target distributions Beta(a,a)

a = c(1,6,12,18)
target = function(x,i) dbeta(x, a[i], a[i]) #beta distribution 
?dbeta

# grid for plotting density function
x = seq(0,1,0.01)
x
#storage vector
N =  150000
sim.vec = matrix(0,4,N)

#Metropolis algorithm with normal kernel
par(mfrow=c(2,2))
for (i in 1:4)
{
  y = target(x,i)
  plot(x, y, type="l", main=paste("Beta(", a[i],")",sep=""))
  
  current.x = 1/2  # Initial value
  for (k in 1:N)
  {
    propose.x = rnorm(1, current.x, 0.5) #~normal dist
    alpha = min(1, target(propose.x, i)/target(current.x,i))
    if (alpha==1) current.x = propose.x
    else
    {
      tmp = runif(1)
      if (tmp <= alpha) current.x = propose.x
    }
    sim.vec[i,k] = current.x
  }
  #plot result
  hist(sim.vec[i,50000:N], n= 50, prob=TRUE, add=T)
  lines(x,y,col="red")
}

#1. Gibbs sampler
#bivariate normal distribution <- target dist

par(mfrow=c(1,2))
library(MASS)

mu = c(0,0)
sigma = matrix(c(1,0.8,1,0.8),2,2)
Y = mvrnorm(100,mu,sigma)
plot(Y, xlim=c(-11,11), ylim=c(-11,11), xlab="X1", ylab="X2", main="Bivarate Normal")

N = 100
rho = 0.8

X1 = vector('numeric', N)
X2 = vector('numeric', N)

#Chain 1
X1[1] = 10
X2[1] = 10

for (i in 2:100)
{
  if ( i%%2 == 1)
  {
    X1[i] = X1[i-1]
    X2[i] = rnorm(1, mean=rho*X1[i-1], sd=sqrt(1-rho^2))
  }
  else
  {
    X1[i] = rnorm(1, mean=rho*X2[i-1], sd=sqrt(1-rho^2))
    X2[i] = X2[i-1]
  }
}

plot(X1, X2, type="b", xlim=c(-11,11), ylim=c(-11,11), main="Gibbs Samplers")