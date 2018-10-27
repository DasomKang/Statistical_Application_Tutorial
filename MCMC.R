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
