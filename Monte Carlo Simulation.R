# 2018.10.21 recap
#########################
# Monte Carlo Simulation#
#########################



##1. Simple example 1


# X1, X2 ~ N(0,1)
# theta = E([X1 - X2])
# theta.hat & SE(theta.hat)

#1.1 generate theta
m = 20000
theta = numeric(m)
theta
for (i in 1:m)
{
  x = rnorm(2)
  theta[i] = abs(x[1]-x[2]) #abs = 절대값
  #rnorm = random generation for the normal distribution, default : mean = 0, sd = 1 
}

#1.2 Empirical distribution of theta.hat
hist(theta, main="Distribution of theta.hat", prob=TRUE)

#1.3 Monte Carlo Estimator
theta.hat = mean(theta) 
theta.hat

#1.4 Exact value of theta
2/sqrt(pi)

#1.5 Standard error of estimator of theta
sd(theta)

#1.6 Standard error of estimator of Monte Carlo estimator
sd(theta)/sqrt(m)


