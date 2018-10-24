# 2018.10.22 recap
#####################################
# Monte Carlo Methods for Inferences#
#####################################

##1. Example : Empirical type 1 error rate

n = 20
alpha = 0.05
mu0 = 500
sigma2 = 100

m = 10000
I = numeric(m)
for (j in 1:m)
{
  x = rnorm(n, mu0, sqrt(sigma2)) #mu0 = 500
  Tj = (mean(x) - mu0)/(sd(x)/sqrt(n)) # T-statistic under H_0
  if (Tj > qt((1-alpha), df=(n-1))) I[j]=1 #qt((1-alpha) = t_(1-alpha) in t-distribution
  else I[j]=0
}

# Empirical Type 1 error rate;
TypeI = mean(I)
se.hat = sqrt(TypeI*(1-TypeI)/m)
c(TypeI,se.hat)



##2. Example : Empirical power (under H_1)

n = 20
alpha = 0.05
mu0 = 500
sigma2 = 100

I = I1 = numeric(m)
mu1 = seq(490,520,5)

m = 10000
r = length(mu1)
power.hat = power1.hat = numeric(r)


for (i in 1:r)
{
  for (j in 1:m)
  {
    x = rnorm(n, mu1[i], sqrt(sigma2)) #mu0 = 500
    Tj = (mean(x) - mu0)/(sd(x)/sqrt(n)) # T-statistic under H_0
    if (Tj > qt((1-alpha), df=(n-1))) I[j]=1 #qt((1-alpha) = t_(1-alpha) in t-distribution
    else I[j]=0
    if (Tj > qnorm((1-alpha),0,1)) I1[j]=1 #
    else I1[j]=0
  }
  power.hat[i] = mean(I)
  power1.hat[i] = mean(I1)
}

plot(mu1, power.hat, type='b', main='Empirical Power', ylab='Power', col='blue')
lines(mu1, power1.hat, type='b', col='red')

##3. Example : Regression

#Regression : Y_1 = 2 + 3X_1 - 0.5X_2 + epsilon
#epsilon's ~ N(0,1)
#Empirical (1-alpha) % Confidence Interval for beta_1 & beta_2

m = 10
alpha = 0.05
X1 = 1:10
X2 = sample(1:10, 10)
n = length(X1)
sigma = 1
beta = NULL


for (j in 1:m)
{
  Y = 2 + 3*X1 - 0.5*X2 + rnorm(n, 0, sigma) #rnorm : epsilon_i
  reg = lm(Y~X1+X2)
  beta = rbind(beta, reg$coefficients[2:3]) #beta_2, beta_3
}


xx1 = seq(2.6,3.4,0.01)
xx2 = seq(-1,0.3,0.01)
X = cbind(1,X1,X2)
varb = sigma^2 * solve(t(X)%*%X)
sb = sqrt(diag(varb))

?dnorm

par(mfrow=c(1,2))
hist(beta[,1], prob=TRUE, main='Distribution of beta_1')
lines(xx1, dnorm(xx1, 3, sb[2]), col='red') # red line: theoredical
lines(density(beta[,1]), col='blue') # blue line: Estimated density of beta_1.hat (MC)

hist(beta[,2], prob=TRUE, main='Distribution of beta_2')
lines(xx2, dnorm(xx2, -0.5, sb[3]), col='red') # red line: theoredical
lines(density(beta[,2]), col='blue') # blue line: Estimated density of beta_2.hat (MC)

?apply
beta.hat = apply(beta, 2, mean)
beta.hat

se = apply(beta,2,sd) #Estimated SE of beta
se

CI1 = quantile(beta[,1], probs=c((alpha/2), (1-alpha/2)))
CI2 = quantile(beta[,2], probs=c((alpha/2), (1-alpha/2)))
CI1
CI2
