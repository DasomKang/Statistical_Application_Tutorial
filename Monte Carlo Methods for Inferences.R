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

