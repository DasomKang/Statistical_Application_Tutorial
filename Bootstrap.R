# 2018.10.31 recap of the simulation class
#####################################
# Bootstrap
#####################################

install.packages("bootstrap")
library(bootstrap)


#1. Boostrap : Estimates of Standard Error and Bias using law data
#1.1 Glance law data
head(law) 
cor(law$LSAT, law$GPA) #Sample Correlation

#1.2 Set up the bootstrap
m = 10000
n = nrow(law)
R = numeric(m)

for (j in 1:m)
{
  i = sample(1:n, size = n, replace = TRUE)
  boot.sample = law[i,]
  R[j] = cor(boot.sample[,1], boot.sample[,2])
}

#1.3 Estimated standard error of bootstrap correlation estimator 
sd(R)
mean(R)

#1.4 Empirical distribution of R
hist(R, prob = TRUE)

#1.5 Bias Estiamtion
mean(R) - cor(law$LSAT, law$GPA)


#2. Jacknife: Estimates of Standard Error and Bias using law data
#2.1 Set up the jackknife
Rjk = numeric(n)
for (i in 1:n) Rjk[i] = cor(law[-i,1],law[-i,2]) #i번째 값 제외하고 구한 correlation

#2.2 Estimate of bias
(n-1) * (mean(Rjk) - cor(law$LSAT, law$GPA))

#2.3 Estimate of Standard Error
sqrt((n-1)/n) * sum((Rjk-mean(Rjk)^2))


############################################
#3. Bootstrap : Confidence Intervals example 1 
# Confidence Interval of mean
install.packages("boot")
library(boot) #내장 함수가 있어 for문보다 빠름

#3.1 Generate data
dat = rchisq(30,df=2)
dat #n이 작아서 bootstrap이 좋다고 할 수는 없으나 연습용으로 data를 작게 만듦
plot(dat)

#3.2 Sample mean 
theta.hat = mean(dat)
theta.hat

boot.f = function(x,i) mean(x[i]) # i = index, x = original data
b.obj = boot(dat, statistic = boot.f, R=2000) #R = 반복횟수

#3.3 Bootstrap samples
nrow(b.obj$t) #2000 
theta.j = as.vector(b.obj$t)

alpha = 0.05

#3.4 Standard Normal bootstrap confidence interval
LB = theta.hat - qnorm((1-alpha/2))*sd(theta.j)
UB = theta.hat + qnorm((1-alpha/2))*sd(theta.j)
c(LB, UB)

#3.5 Basic bootstrap confidence interval
basic = 2*theta.hat - quantile(theta.j,prob=c((1-alpha/2),(alpha/2)))
basic
#3.6 Percentile bootstrap confidence interval
perce = quantile(theta.j,prob=c((alpha/2),(1-alpha/2)))
perce
#3.7 R built-in function for CIs
boot.ci(b.obj, conf=0.95, type=c("norm","basic","perc"))

