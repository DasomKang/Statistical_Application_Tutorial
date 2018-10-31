# 2018.10.31 recap
#####################################
# Bootstrap
#####################################

install.packages("bootstrap")
library(bootstrap)


#1.Boostrap : Estimates of Standard Error and Bias using law data
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



#3. Bootstrap Confidence Intervals         
install.packages("boot")
library(boot) #내장 함수가 있어 for문보다 빠름


