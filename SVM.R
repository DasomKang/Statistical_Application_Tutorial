#########################
# Support vector machine#
#########################

#Packages for create new dataset
install.packages('e1071')
install.packages('mvtnorm')
library(e1071)
library(mvtnorm)

#Support vector classifier (Binary classifier)



#1. Data generation

#1.1 set values and covariance matrix
n1 = 30
n2 = 20
p = 2

Sig = matrix(c(2,-0.75,-0.75,4), nrow=2, ncol=2) #covariance matrix
Sig

x1 = rmvnorm(n1, mean=rep(0,p), sigma=Sig)
x2 = rmvnorm(n2, mean=rep(3,p), sigma=Sig)
x1
x2
x = rbind(x1, x2)
y = c(rep(-1,n1), rep(1,n2))

#1.2 create a new data frame
dat = data.frame(x, y=as.factor(y))
dat

plot(x, col=(2-y))

#2. Classifier

