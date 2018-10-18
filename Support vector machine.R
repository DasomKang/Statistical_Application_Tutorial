#########################
# Support vector machine#
#########################

#Packages for create new dataset
install.packages('e1071')
install.packages('mvtnorm')
library(e1071)
library(mvtnorm)



##Support vector classifier (Binary classifier)

########################
#CASE1 (n1= 30, n2 = 20)
########################
#1. Data generation 

#1.1 set values and covariance matrix
n1 = 30
n2 = 20
p = 2

Sig = matrix(c(2,-0.75,-0.75,4), nrow=2, ncol=2) #covariance matrix
Sig




#1.2 create a training and test dataset

#1.2.1 Training dataset
x1.tr = rmvnorm(n1, mean=rep(0,p), sigma=Sig)
x2.tr = rmvnorm(n2, mean=rep(3,p), sigma=Sig)
x.tr = rbind(x1.tr, x2.tr)
y.tr = c(rep(-1,n1), rep(1,n2))
train = data.frame(x.tr, y=as.factor(y.tr))
train
plot(x.tr, col=(3-y.tr))


#1.2.2 Test dataset
x1.te = rmvnorm(n1, mean=rep(0,p), sigma=Sig)
x2.te = rmvnorm(n2, mean=rep(3,p), sigma=Sig)
x.te = rbind(x1.te, x2.te)
y.te = c(rep(-1,n1), rep(1,n2))
test = data.frame(x.te, y=as.factor(y.te))
test
plot(x.te, col=(3-y.te))



#2. Classifier

# SVM fitting code
# e.g., fit = svm(y ~., data=dat, kernel='linear', cost=5, scale=FALSE)
# cost = tuning parameter, margine의 폭을 결정. C-V로 찾아줘야 하는 값.
#         Non-linear의 경우 decision boundary의 smoothness를 의미함.

#2.1 SVM when cost = 5
fit1 = svm(y ~., data=train, kernel='linear', cost=5, scale=FALSE)
summary(fit1)
fit1$index
plot(fit1,train)

#2.2 SVM when cost = 0.1
fit2 = svm(y ~., data=train, kernel='linear', cost=0.1, scale=FALSE)
summary(fit2)
fit2$index
plot(fit2,train)

#2.3 Parmater tuning of 'SVM' using 5-fold Cross Validation
c = tune(svm, y ~., data=train, kernel='linear', tunecontrol =
           tune.control(cross=5), range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(c)
bestM = c$best.model
summary(bestM)
plot(bestM,train)

#3. Prediction
head(test)
yhat = predict(bestM, test)
sum(yhat != y.te)/ (n1+n2)



################################
#CASE2 #CASE1 (n1= 200, n2 = 200)
################################

#1. Data generation 

#1.1 set values and covariance matrix
n1 = 200
n2 = 200
p = 2

Sig1 = matrix(c(3,-0.75,-0.75,4), nrow=2, ncol=2) #covariance matrix 1
Sig2 = matrix(c(2,0.5,0.5,2), nrow=2, ncol=2) #covariance matrix 2


#1.2 create a training and test dataset

#1.2.1 dataset
x1 = rmvnorm(n1, mean=rep(0,p), sigma=Sig1)
x2 = rmvnorm(n2, mean=rep(5,p), sigma=Sig2)

x = rbind(x1, x2)
y = c(rep(-1,n1), rep(1,n2))

dat = data.frame(x, y=as.factor(y))
dim(dat)
head(dat)
plot(x, col=(3-y))


#1.2.2 separate dataset into training and test set
index = sample(1:(n1+n2),300)
length(index)
train = dat[index,] #training set
test = dat[-index,] #test set
dim(train)
dim(test)




