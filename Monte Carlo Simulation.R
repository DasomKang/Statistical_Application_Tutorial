# 2018.10.21 recap
#########################
# Monte Carlo Simulation#
#########################



##1. Simple example 1 (when X1, X2 ~ N(0,1))

# X1, X2 ~ N(0,1)
# theta = E([X1 - X2])
# Estimate theta.hat and SE of theta.hat



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






##2. Simple example 2 (when X1, ..., XN ~ N(2,1))

# X1, ..., XN ~ N(2,1)
# Estimate MSE of sample mean, median and trimmed mean
# 새로운 추정량 개발 시, 기존 것 보다 좋다는 걸 비교하기 위해서 MSE를 사용.
# 뭘 비교하면 되는지에 따라 MSE, C.I, alpha, beta(검정 시 오류) 중 선택
theta = 2
n = 20
m = 1000
k = 2

ave = med = tmean = numeric(m)
for (j in 1:m)
{
  x = sort(rnorm(n, theta, 1)) #절삭평균때문에 sort함
  ave[j] = mean(x)
  med[j] = median(x)
  tmean[j] = sum(x[(k+1):(n-k)])/(n-2*k) #절삭평균(작은 값, 큰 값 2개 빼고 계산)
}

#MSE of sample mean
mean((ave-theta)^2)

#MSE of sample median
median((med-theta)^2)

#MSE of sample trimmed mean
mean((tmean-theta)^2)


