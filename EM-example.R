# 2018.10.30 recap + 추가로 더 해야할 듯
###############
# EM Algorithm
###############

# EM example using faithful data 

attach(faithful)

#mixture normal dist
W = waiting
hist(W, prob=TRUE) 
lines(density(W), col='red')

#Initial value (theta)
s = c(0.5, 30, 80, 16, 16)

em = function(W, s)
{
  Ep = s[1]*dnorm(W, s[2], sqrt(s[4]))/(s[1]*dnorm(W,s[2], sqrt(s[4])) + (1 - s[1])*dnorm(W, s[3], sqrt(s[5])))
  s[1] = mean(Ep)
  s[2] = sum(Ep*w) /sum(Ep)
  s[3] = sum((1-Ep)*W) / sum(1-Ep)
  s[4] = sum(Ep*(W-s[2])^2) / sum(Ep)
  s[5] = sum((1-Ep)*(W-s[3])^2) / sum(1-Ep) #short time에 대한 mu, sigma
  return(s)
}

iter = function(W,s)
{
  sl = em(W,s)
  for (i in 1:5)
  {
    if (abs(s[i] - sl[i]) > 0.0001)
    {
      s = sl
      sl = iter(W,s) #while을 안써도 converge할 때 까지 돌아감
    }
    else sl
  }
  return(sl)
}

iter(W,s)


