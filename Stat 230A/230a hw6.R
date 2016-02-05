# x <- abs(rnorm(50))
# y <- 2/(2+x)^2
# prod(y)
# 
# sum(log(2+x))


x <- scan('mle.txt')
theta <- seq(0,100000,10)
llh <- rep(0,length(theta))
for (i in 1:length(theta)){
  llh[i] <- length(x)*log(theta[i])-2*sum(log(theta[i]+x))
}

theta <- seq(0,1,.001)
terms <- rep(0,50)
lh <- rep(0,length(theta))
for (i in 1:length(theta)){
  for (j in 1:length(x)){
    terms[j] <- theta[i]/(theta[i]+x[j]^2)
  }
  lh[i] <- prod(terms)
}