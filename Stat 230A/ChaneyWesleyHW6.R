
rm(list=ls())

x <- scan('mle.txt')

#Part 1

#Formula is nlog(theta) - 2 sum(log(theta)+Xi)
#Plotted below
theta <- seq(0,30,.1)
llh <- rep(0,length(theta))
for (i in 1:length(theta)){
  llh[i] <- length(x)*log(theta[i])-2*sum(log(theta[i]+x))
}

plot(theta,llh,xlab='Theta value', ylab='Log-likelihood')

#This plots the likelihood, not part of the assignment
lh <- rep(0,length(theta))
for (i in 1:length(theta)){
  lh[i] <- prod(theta[i]/(theta[i]+x)^2)
}


#Part 2
negllh <- function(theta,x){
  negllh <- -1*length(x)*log(theta)+2*sum(log(theta+x))
}

#I didn't bother transforming theta the way Freedman suggests because I just set lower bound of 0 
opt <- optim(par=20,fn=negllh,x=x,method="Brent",lower=0,upper=50)
theta_hat <- opt$par

#Part 3 - using the second derivative of the log-likelihood at theta_hat, invert, multiply by -1
# to get variance and then take the sqrt

SEtheta_hat <- sqrt((length(x)/theta_hat^2 - 2*sum(1/(theta_hat+x)^2))^-1)


doSims <- function(n=1000,theta=25){
  theta_hat <- rep(0,n)
  for (i in 1:n){
    U <- runif(50)
    x<- theta*U / (1-U)
    opt <- optim(par=20,fn=negllh,x=x,method="Brent",lower=0,upper=50)
    theta_hat[i] <- opt$par
  }
  return(theta_hat)
}

theta_hats <- doSims()
hist(theta_hats)
mean(theta_hats)
sd(theta_hats)