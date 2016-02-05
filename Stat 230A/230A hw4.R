rm(list=ls())
setwd('C:/Users/Wes/Desktop/')

G <- matrix(.05,nrow=100,ncol=100)
diag(G)<-1
R <- chol(G)
errors <- matrix(rnorm(100000),nrow=100,ncol=1000)
errors <- apply(errors,2,function(x){x%*%R})
S <- apply(errors,2,sum)
var_S <- var(S)
sigma <- apply(errors,2,var)
sigma_hat <- mean(sigma)

r <- rep(0,1000)
for (i in 1:1000){
  test <- errors[,i]%*%t(errors[,i])
  diag(test)<-0
  r[i]<-sum(test)/9900
}
mean(r)

var_S
100*sigma_hat + 100*99*sigma_hat*mean(r)

100*1+100*99*1*.05