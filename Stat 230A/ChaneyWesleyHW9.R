setwd('C:/Users/Wes/Desktop/Stat 230A/')
corrs <- read.table('rindcor.txt')
corrs <- as.matrix(corrs)
corrs[upper.tri(corrs)]<-t(corrs)[upper.tri(corrs)]
idx <- c(11,2:8,1)
M <- corrs[idx,idx]
idz <- c(9,2:8,1)
L <- corrs[idz,idx]
Z <- corrs[idz,idz]
Y <- corrs[idz,10]
Bivls1 <- solve(t(L)%*%solve(t(Z)%*%Z)%*%L)%*%t(L)%*%solve(t(Z)%*%Z)%*%as.matrix(Y)
idx2 <- c(10,2:8,9)
M2 <- corrs[idx2,idx2]
idz <- c(9,2:8,1)
L2 <- corrs[idz,idx2]
Z <- corrs[idz,idz]
Y2 <- corrs[idz,11]
Bivls2 <- solve(t(L2)%*%solve(t(Z)%*%Z)%*%L2)%*%t(L2)%*%solve(t(Z)%*%Z)%*%as.matrix(Y2)



sigma1 <- as.numeric(1+t(Bivls1)%*%M%*%Bivls1-2*t(corrs[idx,10])%*%Bivls1)*1766/(1766-9)  #I guess for aymptotic we can ignore 1766/(1766-9) 
sigma2 <- as.numeric(1+t(Bivls2)%*%M2%*%Bivls2-2*t(corrs[idx2,11])%*%Bivls2)*1766/(1766-9)

cov1 <- sigma1*solve(t(L)%*%solve(t(Z)%*%Z)%*%L)
cov2 <- sigma2*solve(t(L2)%*%solve(t(Z)%*%Z)%*%L2)

SEs1 <- sqrt(diag(cov1/(1766-9)))
SEs2 <- sqrt(diag(cov2/(1766-9)))

Bivls1
Bivls2
SEs1
SEs2

#The reason for the differences in the values that we get is likely to be due to rounding of the correlation
#values that are used in our analysis.

##Part 2
reps <- 1000
oneRep <- function(n,B,C){
  
  e <- rnorm(n)
  d <- .3*e+sqrt(1-.3^2)*rnorm(n)
  Z <- rnorm(n)
  X <- Z*C+d
  Y <- X+e
  B_ols <- solve(t(X)%*%X)%*%t(X)%*%Y
  C_hat <- solve(t(Z)%*%Z)%*%t(Z)%*%X
  X_hat <- Z*C_hat
  Bivls <- solve(t(X_hat)%*%X_hat)%*%t(X_hat)%*%Y
  MSE_ols <- mean((B_ols-1)^2)
  MSEivls <- mean((Bivls-1)^2)
  return(c(B_ols,Bivls,MSE_ols,MSEivls))
}

sim1 <- t(replicate(reps,oneRep(10,1,.1)))
sim2 <- t(replicate(reps,oneRep(1000,1,.1)))
sim3 <- t(replicate(reps,oneRep(10,1,.5)))
sim4 <- t(replicate(reps,oneRep(1000,1,.5)))

summary(sim1)
summary(sim2)
summary(sim3)
summary(sim4)

# We can see just by eye balling the mean and the quantiles that the OLS estimator has lower 
# variance in the simulations as mentioned in technical issue (ii). We can also see that
# increasing C and increasing N leads to better performance of the IVLS estimator in terms of
# MSE because both of these increases reduces the small-sample bias.