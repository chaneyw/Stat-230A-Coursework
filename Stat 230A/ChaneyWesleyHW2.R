rm(list=ls())
setwd('C:/Users/Wes/Desktop/Stat 230A/')
load('family.rda')
library(rgl)

#Question 1

#Function calculates the least squares estimate B_hat using matrix algebra
leastsquares <- function(df=family){  
  X <- cbind(rep(1,dim(df)[1]),df[,4],df[,6])
  Y <- as.matrix(df[,5])
  B_hat <- solve(t(X)%*%X)%*%t(X)%*%Y      #Chris Paciorek hates me right now
  predict <- X%*%B_hat
  resid <- Y - predict
  return(list(B_hat,resid))
}

### Code from last assignment, fixed

regcoef<-function(df=family[4:5]){
  # This function takes input of a data frame and returns regression 
  # coefficients predicting the second variable from the first.
  x <- df[,1]
  y <- df[,2]
  r <- sum((x-mean(x))/sqrt(mean((x-mean(x))^2))*(y-mean(y))/sqrt(mean((y-mean(y))^2))/length(x))
  b <- r*sqrt(mean((y-mean(y))^2))/sqrt(mean((x-mean(x))^2))
  a <- mean(y)-b*mean(x)
  return(c(a,b))
}

regline=function(df=family[4:5]){
  # This function plots points and regression line, doesn't need to return
  # anything.
  coefs=regcoef(df)
  plot(df[,1],df[,2],xlab=names(df[1]),ylab=names(df[2]),cex=family[,6]/10)
  abline(coefs)
  title(paste('Regression of ',names(df[2]),' on ', names(df[1])))
}

###

##Question 2

#Utilizes code from last assignment

regline()
## BMI tends to get larger with increasing weight while holding height constant.
## BMI has a positive coefficient in the regression because higher BMI correlated with larger weight.
## This makes sense because BMI is calculated using a ratio of weight to height.


##Question 3

#3D plot of Weight as a function of Height and BMI
fit <- leastsquares()
plot3d( x = family[,4] , y = family[,6] , z = family[,5] , type = "p" , size = 8, xlab="Height",ylab="BMI",zlab="Weight" )
planes3d( fit[[1]][2] , fit[[1]][3] , -1 , fit[[1]][1] , alpha = 0.25 , color = "red" )


##Question 4

#Stepwise linear regression yeilding same result as Part 1
stepwiseLinreg <- function(df=family){
  Y <- df[,5]
  X <- cbind(rep(1,dim(df)[1]),df[,4],df[,6])
  M <- X[,1:2]
  N <- X[,3]
  gamma1 <- solve(t(M)%*%M)%*%t(M)%*%Y
  f <- Y - M%*%gamma1
  gamma2 <- solve(t(M)%*%M)%*%t(M)%*%N
  g <- N - M%*%gamma2
  gamma3 <- t(f)%*%g / t(g)%*%g
  B_hat <- rbind(gamma1-gamma2%*%gamma3, gamma3)
  return(B_hat)
}