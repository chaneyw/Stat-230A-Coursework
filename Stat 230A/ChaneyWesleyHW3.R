#Wes Chaney
#Stat 230A HW3


rm(list=ls())
setwd('C:/Users/Wes/Desktop/Stat 230A/')
load('family.rda')


#Question 1

#this function creates the data frame with the 4 variables with the correct properties

createDF<-function(){
  WT <- rnorm(100,180,40)
  B_1 <- .7*(3/40)
  intercept <- 66 - B_1*180
  eps <- rnorm(100,0,3*sqrt(1-.7^2))
  HT <- B_1*WT+intercept+eps  #calculate the sigma for epsilon empirically
  BMI <- 703*WT/HT^2
  df <- data.frame(HT,WT,BMI,eps)
  return(df)
}

createDF()

#Question 2   generate the coefficients
lm(HT~WT+BMI,data=df)


#stupid function runs the linear model but only works on this particular df keeping only B_1
linmod <- function(df=df){
  fit <- lm(HT~WT+BMI,data=df)
  return(fit$coefficients[2])
}


#Question 2    explanation
#The assumption that is violated is that the data on Y are observed values of XB + eps. The data on HT are generated from
#WT but not from BMI which is included in the model. It also violates the assumption that the data on Y are linear combinations
#of the factors in this case because of the way BMI is computed.
  
#Question 3
linmod() 

#Question 4    explanation  
#Yes the estimate is biased to be lower because the model generates HT from WT, aside from random errors, so
#WT explains more of HT than the Beta weight it is given in a model that includes a covariate which is correlated
#with both height and weight and was not involved in the generation of the data.
  
  
#Question 5
df <-createDF()
fit <- lm(HT~WT+BMI,data=df)
residuals <- df$HT - predict(fit)
par(mfrow=c(2,2))
plot(df$HT,residuals,main='Residuals as function of Height',ylab='Residuals',xlab='Height')
plot(df$WT,residuals,main='Residuals as function of Weight',ylab='Residuals',xlab='Weight')
plot(df$BMI,residuals,main='Residuals as function of BMI',ylab='Residuals',xlab='BMI')
plot(df$eps,residuals,main='Residuals as function of Error',ylab='Residuals',xlab='Errors')
  
#Are eps and HT orthogonal?  Independent?
#Changing eps changes HT so they can't be independent. They are unlikely to be orthogonal.

#Are residuals and HT orthogonal?  Independent?
#No. The predictions are orthogonal to the residuals but not the real Y values.

#Are eps and WT orthogonal?  Independent?
#They are independent by construction. Unlikely to be orthogonal.

#Are residuals and WT orthogonal?  Independent?
#They are orthogonal by construction since WT is a covariate. They are not likely to be independent.

#Are eps and BMI orthogonal?  Independent?
#They are independent by construction. Unlikely to be orthogonal.

#Are residuals and BMI orthogonal?  Independent?
#They should be orthogonal by construction of the linear model. They are not likely to be independent.

#Question 6    #this histogram shows the bias in B_1
coefs <- replicate(1000,linmod(createDF()))

hist(coefs,breaks=20,main='Histogram of B_1 values',xlab='B_1 values',xlim=c(0,.2))
abline(v=.7*(3/40),col="red")
#They are independent. They are unlikely to be orthogonal. The errors are iid and do not depend on weight.
#For them to be orthogonal would be very rare.

