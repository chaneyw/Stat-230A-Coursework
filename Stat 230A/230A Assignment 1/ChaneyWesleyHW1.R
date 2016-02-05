###  HW 1   Wesley Chaney

###  Part 1.
###
rm(list=ls())
setwd('C:/Users/Wes/Desktop/230A Assignment 1')
load('family.rda')


### 2. Write the function regcoef() as described in the assignment.

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

### 3. Similarly, write the regline() function.

regline=function(df=family[4:5]){
  # This function plots points and regression line, doesn't need to return
  # anything.
  coefs=regcoef(df)
  plot(df[,1],df[,2],xlab=names(df[1]),ylab=names(df[2]))
  abline(coefs)
  title(paste('Regression of ',names(df[2]),' on ', names(df[1])))
}