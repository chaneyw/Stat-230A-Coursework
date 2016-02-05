xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with or without replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  if (rep == TRUE){
    newYs <- c(sample(y[1:10],10,replace=TRUE),sample(y[11:20],10,replace=TRUE),sample(y[21:30],10,replace=TRUE),sample(y[31:40],10,replace=TRUE),sample(y[41:50],10,replace=TRUE))
  }else{
    newYs <- c(sample(y[1:10],10),sample(y[11:20],10),sample(y[21:30],10),sample(y[31:40],10),sample(y[41:50],10))
  }
  return(newYs)
}

genBootR = function(fit, err, rep = FALSE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  err <- sample(err)
  ys <- fit+err
  return(ys)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line or a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the model with coefficients and predictions
  if (degree==1){
    model <- lm(y~x)
  }
  if (degree==2){
    model <- lm(y~x + I(x^2))
  }
  return(model)
}

oneBoot = function(data, err1, err2,fit1,fit2){
  ### data are your data (from call to getData)
  ###  err1 are errors from fit of line to data
  ###  err2 are errors from fit of quadratic to data
  ###  generate three bootstrap samples 
  ###  A. use genBootY
  ###  B. use genBootR and errors from linear fit
  ###  C. use genBootR and errors from quadratic fit
  
  ### For A, fit a line to data$x and new y's
  ### Repeat to fit a quadratic
  ### For B, fit a line to data$x and the new y's
  ### For C, fit a quadratic to data$x and new y's
  
  ### Return the coefficients from the 4 fits in a list 
  coefs <- list()
  newYs_a1 <- genBootY(data[,1],data[,2])
  newYs_a2 <- genBootY(data[,1],data[,2])
  model_a1 <- fitModel(data[,1],newYs_a1,degree=1)
  model_a2 <- fitModel(data[,1],newYs_a2,degree=2)
  newYs_b1 <- genBootR(fit1,err1)
  newYs_b2 <- genBootR(fit2,err2)
  model_b1 <- fitModel(data[,1],newYs_b1,degree=1)
  model_b2 <- fitModel(data[,1],newYs_b2,degree=2)
  coefs[[1]] <- model_a1$coefficients
  coefs[[2]] <- model_a2$coefficients
  coefs[[3]] <- model_b1$coefficients
  coefs[[4]] <- model_b2$coefficients
  return(coefs)
}

repBoot = function(data, B = 1000){
  
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  df1 <- data.frame("Int"=numeric(),"X"=numeric())
  df2 <- data.frame("Int"=numeric(),"X"=numeric(),"X^2"=numeric())
  df3 <- data.frame("Int"=numeric(),"X"=numeric())
  df4 <- data.frame("Int"=numeric(),"X"=numeric(),"X^2"=numeric())
  model1 <- fitModel(data[,1],data[,2],degree=1)
  model2 <- fitModel(data[,1],data[,2],degree=2)
  err1 <- model1$residuals
  err2 <- model2$residuals
  fit1 <- predict(model1)
  fit2 <- predict(model2)
  for (i in 1:B){
    output <- oneBoot(data,err1,err2,fit1,fit2)
    df1[i,] <- output[[1]] 
    df2[i,] <- output[[2]]
    df3[i,] <- output[[3]]
    df4[i,] <- output[[4]]
  }
  return(list(df1,df2,df3,df4))
} 

bootPlot = function(data, coeff, trueCoeff){
  ### data is the original data set
  ### coeff is a data frame from repBoot
  ### trueCoeff contains the tru coefficients that generated
  ### data
  
  ### Make a scatter plot of data
  ### Use mapply to add lines or curves for each row in coeff
  ### Use transparency
  ### Use trueCoeff to add line or curve - make it stand out
}

### Run your simulation

xUnique = 1:5
trueCoeff = c(0, 1, 1)
myData = getData(coefs = trueCoeff, xs = xUnique)
expt = repBoot(data = myData )
### par(mfrow = c(2, 2))
### bootPlot(data = myData, coeff = expt[[1]], trueCoeff)
### bootPlot(data = myData, coeff = expt[[2]], trueCoeff)
### bootPlot(data = myData, coeff = expt[[3]], trueCoeff)
### bootPlot(data = myData, coeff = expt[[4]], trueCoeff)

### PART 7.
### Generate data
library('magic')
set.seed(1)
a <- c(1,2)
b <- .3
c <- 4
W = rnorm(100,1:100/5)
K = matrix(c(1,.5,.5,2),nrow=2)
G <- K
for (i in 1:49){
  G <- adiag(G,K)
}

errors <- rnorm(100)%*%G

Y <- rep(0,102)
for (m in 1:50){
  Y[2*m+1] <- a[1] + b*Y[2*m-1] + c*W[2*m-1] + errors[2*m-1]
  Y[2*m+2] <- a[2] + b*Y[2*m] + c*W[2*m] + errors[2*m]
}

mat <- matrix(c(1,0,0,1),nrow=2)
X <- mat
for (i in 1:49){
  X <- rbind(X,mat)
}

X <- cbind(X,Y[0:100],W)

B_ols <- solve(t(X)%*%X)%*%t(X)%*%Y[3:102]
predict <- X%*%B_ols
resids <- Y[3:102]-predict

K_hat <- matrix(rep(0,4),nrow=2)
for (m in 1:50){
  pair <- resids[(2*m-1):(2*m)]
  K_hat <- K_hat + pair%*%t(pair)  
}

K_hat <- K_hat / 50

G_hat <- K_hat
for (i in 1:49){
  G_hat <- adiag(G_hat,K_hat)
}

B_fgls <- solve(t(X)%*%solve(G_hat)%*%X)%*%t(X)%*%solve(G_hat)%*%Y[3:102]

##Bootstrap

bootThis <- function(B_fgls,W,resids){
  
  a_hat <- B_fgls[1:2]
  b_hat <- B_fgls[3]
  c_hat <- B_fgls[4]
  
  resample <- sample(1:50)
  e_star <- rep(0,100)
  for (m in 1:50){
    e_star[2*m-1] <- resids[2*resample[m]-1]
    e_star[2*m] <- resids[2*resample[m]]
  }

  Y_star <- rep(0,102)
  for (m in 1:50){
    Y_star[2*m+1] <- a_hat[1] + b_hat*Y_star[2*m-1] + c_hat*W[2*m-1] + e_star[2*m-1]
    Y_star[2*m+2] <- a_hat[2] + b_hat*Y_star[2*m] + c_hat*W[2*m] + e_star[2*m]
  }

  mat <- matrix(c(1,0,0,1),nrow=2)
  
  X_star <- mat
  for (i in 1:49){
    X_star <- rbind(X_star,mat)
  }

  X_star <- cbind(X_star,Y_star[0:100],W)

  B_ols_star <- solve(t(X_star)%*%X_star)%*%t(X_star)%*%Y_star[3:102]
  predict <- X_star%*%B_ols_star
  resids_star <- Y_star[3:102]-predict

  K_hat_star <- matrix(rep(0,4),nrow=2)
  for (m in 1:50){
    pair <- resids_star[(2*m-1):(2*m)]
    K_hat_star <- K_hat_star + pair%*%t(pair)  
  }

  K_hat_star <- K_hat_star / 50

  G_hat_star <- K_hat_star
  for (i in 1:49){
    G_hat_star <- adiag(G_hat_star,K_hat_star)
  }

  B_fgls_star <- solve(t(X_star)%*%solve(G_hat_star)%*%X_star)%*%t(X_star)%*%solve(G_hat_star)%*%Y_star[3:102]
  return(list(B_fgls_star))
}