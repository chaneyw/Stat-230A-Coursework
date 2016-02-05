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
  
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  
  
}

oneBoot = function(data, err1, err2){
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
  
}

repBoot = function(data, B = 1000){
  
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
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

### xUnique = 1:5
### trueCoeff = c(0, 1, 1)
### myData = genData(coefs = trueCoeff, xs = xUnique)
### expt = repBoot(data = myData )
### par(mfrow = c(2, 2))
### bootPlot(data = myData, coeff = expt[[1]], trueCoeff)
### bootPlot(data = myData, coeff = expt[[2]], trueCoeff)
### bootPlot(data = myData, coeff = expt[[3]], trueCoeff)
### bootPlot(data = myData, coeff = expt[[4]], trueCoeff)

### PART 7.
### Generate data
set.seed(1)
W = rnorm(100,1:100/5)

