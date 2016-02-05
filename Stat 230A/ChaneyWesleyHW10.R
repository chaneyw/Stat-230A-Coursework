#Wesley Chaney
#Stat 230A HW 10


setwd('C:/Users/Wes/Desktop/Stat 230A/')
load('HW10.rda')
OLSmodel <- lm(Y~.,data=data)

calcMSE <- function(model){
  yhat <- predict(model)
  ybar <- mean(data[,1])
  r_sq <- 1-sum((data[,1]-yhat)^2)/sum((data[,1]-ybar)^2)
  MSE <- mean((data[,1]-yhat)^2)
  return(MSE)
}

crossVal <- function(data,leaveout){
  folds <- rep(1:10,each=10) #sample(rep(1:10,each=10))
  CVmodels <- lapply(1:10,function(x){lm(Y~.,data=data[folds!=x,])})
  CVpredict <- lapply(1:10,function(x){predict(CVmodels[[x]],newdata=data[folds==x,])})
  CVmse <- mean(sapply(1:10,function(x){mean((data[folds==x,1]-CVpredict[[x]])^2)}))
  return(CVmse)
}

MSEs <- calcMSE(OLSmodel)
CV_MSEs <- crossVal(data,-1)

leaveout <- -1
curModel <- OLSmodel
for (i in 1:19){
  lowT <- min(abs(coef(summary(curModel))[2:length(curModel$coefficients), "t value"]))
  whichT <- which(abs(coef(summary(curModel))[2:length(curModel$coefficients), "t value"])==lowT)+1
  #leaveout <- c(leaveout,-whichT)
  data[,whichT] <- NULL
  #curModel <- lm(data[,1]~.,data=data[,leaveout])
  curModel <- lm(Y~.,data=data)
  CV_MSEs[i+1] <- crossVal(data,-1)
  MSEs[i+1] <- calcMSE(curModel)
}

Ys <- as.numeric(data[,1])
MSEs[21] <- mean((Ys-mean(Ys))^2)
folds <- sample(rep(1:10,each=10))
CVpredict <- lapply(1:10,function(x){mean(Ys[folds!=x],1)})
CV_MSEs[21] <- mean(sapply(1:10,function(x){mean((Ys[folds==x]-CVpredict[[x]])^2)}))


num_vars <- seq(20,0,-1)
plot(num_vars,CV_MSEs,type='l',main='MSE and CV MSE by # of Predictors',ylab='MSE',xlab='Number of Predictors',col='red',ylim=c(.5,2))
lines(num_vars,MSEs, col="blue")
legend('bottomleft',c('CV MSE','MSE'),fill=c('red','blue'))

which(CV_MSEs==min(CV_MSEs)) #9
#so the CV error is minimized when there are 21-9 = 12 predictors remaining in the model.