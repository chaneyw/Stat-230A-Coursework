setwd('C:/Users/Wes/Desktop/Stat 230A/')
library('Matrix')
library('parcor')
library('plsdof')
library('lars')
library('pls')
library('glmnet')

makedata=function(n,p=20,wh=15){
  X=matrix(rnorm(n*p),n,p)
  rhos <- seq(.1,.9,.2)
  blocks <- lapply(rhos,function(x){matrix(rep(x,9),3,3)})
  blocks[[6]] <- matrix(rep(0.0,25),5,5)
  G <- as.matrix(bdiag(blocks))
  diag(G) <- 1
  U <- chol(G)
  X <- X%*%U
  X <- X[,sample(ncol(X))] 
  exps=seq(-1,-2.5,length=wh)
  beta=rep(0,p)
  beta[1:wh]=exp(exps)
  Y=.5+X%*%beta+rnorm(n)
  return(data.frame(Y,X))
}

standardizeData <- function(data){
  data <- apply(data,2,function(x){(x-mean(x))/sd(x)})
}

# calcRSS <- function(model,data){
#   yhat <- predict(model)
#   ybar <- mean(data[,1])
#   r_sq <- 1-sum((data[,1]-yhat)^2)/sum((data[,1]-ybar)^2)
#   RSS <- sum((data[,1]-yhat)^2)
#   return(RSS)
# }
# 
# crossVal <- function(data,leaveout){
#   folds <- sample(rep(1:10,each=10))
#   CVmodels <- lapply(1:10,function(x){lm(Y~.,data=data[folds!=x,leaveout])})
#   CVpredict <- lapply(1:10,function(x){predict(CVmodels[[x]],newdata=data[folds==x,leaveout])})
#   CVrss <- sum(sapply(1:10,function(x){mean((data[folds==x,1]-CVpredict[[x]])^2)}))
#   return(CVrss)
# }
# 
# 
# oneRep <- function(n){  
#   data <- makedata(n)
#   colnames(data)=c("Y",letters[1:20])
#   switch=sample(20)+1
#   data=data[,c(1,switch)]
#   OLSmodel <- lm(Y~.,data=data)
#   RSSs <- calcRSS(OLSmodel,data)
#   CV_RSSs <- crossVal(data,1:21)
#   N <- dim(data)[1]
#   leaveout <- numeric()
#   curModel <- OLSmodel
#   for (i in 1:19){
#     lowT <- min(abs(coef(summary(curModel))[2:length(curModel$coefficients), "t value"]))
#     whichT <- names(which(abs(coef(summary(curModel))[2:length(curModel$coefficients), "t value"])==lowT)+1)
#     whichT <- which(names(data)==whichT)
#     leaveout <- c(leaveout,-whichT)
#     curModel <- lm(Y~.,data=data[,leaveout])
#     CV_RSSs[i+1] <- crossVal(data,leaveout)
#     RSSs[i+1] <- calcRSS(curModel,data)
#     #curP <- dim(data[,leaveout])[2]-1
#   }
#   
#   P = 20:1
#   MCP <- RSSs-N+2*P
#   BIC <- N*log(RSSs/N)+P*log(N)
#   AIC <- N*log(RSSs/N)+2*P
#   return(list(CV_RSSs,MCP,BIC,AIC))
# }
# 
# # n = 100
# cl <- makeCluster(4)
# registerDoParallel(cl)
# output100 <- foreach(icount(10000),.combine=cbind) %dopar% oneRep(100)
# 
# # n = 1000
# output1000 <- foreach(icount(10000),.combine=cbind) %dopar% oneRep(1000)
# output10000 <- foreach(icount(10000),.combine=cbind) %dopar% oneRep(10000)

# PCR CV
PCRFUN <- function(data,ncomp){
  PCRRMS <- numeric()
  cv_folds <- sample(c(rep(1:10,10)))
  for (fold in 1:10){
    model <- pls::pcr(Y~.,data=data.frame(data[cv_folds!=fold,]),ncomp)
    predictions <- predict(model,newdata=data.frame(data[cv_folds==fold,]))
    PCRRSS[fold] <- sum((data[cv_folds==fold,1]-predictions)^2)
  }
  return(PCRRSS)
}


n <- 100
BSSRSS <- numeric()
RidgeRSS <- numeric()
LassoRSS <- numeric()
PCRRSS <- numeric()
for (rep in 1:100){
  data <- makedata(n)
  data <- standardizeData(data)
  
  #Ridge CV
  model <- ridge.cv(as.matrix(data[,2:21]),data[,1])
  RidgeRSS[rep] <- mean(model$cv.error.matrix[,1])*10
  
  #Lasso CV
  lasso <- adalasso(as.matrix(data[,2:21]),data[,1],k=10,both=FALSE)
  LassoRSS[rep] <- lasso$cv.lasso*n/10
  
#   #Best subset selection - this sucks
#   subdata <- data
#   RSS <- list()
#   history <- numeric()
#   RSS[[1]] <- sum((predict(lm(Y~.,data=data))-data[,1])^2)
#   for (i in 1:19){
#     RSS[[i+1]] <- numeric()
#     for (j in 1:(ncol(subdata)-1)){
#       model <- lm(Y~.,data=subdata[,-(j+1)])
#       pred <- predict(model)
#       RSS[[i+1]][j] <- sum((pred - subdata[,1])^2)
#     }
#     remove <- -(which(RSS[[i+1]]==min(RSS[[i+1]]))+1)
#     history[i] <- which(names(data)==names(subdata)[-remove])
#     subdata <- subdata[,remove]
#   }
#   RSS[[21]] <- sum((predict(lm(Y~1,data=subdata))-data[,1])^2)
#   minRSS <- sapply(RSS,min)
#   AIC <- sapply(1:length(minRSS),function(x){1000*log(minRSS[x]/1000)-2*(22-x)})

#Best subset selection, redux
  cv_folds <- sample(c(rep(1:10,10)))
  BSSRSSs <- numeric()
  for (fold in 1:10){
    train <- data.frame(data[cv_folds!=fold,])
    test <- data.frame(data[cv_folds==fold,])
    model <- lm(Y~.,data=train)
    AICmodel <- step(model,trace=0)
    pred <- predict(AICmodel,newdata=test)
    BSSRSSs[fold] <- sum((pred - test[1,])^2)
  }
  
  BSSRSS[rep] <- mean(BSSRSSs)
  
  #PCR
  PCRCV <- list()
  for (ncomp in 1:20){
    PCRCV[[ncomp]] <- PCRFUN(data,ncomp)
  } 

  PCRRSSs <-sapply(PCRCV,mean)
  PCRRSS[rep] <- min(PCRRSSs)
}