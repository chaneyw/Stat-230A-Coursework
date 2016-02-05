#HW12 for Stat 230A. Comparing the performance of Ridge, LASSO, Principle Component Regression,Regression Tree

setwd('C:/Users/Wes/Desktop/Stat 230A/')
#setwd('C:/Users/Anna/Desktop/Stat 230/')
library('Matrix')
library('parcor')
library('plsdof')
library('lars')
library('pls')
library('glmnet')
library('rpart')

makedata=function(n,p=20,wh=15){
  X=matrix(rnorm(n*p),n,p)
  rhos <- seq(.1,.9,.2)
  blocks <- lapply(rhos,function(x){matrix(rep(x,9),3,3)})
  blocks[[6]] <- matrix(rep(0.0,25),5,5)
  G <- as.matrix(bdiag(blocks))
  diag(G) <- 1
  U <- chol(G)
  X <- X%*%U
  #X <- X[,sample(ncol(X))] 
  exps=seq(-1,-2.5,length=wh)
  beta=rep(0,p)
  beta[1:wh]=exp(exps)
  Y=.5+X%*%beta+rnorm(n)
  return(data.frame(Y,X))
}

standardizeData <- function(data){
  data <- data.frame(apply(data,2,function(x){(x-mean(x))/sd(x)}))
}

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
TreeRSS <- numeric()


#Part 1
for (rep in 1:100){
  data <- makedata(n)
  data <- standardizeData(data)
  
  #Ridge CV
  model <- ridge.cv(as.matrix(data[,2:21]),data[,1])
  RidgeRSS[rep] <- mean(model$cv.error.matrix[,1])*10
  
  #Lasso CV
  lasso <- adalasso(as.matrix(data[,2:21]),data[,1],k=10,both=FALSE)
  LassoRSS[rep] <- lasso$cv.lasso*n/10

  #Best subset selection, redux
  cv_folds <- sample(c(rep(1:10,10)))
  BSSRSSs <- numeric()
  for (fold in 1:10){
    train <- data.frame(data[cv_folds!=fold,])
    test <- data.frame(data[cv_folds==fold,])
    model <- lm(Y~.,data=train)
    AICmodel <- step(model,trace=0)
    pred <- predict(AICmodel,newdata=test)
    BSSRSSs[fold] <- sum((pred - test[,1])^2)
  }
  
  BSSRSS[rep] <- mean(BSSRSSs)
  
  #Regression trees
  model <- rpart(Y~.,data=data,method='anova')
  TreeRSS[rep] <- min(model$cptable[,"xerror"])*10
    #model$cptable[which.min(model$cptable[,"xerror"]),"CP"]*10
  
  #PCR
  PCRCV <- list()
  for (ncomp in 1:20){
    PCRCV[[ncomp]] <- PCRFUN(data,ncomp)
  } 

  PCRRSSs <-sapply(PCRCV,mean)
  PCRRSS[rep] <- min(PCRRSSs)
}

png('Figure1.png')
boxplot(LassoRSS,RidgeRSS,TreeRSS,PCRRSS,BSSRSS,ylab='CV RSS',names=c('Lasso','Ridge','Reg Tree','PCR','Best Subset'),main='Part 1')
dev.off()




#Part 2

rm(list=ls())

makedata2=function(n,p=20,wh=15){
  X=matrix(rnorm(n*p),n,p)
  exps=seq(-1,-2.5,length=wh)
  beta=rep(0,p)
  beta[1:wh]=exp(exps)
  Y=.5+X%*%beta+rnorm(n)
  return(data.frame(Y,X))
}

standardizeData <- function(data){
  data <- data.frame(apply(data,2,function(x){(x-mean(x))/sd(x)}))
}

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
TreeRSS <- numeric()

for (rep in 1:100){
  data <- makedata2(n)
  data <- standardizeData(data)
  
  #Ridge CV
  model <- ridge.cv(as.matrix(data[,2:21]),data[,1])
  RidgeRSS[rep] <- mean(model$cv.error.matrix[,1])*10
  
  #Lasso CV
  lasso <- adalasso(as.matrix(data[,2:21]),data[,1],k=10,both=FALSE)
  LassoRSS[rep] <- lasso$cv.lasso*n/10
  
  #Best subset selection, redux
  cv_folds <- sample(c(rep(1:10,10)))
  BSSRSSs <- numeric()
  for (fold in 1:10){
    train <- data.frame(data[cv_folds!=fold,])
    test <- data.frame(data[cv_folds==fold,])
    model <- lm(Y~.,data=train)
    AICmodel <- step(model,trace=0)
    pred <- predict(AICmodel,newdata=test)
    BSSRSSs[fold] <- sum((pred - test[,1])^2)
  }
  
  BSSRSS[rep] <- mean(BSSRSSs)
  
  #Regression trees
  model <- rpart(Y~.,data=data,method='anova')
  TreeRSS[rep] <- min(model$cptable[,"xerror"])*10
  #model$cptable[which.min(model$cptable[,"xerror"]),"CP"]*10
  
  #PCR
  PCRCV <- list()
  for (ncomp in 1:20){
    PCRCV[[ncomp]] <- PCRFUN(data,ncomp)
  } 
  
  PCRRSSs <-sapply(PCRCV,mean)
  PCRRSS[rep] <- min(PCRRSSs)
}

png('Figure2.png')
boxplot(LassoRSS,RidgeRSS,TreeRSS,PCRRSS,BSSRSS,ylab='CV RSS',names=c('Lasso','Ridge','Reg Tree','PCR','Best Subset'),main='Part 2')
dev.off()

#Part 3
#1-(i+j)/10

rm(list=ls())

makedata3=function(n,p=20,wh=15){
  X=matrix(rnorm(n*5),n,5)
  G <- matrix(rep(0,25),5,5)
  G[lower.tri(G)]<-c(.7,.6,.5,.4,.5,.4,.3,.3,.2,.1)
  G<-t(G)
  G[lower.tri(G)]<-c(.7,.6,.5,.4,.5,.4,.3,.3,.2,.1)
  diag(G)<-1
  U <- chol(G)
  X <- X%*%U
  X2 <- X^2
  X3 <- cbind(sapply(1:4,function(x){X[,x]*X[,x+1]}),sapply(1:3,function(x){X[,x]*X[,x+2]}),sapply(1:2,function(x){X[,x]*X[,x+3]}),X[,4]*X[,5])
  X <- cbind(X,X2,X3)
  X <- X[,sample(ncol(X))]
  exps=seq(-1,-2.5,length=wh)
  beta=rep(0,p)
  beta[1:wh]=exp(exps)
  Y=.5+X%*%beta+rnorm(n)
  return(data.frame(Y,X))
}

standardizeData <- function(data){
  data <- data.frame(apply(data,2,function(x){(x-mean(x))/sd(x)}))
}

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
TreeRSS <- numeric()

for (rep in 1:100){
  data <- makedata3(n)
  data <- standardizeData(data)
  
  #Ridge CV
  model <- ridge.cv(as.matrix(data[,2:21]),data[,1])
  RidgeRSS[rep] <- mean(model$cv.error.matrix[,1])*10
  
  #Lasso CV
  lasso <- adalasso(as.matrix(data[,2:21]),data[,1],k=10,both=FALSE)
  LassoRSS[rep] <- lasso$cv.lasso*n/10
  
  #Best subset selection, redux
  cv_folds <- sample(c(rep(1:10,10)))
  BSSRSSs <- numeric()
  for (fold in 1:10){
    train <- data.frame(data[cv_folds!=fold,])
    test <- data.frame(data[cv_folds==fold,])
    model <- lm(Y~.,data=train)
    AICmodel <- step(model,trace=0)
    pred <- predict(AICmodel,newdata=test)
    BSSRSSs[fold] <- sum((pred - test[,1])^2)
  }
  
  BSSRSS[rep] <- mean(BSSRSSs)
  
  #Regression trees
  model <- rpart(Y~.,data=data,method='anova')
  TreeRSS[rep] <- min(model$cptable[,"xerror"])*10
  #model$cptable[which.min(model$cptable[,"xerror"]),"CP"]*10
  
  #PCR
  PCRCV <- list()
  for (ncomp in 1:20){
    PCRCV[[ncomp]] <- PCRFUN(data,ncomp)
  } 
  
  PCRRSSs <-sapply(PCRCV,mean)
  PCRRSS[rep] <- min(PCRRSSs)
}

png('Figure3.png')
boxplot(LassoRSS,RidgeRSS,TreeRSS,PCRRSS,BSSRSS,ylab='CV RSS',names=c('Lasso','Ridge','Reg Tree','PCR','Best Subset'),main='Part 2')
dev.off()


#Part 4

rm(list=ls())

# makedata4=function(n,p=9,wh=9){
#   X=matrix(rnorm(n*p),n,p)
#   rhos <- c(.1,.4,.7)
#   blocks <- lapply(rhos,function(x){matrix(rep(x,9),3,3)})
#   G <- as.matrix(bdiag(blocks))
#   diag(G) <- 1
#   U <- chol(G)
#   X <- X%*%U
#   X <- X[,sample(ncol(X))] 
#   exps=seq(-1,-2.5,length=wh)
#   beta=rep(0,p)
#   beta[1:wh]=exp(exps)
#   Y=.5+X%*%beta+rnorm(n)
#   return(data.frame(Y,X))
# }

# makedata4=function(n,p=20,wh=15){
#   X <- matrix(rnorm(n*5),n,5)
#   X2 <- cbind(sapply(1:4,function(x){X[,x]+X[,x+1]*3}),sapply(1:3,function(x){X[,x]+X[,x+2]*2}),sapply(1:2,function(x){X[,x]+X[,x+3]*1}),X[,4]+X[,5])
#   X3 <- cbind(X2[,3]*2+X[,2]+X[,5]*3,X[,4]+X2[,1],X2[,3]*X[,4],X2[,1]*X2[,3],X[,2]*4+X2[,4])
#   X <- cbind(X,X2,X3)
#   X <- X[,sample(ncol(X))]
#   exps=seq(-1,-2.5,length=wh)
#   beta=rep(0,p)
#   beta[1:wh]=exp(exps)
#   Y=.5+X^2%*%beta+rnorm(n)
#   return(data.frame(Y,X))
# }
makedata4=function(n,p=20,wh=15){
  X=matrix(rnorm(n*5),n,5)
  X2 = X^2
  X3 = X^3
  X4 = X*4+2
  X<-cbind(X,X2,X3,X4)
  exps=seq(-1,-2.5,length=wh)
  beta=rep(0,p)
  beta[1:wh]=4
  Y=.5+X%*%beta+rnorm(n)
  return(data.frame(Y,X))
}

standardizeData <- function(data){
  data <- data.frame(apply(data,2,function(x){(x-mean(x))/sd(x)}))
}

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
TreeRSS <- numeric()

for (rep in 1:100){
  data <- makedata4(n)
  data <- standardizeData(data)
  
  #Ridge CV
  model <- ridge.cv(as.matrix(data[,2:21]),data[,1])
  RidgeRSS[rep] <- mean(model$cv.error.matrix[,1])*10
  
  #Lasso CV
  lasso <- adalasso(as.matrix(data[,2:21]),data[,1],k=10,both=FALSE)
  LassoRSS[rep] <- lasso$cv.lasso*n/10
  
  #Best subset selection, redux
  cv_folds <- sample(c(rep(1:10,10)))
  BSSRSSs <- numeric()
  for (fold in 1:10){
    train <- data.frame(data[cv_folds!=fold,])
    test <- data.frame(data[cv_folds==fold,])
    model <- lm(Y~.,data=train)
    AICmodel <- step(model,trace=0)
    pred <- predict(AICmodel,newdata=test)
    BSSRSSs[fold] <- sum((pred - test[,1])^2)
  }
  
  BSSRSS[rep] <- mean(BSSRSSs)
  
  #Regression trees
  model <- rpart(Y~.,data=data,method='anova')
  TreeRSS[rep] <- min(model$cptable[,"xerror"])*10
  #model$cptable[which.min(model$cptable[,"xerror"]),"CP"]*10
  
  #PCR
  PCRCV <- list()
  for (ncomp in 1:20){
    PCRCV[[ncomp]] <- PCRFUN(data,ncomp)
  } 
  
  PCRRSSs <-sapply(PCRCV,mean)
  PCRRSS[rep] <- min(PCRRSSs)
}

