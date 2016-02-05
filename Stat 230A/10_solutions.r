
####################################
###                              ###
####   HOMEWORK 10: Solutions    ####
###                              ###
####################################

  # by Sean Ruddy


### WARNING: UNINTENTIONAL ERRORS MAY EXIST. IF YOU SEE ANY ERRORS PLEASE LET                                                                       
###          ME (YOUR GSI) KNOW SO I CAN UPDATE.


########
##### 1. OLS
########

## DATA ## 
  dat <- load("C:\\Users\\Public\\Documents\\School\\Teaching\\SP13\\STAT 230A\\Homeworks\\10\\Data\\HW10.rda")
  seed <- get(dat[1])
  dat <- get(dat[2])
  
## OLS FIT ## 
  Y <- as.matrix(dat[,1,drop=FALSE])
  X <- as.matrix(dat[,-1])
  lm.fit <- lm( Y~X )
  
  (mse <- mean(resid(lm.fit)^2))
  


########
##### 2. CV 
########

## CV ## 
  cvFUN <- function(x,y,K)
             {
               just.int = FALSE
               if( is.null(x) ) just.int = TRUE 
                else x <- as.matrix(x)
               y <- as.matrix(y)
               data.split <- split(sample.int(nrow(y),replace=FALSE),1:K)
               mses <- sapply( 1:length(data.split) , 
                               FUN = function( i )
                                      {
                                        wh.test <- data.split[[i]]
                                        wh.train <- do.call( "c" , data.split[-i] )
                                        if(!just.int) 
                                         {
                                           coef.train <- coef(lm( y[wh.train,] ~ x[wh.train,] ))
                                           fitted.test <- cbind(1,x[wh.test,,drop=FALSE]) %*% coef.train
                                         }  
                                        else 
                                         {
                                           coef.train <- coef(lm( y[wh.train,] ~ 1 ))
                                           fitted.test <- rep(coef.train,K)
                                         }  
                                        y.test <- y[wh.test,,drop=FALSE]
                                        mse = mean( (y.test-fitted.test)^2 )
                                        return(mse)
                                      } , simplify = TRUE )   
               return( mean(mses) )                       
             }
  set.seed(seed)
  (cvFUN( x=X , y=Y , K=10 ))
                                  
  

########
##### 3. Model Selection
########
                                 
  backMS <- function(x,y,nsteps,K,seed)
             {
               set.seed(seed)
               x <- as.matrix(x)
               y <- as.matrix(y)
               if( nsteps > ncol(x) ) stop( "'nsteps' must be <= to the number of columns of 'x'" )

               fit <- lm( y~x )

               mses = matrix( NA , nr = nsteps+1 , nc = 2 )
                colnames(mses) <- c( "OLS" , "CV" )
               mse.cv = cvFUN(x=x,y=y,K=K)
               mse.fit <- mean( resid( fit )^2 )
               mses[1,] <- c( mse.fit , mse.cv )

               wh.rm <- which.max( summary(fit)$coefficients[,"Pr(>|t|)"][-1] )
               newx = x[,-wh.rm,drop=FALSE]

               iter = 2
               while( iter <= nsteps+1 )
                {
                  if( ncol(newx)==0 ) 
                   { 
                     newx = NULL
                     fit <- lm( y~NULL )
                   }  
                  else fit <- lm(y~newx) 
                  mse.fit <- mean( resid(fit)^2 ) 
                  mse.cv = cvFUN(x=newx,y=y,K=K)
                  mses[iter,] <- c( mse.fit, mse.cv )                            
                  wh.rm <- which.max( summary(fit)$coefficients[,"Pr(>|t|)"][-1] )
                  if( !is.null(newx) ) newx = newx[,-wh.rm,drop=FALSE]
                  iter = iter + 1                          
                }
               return( mses )    
             }                               
  (MSEs <- backMS( x=X , y=Y , nsteps=ncol(X) , K = 10 , seed))
x11()
  plot( 0:ncol(X) , MSEs[,1] , col=0 , xlab = "Number of Variables in Model" , ylab = "MSE" )
   lines( ncol(X):0 , MSEs[,"OLS"] , col = "red" )
   lines( ncol(X):0 , MSEs[,"CV"] , col = "blue" )
  
  ncol(X)-which.min(MSEs[,"CV"])+1





