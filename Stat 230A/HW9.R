#Wes Chaney
#Stat 230A
#HW9


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
solve(t(L2)%*%solve(t(Z)%*%Z)%*%L2)%*%t(L2)%*%solve(t(Z)%*%Z)%*%as.matrix(Y2)


#sum(corrs[,10]^2)+t(Bivls1)%*%solve(t(M2)%*%M2)%*%Bivls1-2*corrs[idx,10]%*%Bivls1
sqrt(1+t(Bivls1)%*%solve(t(M2)%*%M2)%*%Bivls1-2*corrs[idx,10]%*%Bivls1)
