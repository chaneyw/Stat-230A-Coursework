n=36
M <- matrix(c(1,.52,.52,1),nrow=2)
N <- matrix(c(-.26,-.42),nrow=2)
b_hat <- solve(M)%*%N
sigma <- (1-(.057^2 + .390^2 + 2*-.057*-.390*.52))*36/33



se_bhat <- sqrt(solve(M)[1]*sigma*1/n)
t <- b_hat / se_bhat
2*pt(-abs(t[1]),df=n-3)
2*pt(-abs(t[2]),df=n-3)