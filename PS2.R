##################### Problem Set 2 #####################
### Question 4 ###
# i)
set.seed(4412)
N = 1500
K = 5
Xtilde = matrix(rnorm(N*K, 0, 1), N, K) # Creating a N*K x1 matrix of random independent standard Normals and turning them into a N x K matrix

X1 = Xtilde[,1]
X2 = Xtilde[,2] + X1
X3 = Xtilde[,3] + X2
X4 = Xtilde[,4] + X3
X5 = Xtilde[,5] + X4

X <- cbind(X1, X2, X3, X4, X5)

EX <- c()
for (i in 1:K) {
  EX <- cbind(EX, mean(X[,i]))
}
print(EX)

VX <- matrix(c(var(X1), cov(X1, X2), cov(X1, X3), cov(X1, X4), cov(X1, X5), cov(X1, X2), var(X2),  cov(X2, X3), cov(X2, X4), cov(X2, X5), cov(X1, X3), cov(X2, X3), var(X3),cov(X3, X4), cov(X3, X5), cov(X1, X4), cov(X2, X4),cov(X3, X4), var(X4), cov(X4, X5), cov(X1, X5), cov(X2, X5),cov(X3, X5), cov(X4, X5),  var(X5)), K, K)
print(VX)

# ii)
myfn <- function(N, B, X) {

  X <- X[1:N,]
  K <- ncol(X)
  beta0 <- matrix(rep(1, K), ncol = 1)
  
  sigma_saqured_hat <- c()
  s_squared <- c()
  t1 <- c()
  t2 <- c()
  for (b in 1:B) {
    e_b <- matrix(rnorm(N, 0, sqrt(5)))
    Y_b <-X %*% beta0 + e_b
    beta_b_hat <- solve(t(X) %*% X) %*% t(X) %*% Y_b
    betahat <- matrix(beta_b_hat, ncol = 1)
    e_b_hat <- Y_b - X %*% betahat
    
    sumb <- 0
    for (i in 1:N) {
      sumb <- sumb + (e_b_hat[i,])^2
    }
    sigma_b_squared_hat <- sumb/N
    sigma_saqured_hat <- cbind(sigma_saqured_hat, sigma_b_squared_hat)
    
    s_b_squared <- sumb/(N-K)
    s_squared <- cbind(s_squared, sigma_saqured_hat)
    
    t1_b <- (betahat[1,]-1)/sqrt(sigma_b_squared_hat * (t(X) %*% X)^(-1))
    t1 <- cbind(t1, t1_b)
    
    t2_b <- (betahat[1,]-1)/sqrt(s_b_squared * (t(X) %*% X)^(-1))
    t2 <- cbind(t2, t2_b)
  }
  var_residuals <- matrix(c(sigma_saqured_hat, s_squared), ncol = 2)
  t_statistics  <- matrix(c(t1, t2), ncol = 2)
  
  return(list(beta_hat = betahat, residualsvar = var_residuals, tstat = t_statistics))

}

#iii)
part3 = myfn(1500, 2000, X)

#iv)
N = 50
N50 = myfn(N, 2000, X)
print(mean(N50$residualsvar[,1]))
print(mean(N50$residualsvar[,2]))

N = 100
N100 = myfn(N, 2000, X)
print(mean(N100$residualsvar[,1]))
print(mean(N100$residualsvar[,2]))

N = 500
N500 = myfn(N, 2000, X)
print(mean(N500$residualsvar[,1]))
print(mean(N500$residualsvar[,2]))

N = 1000
N1000 = myfn(N, 2000, X)
print(mean(N1000$residualsvar[,1]))
print(mean(N1000$residualsvar[,2]))

N = 1250
N1250 = myfn(N, 2000, X)
print(mean(N1250$residualsvar[,1]))
print(mean(N1250$residualsvar[,2]))

#As n goes larger, the estimators are more close to the true value.