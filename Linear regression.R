### Question 2 ###
# i)
A <- c(5, 3, 2, 5, 2, 4, 3, 5, 1, 6, 1, 2, 3, 1, 4, 1)
B <- c(7, 8, 2, 3)

matrixA <- matrix(A, nrow = 4, byrow = TRUE)
matrixB <- matrix(B, nrow = 4, byrow = TRUE)

X <- solve(matrixA) %*% matrixB
X

# ii)
set.seed(4412)
n <- 200
data <- rnorm(n, mean = 4, sd = sqrt(7))
mean(data)
var(data)

# iii)
vectorX <- matrix(data, nrow = 1, byrow = TRUE)
vector1 <- matrix(rep(1, n), nrow = 200)
mean2 <- vectorX %*% vector1 / n
mean2

# iv)
s <- 0
for (i in 1:n) {
  s <- s + data[i]
}

mean3 <- s/n
mean3

### Question 3 ###
# i)
install.packages("foreign")
library(foreign)
setwd("/Users/MENGYANG/R/4412/PS1")
growthdata = read.dta("Growth.dta")
head(growthdata)

# ii)
y = growthdata$growth
X = cbind(1, growthdata$tradeshare , growthdata$yearsschool) 
colnames(X) = c("Intercept", "TradeShare", "YearsSchool")

#compute OLS coefficients
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% y  

#compute residuals
residuals <- y - X %*% beta_ols  
mean(residuals)
var(residuals)

#compute residuals e_hat directly using residual maker matrix M
M <- diag(length(y)) - X %*% solve(t(X) %*% X) %*% t(X)
e_hat <- M %*% y
mean(e_hat)
var(e_hat)

# iii) R^2
var_y <- var(y)
uncentered_R_squared <- 1 - t(e_hat) %*% e_hat / t(y) %*% y

y_bar <- matrix(rep(mean(y), 65), nrow = 65)
centered_R_squared <- 1 - t(e_hat) %*% e_hat / t(y-y_bar) %*% (y-y_bar)

# iv) Frisch-Waugh-Lowell theorem
X1 = cbind(1, growthdata$yearsschool) 
beta1 <- solve(t(X1) %*% X1) %*% t(X1) %*% y 
y_tilda <- y - X1 %*% beta1 

gamma <- solve(t(X1) %*% X1) %*% t(X1) %*% growthdata$tradeshare
X2_tilda <- growthdata$tradeshare - X1 %*% gamma

beta2 <- solve(t(X2_tilda) %*% X2_tilda) %*% t(X2_tilda) %*% y_tilda
beta2

uncentered_R2 <- 1 - t(X2_tilda) %*% X2_tilda / t(growthdata$tradeshare) %*% growthdata$tradeshare
tradeshare_mean <- matrix(rep(mean(growthdata$tradeshare), 65), nrow = 65)
centered_R2 <- 1 - t(X2_tilda) %*% X2_tilda / t(growthdata$tradeshare-tradeshare_mean) %*% (growthdata$tradeshare-tradeshare_mean)

# v)
y = growthdata$growth
X1 = cbind(1, growthdata$yearsschool) 
X0 = growthdata$tradeshare
I = diag(length(y))
M1 = I - X1 %*% solve(t(X1) %*% X1) %*% t(X1)
beta_hat_ts = solve(t(M1%*%X0) %*% (M1%*%X0)) %*% t(M1%*%X0) %*% (M1%*%y)

# vi)
X3 = growthdata$yearsschool
beta3 <- solve(t(X3) %*% X3) %*% t(X3) %*% y 
residuals3 <- y - X3 %*% beta3 
mean(residuals3)


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

author: "Meng Yang - my2763"
latex_engine: xelatex
beta_hat_ts
