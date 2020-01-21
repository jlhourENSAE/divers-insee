### Test ML for duration models: simple example
### Jeremy L'Hour
### 21/01/2020

rm(list=ls())
set.seed(30031987)

### 0. Data generating process

DurationSim <- function(n=2000,p=50,rho=.5){
  library("MASS")
  
  ### Covariate variance matrix
  Sigma <- matrix(0,nrow=p, ncol=p)
  for(k in 1:p){
    for(j in 1:p){
      Sigma[k,j] <- rho^abs(k-j)
    }
  }
  
  ### Coefficient
  theta <- rep(0,p)
  for(j in 1:abs(p/2)){
    theta[j] <- 1*(-1)^(j) / j^2
  }
  
  ### Simulate X
  X <- mvrnorm(n = n, mu=rep(0,p), Sigma)
  X <- cbind(rep(1,n),X)
  theta <- c(log(.1),theta)
  
  ### Simulate Duration
  u_duration <- runif(n)
  param <- exp(X%*%theta)
  duration <- -log(1-u_duration)/param
  
  ### Simulate censoring  
  u_censor <- runif(n)
  censor <- -log(1-u_duration)/.1
  
  y <- min(duration,censor)
  d <- 1*(duration<censor)
  
  return(list(X=X,
              y=y,
              d=d,
              theta=theta))
}

### 1. Loss function
duration_ML <- function(theta,y,d,X){
  return(mean(exp(X%*%theta)*y - d *(X%*%theta)))
}

duration_pen_ML <- function(theta,y,d,X,lambda=0){
  return(mean(exp(X%*%theta)*y - d *(X%*%theta)) + lambda*(theta[-1]%*%theta[-1])) # do not penalize intercept
}

duration_grad <-function(theta,y,d,X,lambda=0){
 diag(c((exp(X%*%theta)*y - d))) %*% X + 2*nrow(X)*lambda*theta[-1] # do not penalize intercept
}

SGD <- function(y,d,X,eta=.1,tol=1e-8,Kmax=1000,lambda=1){
  n = length(y)
  theta = rep(0,ncol(X))
  k = 0
  
  repeat{
    k = k+1; print(k)
    thetaO = theta
    Delta = duration_grad(thetaO,y,d,X,lambda=lambda)
    NewOrder = sample(1:n,n)
    for(i in 1:n){
      theta = theta - eta*Delta[NewOrder[i],]
    }
    print(duration_pen_ML(theta,y,d,X,lambda=lambda))
    if(k > Kmax || abs(duration_pen_ML(thetaO,y,d,X,lambda=lambda) - duration_pen_ML(theta,y,d,X,lambda=lambda)) < tol) break
  }
  return(theta)
}

### 2. Test
data_train <- DurationSim(n=2000,p=5,rho=.5)
data_test <- DurationSim(n=500,p=5,rho=.5)

for(lambda in c(.1,1,2,3,5,100)){
  print(paste("lambda value:",lambda))
  train = SGD(data_train$y,data_train$d,data_train$X,eta=.001,tol=1e-4,Kmax=3,lambda=lambda)
  
  print(paste("Test loss:",duration_ML(train,y=data_test$y,d=data_test$d,X=data_test$X)))
}


