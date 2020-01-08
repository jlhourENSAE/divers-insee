### Simple Two-Layer Neural Network in R
### Jeremy L'Hour
### 08/01/2020

rm(list=ls())

### 1. Create Useful functions

sigmoid <- function(x) 1/(1+exp(-x))
sigmoid_diff <- function(x) sigmoid(x)*(1-sigmoid(x))

l2_loss <- function(y,y_hat) mean((y-y_hat)^2)

feedforward <- function(par,x){
  alpha1 <- par[1]; beta1 <- par[2]
  alpha2 <- par[3]; beta2 <- par[4]
  h_1 <- sigmoid(alpha1 + beta1*x)
  h_2 <- sigmoid(alpha2 + beta2*h_1)
  return(list(h_1=h_1,
              h_2=h_2))
}

backprop <- function(par,x,y){
  val <- feedforward(par,x)
  h_1 <- val$h_1
  h_2 <- val$h_2
  
  alpha1 <- par[1]; beta1 <- par[2]
  alpha2 <- par[2]; beta2 <- par[4]
  
  diff_beta2 <- -2*mean((sigmoid_diff(h_1)*t(h_1))%*%(y-h_2))
  diff_alpha2 <- -2*mean(sigmoid_diff(h_1)*(y-h_2))
  
  diff_beta1 <- 2*mean(t(x)%*%(beta2*sigmoid_diff(h_1)*sigmoid_diff(h_2)*(y-h_2)))
  diff_alpha1 <- 2*mean((beta2*sigmoid_diff(h_1)*sigmoid_diff(h_2)*(y-h_2)))
  
  alpha1 <- alpha1-diff_alpha1
  beta1 <- beta1-diff_beta1
  
  alpha2 <- alpha2-diff_alpha2
  beta2 <- beta1-diff_beta2
  
  new_par <- par
  new_par[1] <- alpha1; new_par[2] <- beta1;
  new_par[3] <- alpha2; new_par[4] <- beta2;
  
  return(new_par)
}


### 2. Simulation
n <- 100
x <- rnorm(n)
y <- 1*(runif(n) > sigmoid(x))

par0 <- c(1,-.5,-2,.3)

for(i in 1:10){
  par0 <- backprop(par0,x,y)
  y_hat <- feedforward(par0,x)$h_2
  print(l2_loss(y,y_hat))
}