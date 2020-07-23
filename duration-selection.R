#' Duration model with selection
#' @author Jérémy L'Hour
#' 23/07/2020
#' Lié avec projet RSVERO2
#' Voir effet de sélection par rapport à l'année de mise en circulation.

### Test ML for duration models: simple example
### Jeremy L'Hour
### 21/01/2020

rm(list=ls())
set.seed(30031987)

library("MASS")

### 0. DGP
DurationSim <- function(n=2000,p=50){
  ### Coefficient
  theta <- rep(0,p)
  for(j in 1:p){
    theta[j] <- 1 / j
  }
  theta = -log(10*theta)
  
  ### Simulate X
  mise_en_cir <- sample(1:p, n, replace=T) # annee de mise en circulation
  X = matrix(nrow=n, ncol=p)
  for(j in 1:p){
    X[,j] = 1*(mise_en_cir == j)
  }  
  
  ### Simulate Duration
  u_duration <- runif(n)
  param <- exp(X%*%theta)
  duration <- -log(1-u_duration)/param
  
  ### Simulate censoring  
  u_censor <- runif(n)
  censor <- -log(1-u_duration)/.1
  
  y <- apply(cbind(duration,censor),1,min)
  d <- 1*(duration<censor)
  
  ### Simulate selection
  ### On fait comme si on n'avait que les voitures qui ne sont pas mortes avant l'année 5
  ### ou qui sont nées après
  index_ok = (mise_en_cir+duration>4)
  
  return(list(X=X[index_ok,],
              y=y[index_ok],
              d=d[index_ok],
              theta=theta))
}

### 1. Loss functions
### A. Standard
LogLik_duration <- function(theta,y,d,X){
  return(as.numeric(mean(exp(X%*%theta)*y - d *(X%*%theta))))
}

LogLik_grad <-function(theta,y,d,X){
  f_val = diag(c((exp(X%*%theta)*y - d))) %*% X
  return(apply(f_val,2,mean))
}

### B. Avec sélection
LogLik_duration_selec <- function(theta,y,d,X,c=4){
  neg_LL = mean(exp(X%*%theta)*y - d *(X%*%theta))
  mise_en_cir = apply(X,1,function(x) which(x==1))
  borne_inf = c-mise_en_cir
  borne_inf[borne_inf<0] = 0
  neg_LL_selec = mean(-borne_inf*exp(X%*%theta))
  return(as.numeric(neg_LL+neg_LL_selec))
}

LogLik_selec_grad <-function(theta,y,d,X,c=4){
  mise_en_cir = apply(X,1,function(x) which(x==1))
  borne_inf = c-mise_en_cir
  borne_inf[borne_inf<0] = 0
  f_val = diag(c((exp(X%*%theta)*y - exp(X%*%theta)*borne_inf - d))) %*% X
  return(apply(f_val,2,mean))
}

### 2. Test

### A. Sans prise en compte de la sélection
data_train <- DurationSim(n=10000,p=10)

theta_0 = rep(0, ncol(data_train$X))

result = optim(theta_0, LogLik_duration, gr = LogLik_grad,
               y = data_train$y, d = data_train$d, X = data_train$X,
               method = c("BFGS"))

### B. Avec prise en compte de la sélection
result_selec = optim(theta_0, LogLik_duration_selec, gr = LogLik_selec_grad,
                     y = data_train$y, d = data_train$d, X = data_train$X,
                     method = c("BFGS"))

collect_res = t(rbind(data_train$theta,
                      result$par,
                      result_selec$par))
colnames(collect_res) = c("Truth", "ML no selec", "ML selec")

collect_res = 1/exp(collect_res) # affichage en terme d'esperance

print(collect_res)

### On voit que le fait de ne pas prendre en comtpe la sélection aboutit à une 
### surestimation de la durée de vie espérée pour les vieux véhicules

