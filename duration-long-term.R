#' Survival analysis with long term survivors
#' 14/08/2020
#' @author Jérémy LHour


rm(list=ls())
set.seed(30031987)

library("MASS")
library("survival")
library("glmnet")
library("hdnom")

### 0. DGP
survival_lt <- function(x,lambda=1,p=.1,OR=0){
  #' @param x point at which to evaluate the function, x>0
  #' @param lambda parameter of the exponential distribution for baseline
  #' @param p probability of being susceptible (not a LT survivor)
  #' @param OR individual risk factor, X'beta
  return(exp(-exp(OR)*(lambda*x - log(exp(lambda*x)*(1-p) + p))))
}


duration_lt_dgp <- function(n=2000, p=50, proba=.9, rho=.5){
  #' @param n sample size
  #' @param p dimension of X
  #' @param proba_LT probability of not being a long-term surviror, ie duration < +\infty
  #'  see article by Zhou and Zhaou (2006) 

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
  theta <- c(.01,1*theta)
  
  ### Simulate Duration
  u_duration <- runif(n)
  param <- exp(X%*%theta)
  duration <- -log(1-u_duration)/param
  
  ### Compute proba of being a LT
  LT_proba = (1-proba)^param
  duration[runif(n)<LT_proba] = Inf
  
  ### Simulate censoring  
  u_censor <- runif(n)
  censor <- -log(1-u_duration)/.1
  
  y <- apply(cbind(duration,censor),1,min)
  d <- 1*(duration<censor)
 
  
  return(list(X=X,
              y=y,
              d=d,
              theta=theta))
}

get_survival_proba <- function(surv_curve,t,risk,p_never=0){
  if(length(t)!=length(risk)) stop("age and risk not of same dimension!")
  if(any(is.na(t))) warning("NAs in timevector.")
  
  start_time = Sys.time()
  t0 = mapply(function(x) min(surv_curve$times[surv_curve$times>x]),t) # on recupere la duree la plus proche de celle observee (par la droite)
  t0 = ifelse(t0==Inf,max(surv_curve$times),t0) # si la duree obsvervee depasse les possibles, alors on prend le max
  
  indices = vector(length=length(t))
  indices[!is.na(t0)] = mapply(function(x) which(surv_curve$times==x),t0[!is.na(t0)]) # Attnion: distinguer les NA sinon ça retourne une liste
  indices[is.na(t0)] = NA
  print(paste("Time elapsed -- ",Sys.time()-start_time))
  
  return(surv_curve$base_surv[indices]^risk-p_never)
}


### Simulation
data = duration_lt_dgp(n=2000, p=5, proba=.9)

surv_duration = Surv(time = data$y, event = data$d)
km_estimator = survfit(surv_duration~1,type='kaplan-meier',conf.type='log')
plot(km_estimator)

### Fit Cox model
Cox_fit = coxph(surv_duration ~ data$X)

linear_prediction = predict(Cox_fit, as.data.frame(data$X), type="lp")
surv_curve0 = glmnet_basesurv(time=data$y, event=data$d, lp=linear_prediction, centered = FALSE)

# Get survival proba
rel_risk = predict(Cox_fit, as.data.frame(data$X), type="risk")
proba_surv = get_survival_proba(surv_curve0, data$y, rel_risk)

# Get proba of being a long-term survivor
p_hat = 1 - get_survival_proba(surv_curve0, Inf, 1)
proba_Inf = (1-p_hat)^rel_risk

# Get proba of dying at some point in the future
proba_dead = proba_surv - proba_Inf

plot(data$y,proba_dead, pch=16)

plot(data$y,proba_Inf, pch=16)

