#' Matching function
#' 
#' Returns a vector of weights for each control unit:
#' 1/m for the m closest to X1, and 0 otherwise.
#' 
#' Edited: 12 octobre 2016
#' 
#' @param X0 is a p x n0 matrix
#' @param X1 is a p x 1 vector
#' @param V is a p x p matrix of weights
#' @param m number of neighbors to find
#' 
#' @author Jeremy LHour

matching <- function(X0,X1,V,m=3){
  n = ncol(X0)
  Delta = matrix(t(X1)%*%V%*%X1, nrow=n, ncol=1) - 2*t(X0)%*%V%*%X1 + diag(t(X0)%*%V%*%X0) # Compute the distance
  r = rank(Delta)
  
  # I take the first m
  sol = ifelse(r <= m,1/m,0)
  return(sol)
}
