#' Regularized Synthetic Control
#' 
#' Created: 20 juillet 2016
#' Edited: 12 octobre 2016 
#' Returns ATT, Conditional ATT, counterfactual and n1 sets of n0 weights from regularized Synthetic Control
#' 
#' @param X0 is a p x n0 matrix
#' @param X1 is a p x n1 matrix
#' @param Y0 is a n0 x 1 vector
#' @param Y1 is a n1 x 1 vector
#' @param V is a p x p matrix of weights
#' @param pen is l1 penalty level
#' @param tol gives the threshold for considering true zeros
#' 
#' @seealso \code{\link{wsoll1}}
#' 
#' @autor Jeremy LHour

regsynth <- function(X0,X1,Y0,Y1,V,pen,tol=1e-6){
  n0 = ncol(X0); n1 = ncol(X1)

  Wsol = matrix(nrow=n1,ncol=n0)
  f = file()
  sink(file=f)
  for(i in 1:n1){
    sol = wsoll1(X0,X1[,i],V,pen)
    sol = TZero(sol,tol)
    Wsol[i,] = sol
  }
  y0_hat = Wsol%*%Y0
  tau = Y1 - y0_hat
  sink()
  close(f)
  
  return(list(ATT = mean(tau),
              CATT = tau,
              Wsol = Wsol,
              y0_hat = y0_hat))
}
