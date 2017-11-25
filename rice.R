# Rice
# Input: 
#      X,Y  vectors

#
# Output:  
#      The estimation of sigma^2 using both methods.
# 

calculate_residual_variance <- function(X,Y) {
  XY <- data.frame(X,Y)
  XY <- XY[order(XY$X),]
  rownames(XY) <- 1:nrow(XY)
  n <- length(Y)
  
  # Rice
  t2 = XY[-c(length(X)),"Y"]
  t1 = XY[-c(1),"Y"]
  rice.sigma_2 <- (sum((t2-t1)^2))/(2*(n-1))
  
  # Gasser, Sroka, and Jennen-Steinmetz
  summation <- 0
  
  for (i in 2:(n-1)) {
    xi <- XY[i, 'X']
    
    x.previous = XY[i-1,'X'] # x_i-1
    x.following = XY[i+1,'X'] # x_i+1
    y.previous = XY[i-1,'Y'] # y_i-1
    y.following = XY[i+1,'Y'] # y_i+1
    
    #rep <- table(X)
    #repetitions <- rep[names(rep)==xi]
    
    if (x.previous == x.following) {
      x.following <- min(XY[XY$X > xi,]$X)
      x.previous <- max(XY[XY$X < xi,]$X)
    }
    
<<<<<<< Updated upstream
    a_i <- (x.following - xi)/(x.following - x.previous)
    b_i <- (xi - x.previous)/(x.following - x.previous)
=======
    ai <- (xui - xi)/(xui - xli)
    bi <- (xi - xli)/(xui - xli)
>>>>>>> Stashed changes
    
    y.hat[i] <- ai*y.previous + bi*y.following
    
    residual.hat[i] <- y.hat[i] - XY[i, 'Y']
    summation <- summation + (residual.hat[i]^2/(ai^2 + bi^2 + 1))
  }
  
  gasser.sigma2 <- summation/(n - 2)
  return(list(rice.sigma_2, gasser.sigma2))
}
#bos.fit.loess <- loess(ROOM ~ LSTAT, boston.c)
#bos.fit.sm <- sm.regression(LSTAT,ROOM)