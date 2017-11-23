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
    
    if (x.previous == x.following) {
      x.following <- min(XY[XY$X > xi,]$X)
      x.previous <- max(XY[XY$X < xi,]$X)
    }
    
    a_i <- (x.following - xi)/(x.following - x.previous)
    b_i <- (xi - x.previous)/(x.following - x.previous)
    
    y.hat_i <- a_i*y.previous + b_i*y.following
    
    residual.hat_i <- y.hat_i - XY[i, 'Y']
    summation <- summation + (residual.hat_i^2/(a_i^2 + b_i^2 + 1))
  }
  
  gasser.sigma2 <- summation/(n - 2)
  return(list(rice.sigma_2, gasser.sigma2))  
}

#bos.fit.loess <- loess(ROOM ~ LSTAT, boston.c)
#bos.fit.sm <- sm.regression(LSTAT,ROOM)