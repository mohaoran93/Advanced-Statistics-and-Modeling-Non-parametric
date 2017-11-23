# Rice
# Input: 
#      X,Y  vectors

#
# Output:  
#      The estimation of sigma^2 using both methods.
# 


rice <- function(X,Y){
  # method 1 Rice
  XY <- data.frame(X,Y)
  XY = XY[order(XY[,'X']),]
  print(XY)
  t2 = XY[-c(length(X)),"Y"]
  t1 = XY[-c(1),"Y"]
  sigma_2 <- (sum((t2-t1)^2))/(2*(length(ROOM)-1))
  
  # method 2 
  summation <- 0
  n <- length(Y)
  y.hat <- c()
  residual.hat <- c()
  
  for (i in 2:(n-1)) {
    xi <- XY[i, 'X']
    
    x.previous = XY[i-1,'X'] # x_i-1
    x.following = XY[i+1,'X'] # x_i+1
    y.previous = XY[i-1,'Y'] # y_i-1
    y.following = XY[i+1,'Y'] # y_i+1
    
    #rep <- table(X)
    #repetitions <- rep[names(rep)==xi]
    
    if (x.previous == x.following) {
      xui <- XY[i-2,'X']
      xli <- XY[i+2,'X']
    } else {
      xui <- x.following
      xli <- x.previous
    }
    
    ai <- (x.following - xi)/(xui - xli)
    bi <- (xi - x.previous)/(xui - xli)
    
    y.hat[i] <- ai*y.previous + bi*y.following
    
    residual.hat[i] <- y.hat[i] - XY[i, 'Y']
    summation <- summation + (residual.hat[i]^2/(ai^2 + bi^2 + 1))
  }
  
  residual.variance <- summation/(n - 2)
  return(list(sigma_2,residual.variance))  
}
#bos.fit.loess <- loess(ROOM ~ LSTAT, boston.c)
#bos.fit.sm <- sm.regression(LSTAT,ROOM)