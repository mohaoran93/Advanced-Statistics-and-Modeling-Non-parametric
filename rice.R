# Rice
# Input: 
#      x,y  vectors

#
# Output:  
#      The estimation of sigma^2 using both methods.
# 


rice <- function(x,y){
  df <- data.frame(x,y)
  df = df[order(df[,'x']),]
  t2 = df[-c(length(ROOM)),"y"]
  t1 = df[-c(1),"y"]
  sigma_2 <- (sum((t2-t1)^2))/(2*(length(ROOM)-1))
  return(sigma_2)  
}
#bos.fit.loess <- loess(ROOM ~ LSTAT, boston.c)
#bos.fit.sm <- sm.regression(LSTAT,ROOM)