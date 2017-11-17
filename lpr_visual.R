# lpr_visual.R Local polynomial regression, visual version.
#
# Input:
#      x,y  Observed data (two (n,1) vectors)
#      h    Smoothing parameter 
#      q    degree of the local polynomial to be fitted (default: 1)
#      tg   grid of values t where the estimated regression function 
#           is evaluated (default: x)
#
# Output:
#      mtg Estimated values of the regression function at points in vector tg
#
# Use:
#      result <- lpr_visual(x,y,h,q,tg)
lpr_visual <- function(x,y,h=(max(x)-min(x))/5,q=1,tg=min(x)+(1:3)*(max(x)-min(x))/4,...){
   if (sum(diff(x)<0)>0){aux <- sort(x,index.return=TRUE); x <- x[aux$ix]; y <- y[aux$ix]}
   if (sum(diff(tg)<0)>0) tg <- sort(tg)

   n <- length(x);
   m <- length(tg);
   mtg <- numeric(m);

   locpolreg(x=x,y=y,h=h,q=q,r=0,tg=x,...);

   color.3 <- c("blue","green","red")

   for (i in seq(1,m)){
      color <- color.3[ i%%3 +1 ] # i%%3 es igual a "i modulo 3"
      Ih <- abs(x-tg[i])<h;
      ni <- sum(Ih);
      xh <- x[Ih]-tg[i];
      Dq <- matrix(1,nrow=ni,ncol=q+1);
      if (q>0){for (j in 1:q) Dq[,j+1] <- xh^j}
      Wx <- epan(xh/h)/h;
      Wm <- Wx%*%ones(1,q+1);
      Dqq <- Wm*Dq;
      Si <- solve(t(Dq)%*%Dqq)%*%t(Dqq);
      beta <- Si%*%y[Ih];
      yhat=Dq %*% beta;
      lines(x[Ih],yhat,col=color,lwd=2)
      lines(c(tg[i],tg[i]),c(0,beta[1]),col=color,lty=2)
   }
}
