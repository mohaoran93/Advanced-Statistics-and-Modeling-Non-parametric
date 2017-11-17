loc.lin.reg <- function(x, y, h=range(x)/10, tg=seq(min(x),max(x),length=51))
{
  nt <-length(tg)
  mt <-numeric(nt)
  for (i in 1:nt)
  {
    t <- tg[i]
    w.t <-dnorm(x-t,mean=0,sd=h)
    x.t <- x-t
    lm.t <-lm(y~x.t,weights=w.t)
    mt[i] <- lm.t$coefficients[1]
  }
  return(list(mt=mt,tg=tg,h=h))
}