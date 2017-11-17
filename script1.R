library(sm)
data(aircraft)
attach(aircraft)
lgWeight <-log(Weight)
plot(Yr,lgWeight)

# step 1 Fit a nonparametric regression to data (xi,yi) 
# and save the estimated values mˆ (xi).
source("lpr_visual.R")
source("locpolreg.R")
source("loc_lin_reg.R")
op<-par(mfrow=c(1,1))
llr <-loc.lin.reg(x=Yr,y=lgWeight, tg=Yr)
lines(Yr, llr$mt, col=2, lwd=2)
# TODO is this regression good enough?

# step 2 Transform the estimated residuals hat.e? = y_i - llr$mt
z_i = log((lgWeight - llr$mt)^2)

# step 3 Fit a nonparametric regression to data (Yr,z_i) and call the estimated
#function qˆ(x).
plot(Yr,z_i)
llr2 <-loc.lin.reg(x=Yr,y=z_i, tg=Yr)
lines(Yr, llr2$mt, col=2, lwd=2)

# step 4 Estimate sigma_2(x)
sigma_2 =  exp(llr2$mt)
lines(Yr, sigma_2, col=3, lwd=3)

plot(Yr,sqrt(sigma_2))