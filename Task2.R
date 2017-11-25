load("~/Documents/F2017/UPC/ASM_nonpara/Rproject/Non-parameter_Task1/boston.Rdata")
#ROOM against LSTAT
library(sm)
#df = as.data.frame(boston.c)
#df = df[order(df$LSTAT)]

df = boston.c[order(boston.c$LSTAT),]


t2 = df[-c(length(ROOM)),"RM"]
t1 = df[-c(1),"RM"]

sigma_2 <- (sum((t2-t1)^2))/(2*(length(ROOM)-1))

#print(sigma_2)


bos.fit.loess <- loess(ROOM ~ LSTAT, boston.c)
bos.fit.sm <- sm.regression(LSTAT,ROOM)


sigma_2_loess <- var(bos.fit$residuals)
# mean((bos.fit$residuals-mean(bos.fit$residuals))^2)
sigma_2_sm <- bos.fit.sm$sigma