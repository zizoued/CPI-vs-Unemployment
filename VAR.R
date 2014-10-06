cpi <- read.csv("cpi2.csv")
un <- read.csv("unemployment.csv")
# from 1/1/2001 - 3/1/2014
cpi2 <- cpi[c(529:687),]
un2 <- un[c(637:795),]
cpi.ts <- ts(cpi2[,2], start=c(2001,01), end=c(2014,03), frequency=12)
un.ts <- ts(un2[,2], start=c(2001,01), end=c(2014,03), frequency=12)
# cpi.ts <- ts(cpi2[,2])
# un.ts <- ts(un2[,2])
### (1) each series inspect 
plot(cpi.ts)
plot(un.ts)
# interested in the comparison of change. 
cpi.diff <- diff(cpi.ts)
#cpi.diff <- ts(cpi.diff[1:158])
un.diff <- diff(un.ts)
#un.diff <- ts(un.diff[1:158])
plot(cpi.diff)
plot(un.diff)

fitvar1 = ar.ols(cbind(un.diff,cpi.diff), order = 5)
fitvar1
fitvar1$asy.se.coef
fitvar1$aic
plot(fitvar1$resid)
acf(fitvar1$resid[,1], na.action = na.omit)
acf(fitvar1$resid[,2], na.action = na.omit)

p = predict(fitvar1, n.ahead = 4)
plot(p$pred)