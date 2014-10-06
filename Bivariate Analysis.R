library(astsa)

################### cpi and unemployment. monthly data
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
# 2008 - 2010. significant spike in unemployement and decrease in dpi. economic crisis in 2008. 
acf(ts.union(cpi.diff, un.diff))
acf2(cpi.diff)
acf2(un.diff)
# cpi.diff : MA(1) - New one looks like ARMA
# unemployment.diff: AR(5) - New one looks like ARMA

### (2) relationship between two 
cpi2.ts <- ts(cpi2[,2])
un2.ts <- ts(un2[,2])
cpi2.diff <- diff(cpi2.ts)
un2.diff <- diff(un2.ts)
ccf(un2.diff, cpi2.diff, main = " CCF of Unemployment rates and CPI")
ccfvalues = ccf(un2.diff, cpi2.diff)
ccfvalues
# significant at h = -15,-12,-11 esp.15(15months here - one year!)
# un lags cpi 
lag2.plot(un.diff, cpi.diff, 15)
lag2.plot(cpi.diff, un.diff, 15)

### (3) regression model. 1st suggestion
lm = lm(cpi.diff ~ un.diff)
summary(lm)
plot.ts(lm$resid)
acf2(lm$resid)
lag1.plot(lm$resid)
qqnorm(lm$resid)
qqline(lm$resid)
#fit good. normal assumption good. resid: MA(1)
# lm = lm(un.diff ~ cpi.diff)
# summary(lm)
# plot.ts(lm$resid)
# acf2(lm$resid)
# lag1.plot(lm$resid)
# qqnorm(lm$resid)
# qqline(lm$resid)
# once the serial correlation accounted for.. 
s11 <- arima(cpi.diff, order=c(0,0,0), xreg=un.diff)
s11
s12 <- arima(cpi.diff, order=c(0,0,1), xreg=un.diff)
s12
tsdiag(s12)
acf2(s12$resid)
acf2(s11$resid)
# confimrs that MA(1) is better. so final model would be the one with resid ~ MA(1)


### (4) 2nd suggestion. according to the ccf, h=2 would be the best decision. 
# s21 =arima(cpi.diff[3:158], order=c(0,0,0), xreg=un.diff[1:156])
# s21
# acf2(s21$resid)

s00 =arima(cpi.diff[16:158], order=c(0,0,0), xreg=un.diff[1:143])
s00
acf2(s00$resid)
tsdiag(s00)

s11 =arima(cpi.diff[16:158], order=c(2,0,2), xreg=un.diff[1:143])
s11
acf2(s11$resid)
tsdiag(s11)

s12 =arima(cpi.diff[16:158], order=c(1,0,2), xreg=un.diff[1:143])
s12
acf2(s12$resid)
tsdiag(s12)

# S11 works the best
# s22 = arima(cpi.diff[3:158], order=c(0,0,1), xreg=un.diff[1:156])
# s22
# tsdiag(s22)
# acf2(s22$resid)
# confirms that MA(1) is better. also ~MA(1). but this time, lag=2. but previou one shows a very slight better AIC

# SPECTRAL

w<-cbind(un.diff, cpi.diff) 
spec<-spectrum(w,log = "no", spans = c(3,7,9),main="Unemployment (solid line) and CPI (dashed)") 

plot(spec,plot="coh") 
plot(spec,plot="pha") 
abline(h=0)
spec$df
f = qf(.999, 2, spec$df-2)
C = f/(18+f) 


monthplot(un.ts)
monthplot(cpi.ts)
monthplot(un.diff)
monthplot(cpi.diff)

#Lagged Regression - Pre Whitening Model
l.fit<-arima(un.diff,order=c(1,0,1))#fit ARMA(1,1) to un
s.prewhiten<-KalmanRun(cpi.diff-mean(cpi.diff), makeARIMA(phi=l.fit$model$phi, theta=l.fit$model$theta,Delta=0))$resid#pre-whiten the demeaned Ds, Delta is 

# to test it's working: the pre-whitened 
# diff(lead) should be the residuals! 
l.prewhiten<-KalmanRun(un.diff-l.fit$coef["intercept"], makeARIMA(phi=l.fit$model$phi, theta=l.fit$model$theta,Delta=0) )$resid 
plot(l.prewhiten-l.fit$resid)
ccf(s.prewhiten,l.fit$resid)

#VAR Modelling
plot(w)
acf(w)
plot(w^2)
cpgram(w[,1])
cpgram(w[,2])
w=TSdata(output=w) 
VAR1<-estVARXls(w, max.lag = 15)
VAR1
checkResiduals(VAR1) 
grangertest(un.diff,cpi.diff, order =15) 
informationTests(VAR1)
stability(VAR1) 
VAR1$est$cov 
names(VAR1)
m1=estVARXls(window(w,end=c(2014,3)))
pr=forecast(m1, conditioning.inputs=inputData(w))
tfplot(pr)
