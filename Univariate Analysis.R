### Unemployment Rates
library(forecast)
un <- read.csv("unemployment.csv")
un2 <- un[c(637:795),]
un.ts <- ts(un2[,2], start=c(2001,01), end=c(2014,03), frequency=12)
plot(un.ts, main = " Unemployment Rates")
monthplot(un.ts, main = "Month Plot")
plot(stl(un.ts, "periodic"), main = "Seasonal Decomposition of Time Series by Loess")


un.diff <- diff(un.ts)
plot(un.diff, main = "Difference of the Unemployment Rates")
plot(stl(un.diff, "periodic"), main = "Seasonal Decomposition of Time Series by Loess")
monthplot(un.diff)
acf2(un.diff)
x=un.diff


auto.arima(un.ts,max.p=4,max.q=4,ic="bic")

aic5 = sarima(x,1,0,1)$AIC
bic5 = sarima(x,1,0,1)$BIC

aic1=sarima(x,1,0,1,1,1,1,12)$AIC 
aic2=sarima(x,2,0,2)$AIC 
aic3=sarima(x,1,0,2)$AIC 
aic4=sarima(x,1,0,1)$AIC 
bic1=sarima(x,0,1,1)$BIC 
bic2=sarima(x,2,0,1)$BIC 
bic3=sarima(x,1,0,2)$BIC 
bic4=sarima(x,1,1,1)$BIC 

#Out of all the models ARMA(1,1) looks the best according to the AIC and BIC criteria.
# This ARMA(1,1) model is on the differences of the data.
model1 = sarima(x,1,0,1)
cpgram(model1$fit$resid, main = "CPGram for the ARMA(1,1)") 
#Prediction:

sarima.for(x, 24, 1,0,1)


#CPI 

cpi <- read.csv("cpi2.csv")
cpi2 <- cpi[c(529:687),]
cpi.ts <- ts(cpi2[,2], start=c(2001,01), end=c(2014,03), frequency=12)
plot(cpi.ts, main = " Consumer Price Index")
cpi.diff <- diff(cpi.ts)
plot(cpi.diff, main = " Differenc of CPI time series")
acf2(cpi.diff)

monthplot(cpi.ts)
plot(stl(cpi.ts, "periodic"), main = "Seasonal Decomposition of Time Series by Loess")

y = cpi.diff

aic1=sarima(y,1,0,5)$AIC 
aic2=sarima(y,1,0,4)$AIC 
aic3=sarima(y,2,0,5)$AIC 
aic4=sarima(y,1,0,5)$AIC 
bic1=sarima(y,1,0,5)$BIC 
bic2=sarima(y,1,0,4)$BIC 
bic3=sarima(y,2,0,5)$BIC 
bic4=sarima(y,1,1,1)$BIC 

model2 = sarima(y,1,0,5)
cpgram(model2$fit$resid, main = " CPGram for ARMA(1,5)") 



#SPECTRAL ANALYSIS
ker=kernel("modified.daniell", c(3,3))#compare with span above
spectrum(un.ts,ker,log = "no", main = " Unemployment smoothed periodogram")
spectrum(un.diff,ker, log = "no")
spectrum(cpi.ts,ker, log = "no",main = " CPI smoothed periodogram")
spectrum(cpi.diff,ker, log = "no")

