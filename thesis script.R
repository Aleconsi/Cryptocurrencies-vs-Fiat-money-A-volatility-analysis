library(FinTS) #for function `ArchTest()`
library(rugarch) #for GARCH models
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast)
library(rugarch)
library(readr)
library(dplyr)
library(quantmod)
library(fBasics)
library(graphics)
library(MTS)
library(rmgarch)

#Prezzi valute
startDate= as.Date("2015-08-10")
endDate= as.Date("2019-12-31")
getSymbols ("BTC-USD", from= startDate, to= endDate)
getSymbols("ETH-USD",from=startDate,to=endDate)
getSymbols("EURUSD=X",from=startDate,to=endDate)
getSymbols("GBPUSD=X",from=startDate,to=endDate)

`EURUSD=X`=na.approx(`EURUSD=X`)
`GBPUSD=X`=na.approx(`GBPUSD=X`)

chartSeries(`BTC-USD`)
chartSeries(`ETH-USD`)
chart_Series(`EURUSD=X`)
chartSeries(`GBPUSD=X`)

btc_price = `BTC-USD` [,4]
colnames (btc_price) = c('btc_price')
basicStats (btc_price)
plot(btc_price)

eth_price=`ETH-USD`[,4]
colnames(eth_price)=c('eth_price')
basicStats(eth_price)
plot(eth_price)

eur_usd=`EURUSD=X`[,4]
colnames(eur_usd)=c("eur_usd")
basicStats(eur_usd)
plot(eur_usd)

gbp_usd=`GBPUSD=X`[,4]
colnames(gbp_usd)=c("gbp_usd")
basicStats(gbp_usd)
plot(gbp_usd)

#ACF - PACF Bitcoin and Ethereum
par (mfrow=c (2,1))
acf((btc_price), lag.max = NULL, main = "bitcoin")
pacf((btc_price), lag.max = NULL, main= "bitcoin")
dev.off()

par (mfrow=c (2,1))
acf((eth_price),lag.max = NULL,main="ethereum")
pacf((eth_price),lag.max = NULL,main="ethereum")
dev.off()

#ACF - PACF EUR/USD     GBP/USD
par (mfrow=c (2,1))
acf(eur_usd,lag.max = NULL,main="eur_usd")
pacf(eur_usd,lag.max = NULL,main="eur_usd")
dev.off()

par (mfrow=c (2,1))
acf(gbp_usd,lag.max = NULL,main="gbp_usd")
pacf(gbp_usd,lag.max = NULL,main="gbp_usd")
dev.off()




#Calcolo rendimenti Crypto
btc_ret= dailyReturn(`BTC-USD`)
colnames(btc_ret) = c('btc_ret')

eth_ret= dailyReturn(`ETH-USD`)
colnames(eth_ret) = c('eth_ret')
par (mfrow=c (2,1))
plot(btc_ret)
plot(eth_ret)

#Calcolo rendimenti fiat money
eur_usd_ret=dailyReturn(`EURUSD=X`)
colnames(eur_usd_ret)=c("eur_usd_ret")

gbp_usd_ret=dailyReturn(`GBPUSD=X`)
colnames(gbp_usd_ret)=c("gbp_usd_ret")



par(mfrow=c(2,1))
plot(eur_usd_ret)
plot(gbp_usd_ret)

#Analisi rendimenti crypto
basicStats(btc_ret)
basicStats(eth_ret)
par (mfrow= c (2,2))
acf((btc_ret), lag.max = NULL, main = "bitcoin")
pacf((btc_ret), lag.max = NULL, main = "bitcoin")
acf((eth_ret), lag.max = NULL, main = "ethereum")
pacf((eth_ret), lag.max = NULL, main = "ethereum")
dev.off()
ts.btc.ret<-ts(btc_ret$btc_ret)
ts.eth.ret<-ts(eth_ret$eth_ret)

#Analisi rendimenti fiat
basicStats(eur_usd_ret)
basicStats(gbp_usd_ret)
par (mfrow= c (2,2))
acf(eur_usd_ret,lag.max = NULL,main="eur-usd")
pacf(eur_usd_ret,lag.max = NULL,main="eur-usd")
acf(gbp_usd_ret,lag.max = NULL,main="gbp-usd")
pacf(gbp_usd_ret,lag.max = NULL,main="gbp-usd")
dev.off()


#Stationarity test rendimenti crypto
adf.test(btc_ret)#augmented dickey fuller test
adf.test(eth_ret)
PP.test(btc_ret)#philips perron test
PP.test(eth_ret)

#Stationarity test rendimenti fiat money
adf.test(eur_usd_ret)
adf.test(gbp_usd_ret)
pp.test(eur_usd_ret)
pp.test(gbp_usd_ret)

#Miglior modello arima per rendimenti crypto
auto.arima(ts.btc.ret, trace = T,
           stepwise = F,
           approximation = F, test = "adf", ic = "aic")

auto.arima(ts.eth.ret, trace = T,
           stepwise = F,
           approximation = F,test = "adf",ic="aic")


#Miglior modello arima per rendimenti fiat money
auto.arima(eur_usd_ret, trace = T,
           stepwise = F,
           approximation = F,test = "adf",ic="aic")

auto.arima(gbp_usd_ret, trace = T,
           stepwise = F,
           approximation = F,test = "adf",ic="aic")

#ARCH model BTC/USD
arima303.btc<-arima(ts.btc.ret,order=c(3,0,3))
coeftest(arima303.btc)
res.arima303.btc=arima303.btc$residuals
res.arima303.btc.square=res.arima303.btc^2
Box.test(res.arima303.btc.square, lag=20, type="Ljung-Box")#esiste autocorrelazione tra gli errori #verifico arch effect
ArchTest(res.arima303.btc)#verifico arch effect
par(mfrow=c(2,1))
acf(res.arima303.btc.square,lag.max = NULL,demean = TRUE)
pacf(res.arima303.btc.square,lag.max = NULL,demean=TRUE)
dev.off()
basicStats(res.arima303.btc.square)

#ARCH model ETH/USD
arima202.eth<-arima(ts.eth.ret,order = c(2,0,2))
coeftest(arima202.eth)
res.arima202.eth=arima202.eth$residuals
res.arima202.eth.square=res.arima202.eth^2
Box.test(res.arima202.eth.square,lag = 20,type = "Ljung-Box")#verifico arch effect
ArchTest(res.arima202.eth)#verifico arch effect
par(mfrow=c(2,1))
acf(res.arima202.eth.square,lag.max = NULL,demean = TRUE)
pacf(res.arima202.eth.square,lag.max = NULL,demean=TRUE)
dev.off()
basicStats(res.arima202.eth.square)

#ARCH model EUR/USD
ts.eur_usd.ret=ts(eur_usd_ret$eur_usd_ret)
ts.gbp_usd.ret=ts(gbp_usd_ret$gbp_usd_ret)

arima000.eur<-arima(ts.eur_usd.ret,order = c(0,0,0),include.mean =FALSE)
res.eur.usd=arima000.eur$residual
res.eur.usd.square=res.eur.usd^2
Box.test(res.eur.usd.square,lag = 20,type = "Ljung-Box")#verifico arch effect
ArchTest(res.eur.usd)#verifico arch effect
par(mfrow=c(2,1))
acf(res.eur.usd.square,lag.max = NULL,demean = TRUE)
pacf(res.eur.usd,lag.max = NULL,demean = TRUE)
dev.off()
basicStats(res.eur.usd.square)

#ARCH model  GBP/USD
arima103.gbp<-arima(ts.gbp_usd.ret,order = c(1,0,3))
coeftest(arima103.gbp)
res.gbp.usd=arima103.gbp$residual
res.gbp.usd.square=res.gbp.usd^2
Box.test(res.gbp.usd.square,lag = 20,type = "Ljung-Box")#verifico arch effect
ArchTest(res.gbp.usd)#verifico arch effect
par(mfrow=c(2,1))
acf(res.gbp.usd.square,lag.max = NULL,demean = TRUE)
pacf(res.gbp.usd.square,lag.max = NULL,demean = TRUE)
dev.off()
basicStats(res.gbp.usd.square)

#Specificazioni GARCH per residui BTC\USD
###SGARCH(1,1) t-student
Std.spec<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                            submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                       = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE),distribution.model = "std", start.pars = list(), fixed.pars = list())# t di student
fit.GARCH.Std<-ugarchfit(data = res.arima303.btc.square,spec = Std.spec)
show(fit.GARCH.Std)

###SGARCH(1,1) Normal distribution
NORM.spec<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,0),include.mean=F),distribution.model = "norm")
fit.GARCH.NORM<-ugarchfit(data = res.arima303.btc.square,spec=NORM.spec)
show(fit.GARCH.NORM)

###SGARCH(1,1) Skeweness Normal
SNORM.spec<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                             submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                        = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                          NULL, archex = FALSE),distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.GARCH.SNORM<-ugarchfit(data=res.arima303.btc.square,spec=SNORM.spec)
show(fit.GARCH.SNORM)

###SGARCH(1,1) Normal inverse Gaussian
NIG.spec<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                           submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                      = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                        NULL, archex = FALSE),distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.GARCH.NIG<-ugarchfit(data = res.arima303.btc.square,spec=NIG.spec)
show((fit.GARCH.NIG))

###TGARCH(1,1) t-student
spec.Std<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                              "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                     c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                     NULL, archex = FALSE), distribution.model = "std", start.pars = list(), fixed.pars = list())
fit.TGARCH.Std<-ugarchfit(data = res.arima303.btc.square,spec = spec.Std)
show(fit.TGARCH.Std)

###TGARCH(1,1) Normal distribution
spec.NORM<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                               "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                      c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                      NULL, archex = FALSE), distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.TGARCH.NORM<-ugarchfit(data = res.arima303.btc.square,spec=spec.NORM)
show(fit.TGARCH.NORM)

###TGARCH(1,1) Skewness normal
spec.SNORM <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                 "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                        c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                        NULL, archex = FALSE), distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.TGARCH.SNORM<-ugarchfit(data = res.arima303.btc.square,spec = spec.SNORM)
show(fit.TGARCH.SNORM)

###TGARCH(1,1) Negative Gaussian inverse
spec.NIG<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                              "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                     c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                     NULL, archex = FALSE), distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.TGARCH.NIG<-ugarchfit(data = res.arima303.btc.square,spec = spec.NIG)
show(fit.TGARCH.NIG)

###specificazioni per GARCH ETH/USD
#SGARCH(1,1) t-student
Std.spec2<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                             submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                        = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                          NULL, archex = FALSE),distribution.model = "std", start.pars = list(), fixed.pars = list())# t di student
fit.GARCH.Std2<-ugarchfit(data = res.arima202.eth.square,spec = Std.spec2)
show(fit.GARCH.Std2)

###SGARCH(1,1) Normal distribution
NORM.spec2<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                             submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                        = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                          NULL, archex = FALSE),distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.GARCH.NORM2<-ugarchfit(data = res.arima202.eth.square,spec= NORM.spec2)
show(fit.GARCH.NORM2)

###SGARCH(1,1) Skeweness Normal
SNORM.spec2<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                              submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                         = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                           NULL, archex = FALSE),distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.GARCH.SNORM2<-ugarchfit(data=res.arima202.eth.square,spec=SNORM.spec2)
show(fit.GARCH.SNORM2)

###SGARCH(1,1) Normal Inverse Gaussian
NIG.spec2<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                            submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                       = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE),distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.GARCH.NIG2<-ugarchfit(data = res.arima202.eth.square,spec=NIG.spec2)
show((fit.GARCH.NIG2))

###TGARCH(1,1) t-student
spec.Std2<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                               "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                      c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                      NULL, archex = FALSE), distribution.model = "std", start.pars = list(), fixed.pars = list())
fit.TGARCH.Std2<-ugarchfit(data = res.arima202.eth.square,spec = spec.Std2)
show(fit.TGARCH.Std2)

###TGARCH(1,1) Normal distribution
spec.NORM2<-fit.GEDTspec <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                               "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                                      c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                                      NULL, archex = FALSE), distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.TGARCH.NORM2<-ugarchfit(data = res.arima202.eth.square,spec=spec.NORM2)
show(fit.TGARCH.NORM2)

###TGARCH(1,1) Skewness normal
spec.SNORM2 <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                  "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                         c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE), distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.TGARCH.SNORM2<-ugarchfit(data = res.arima202.eth.square,spec = spec.SNORM2)
show(fit.TGARCH.SNORM2)

###TGARCH(1,1) Normal Inverse Gaussian
spec.NIG2<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                               "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                      c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                      NULL, archex = FALSE), distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.TGARCH.NIG2<-ugarchfit(data = res.arima202.eth.square,spec = spec.NIG2)
show(fit.TGARCH.NIG2)

#SGARCH(1,1) EUR/USD
#SGARCH(1,1) t-student
Std.spec.eur<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                           = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                             NULL, archex = FALSE),distribution.model = "std", start.pars = list(), fixed.pars = list())# t di student
fit.GARCH.Std.eur<-ugarchfit(data = res.eur.usd.square,spec = Std.spec.eur)
show(fit.GARCH.Std.eur)

###SGARCH(1,1) Normal distribution
NORM.spec.eur<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                           = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                             NULL, archex = FALSE),distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.GARCH.NORM.eur<-ugarchfit(data = res.eur.usd.square,spec=NORM.spec.eur)
show(fit.GARCH.NORM.eur)

###SGARCH(1,1) Skeweness normal
SNORM.spec.eur<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                 submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                            = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                              NULL, archex = FALSE),distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.GARCH.SNORM.eur<-ugarchfit(data=res.eur.usd.square,spec=SNORM.spec.eur)
show(fit.GARCH.SNORM.eur)

###SGARCH(1,1) Normal Inverse Gaussian
NIG.spec.eur<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                               submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                          = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                            NULL, archex = FALSE),distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.GARCH.NIG.eur<-ugarchfit(data = res.eur.usd.square,spec=NIG.spec.eur)
show((fit.GARCH.NIG.eur))

###TGARCH(1,1) t-student
spec.Std.eur<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                  "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                         c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE), distribution.model = "std", start.pars = list(), fixed.pars = list())
fit.TGARCH.Std.eur<-ugarchfit(data = res.eur.usd.square,spec = spec.Std.eur)
show(fit.TGARCH.Std.eur)

###TGARCH(1,1) Normal distribution
spec.NORM.eur<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                   "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                          c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                          NULL, archex = FALSE), distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.TGARCH.NORM.eur<-ugarchfit(data = res.eur.usd.square,spec=spec.NORM.eur)
show(fit.TGARCH.NORM.eur )

###TGARCH(1,1) Skewness normal
spec.SNORM.eur <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                     "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                            c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                            NULL, archex = FALSE), distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.TGARCH.SNORM.eur<-ugarchfit(data = res.eur.usd.square,spec = spec.SNORM.eur)
show(fit.TGARCH.SNORM.eur)

###TGARCH(1,1) Normal Inverse Gaussian
spec.NIG.eur<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                  "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                         c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE), distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.TGARCH.NIG.eur<-ugarchfit(data = res.eur.usd.square,spec = spec.NIG.eur)
show(fit.TGARCH.NIG.eur)

###GARCH GBP/USD
###SGARCH(1,1) t-student
Std.spec.gbp<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                           = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                             NULL, archex = FALSE),distribution.model = "std", start.pars = list(), fixed.pars = list())# t di student
fit.GARCH.Std.gbp<-ugarchfit(data = res.gbp.usd.square,spec = Std.spec.gbp)
show(fit.GARCH.Std.gbp)

###SGARCH(1,1) Normal distribution
NORM.spec.gbp<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                           = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                             NULL, archex = FALSE),distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.GARCH.NORM.gbp<-ugarchfit(data = res.gbp.usd.square,spec=NORM.spec.gbp)
show(fit.GARCH.NORM.gbp)

###SGARCH(1,1) Skeweness normal
SNORM.spec.gbp<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                 submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                            = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                              NULL, archex = FALSE),distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.GARCH.SNORM.gbp<-ugarchfit(data=res.gbp.usd.square,spec=SNORM.spec.gbp)
show(fit.GARCH.SNORM.gbp)

###SGARCH(1,1) Normal Inverse Gaussian
NIG.spec.gbp<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                               submodel = NULL, external.regressors= NULL, variance.targeting = FALSE), mean.model = list(armaOrder
                                                                                                                                          = c(0,0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                            NULL, archex = FALSE),distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.GARCH.NIG.gbp<-ugarchfit(data = res.gbp.usd.square,spec=NIG.spec.gbp)
show((fit.GARCH.NIG.gbp))

###TGARCH(1,1) t-student
spec.Std.gbp<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                  "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                         c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE), distribution.model = "std", start.pars = list(), fixed.pars = list())
fit.TGARCH.Std.gbp<-ugarchfit(data = res.gbp.usd.square,spec = spec.Std.gbp)
show(fit.TGARCH.Std.gbp)

###TGARCH(1,1) Normal distribution
spec.NORM.gbp<-fit.GEDTspec <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                                  "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                                         c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                                         NULL, archex = FALSE), distribution.model = "norm", start.pars = list(), fixed.pars = list())
fit.TGARCH.NORM.gbp<-ugarchfit(data = res.gbp.usd.square,spec=spec.NORM.gbp)
show(fit.TGARCH.NORM.gbp)

###TGARCH(1,1) Skewness Normal
spec.SNORM.gbp<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                    "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                           c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                           NULL, archex = FALSE), distribution.model = "snorm", start.pars = list(), fixed.pars = list())
fit.TGARCH.SNORM.gbp<-ugarchfit(data = res.gbp.usd.square,spec = spec.SNORM.gbp)
show(fit.TGARCH.SNORM.gbp)

###TGARCH(1,1) Normal Inverse Gaussian
spec.NIG.gbp<- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel =
                                                  "TGARCH", external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder =
                                                                                                                                         c(0, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors =
                                                                                                                                         NULL, archex = FALSE), distribution.model = "nig", start.pars = list(), fixed.pars = list())
fit.TGARCH.NIG.gbp<-ugarchfit(data = res.gbp.usd.square,spec = spec.NIG.gbp)
show(fit.TGARCH.NIG.gbp)