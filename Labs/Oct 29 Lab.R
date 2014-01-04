install.packages("WDI")
require(WDI)

gdp <- WDI(country=c("US","CA","GB","CN","JP"), indicator=c("NY.GDP.PCAP.CD","NY.GDP.MKTP.CD"),
           start=1960,end=2011)

require(plyr)
require(ggplot2)

names(gdp) <- c("iso2c","Country","Year","PerCapGDP","GDP")
ggplot(gdp, aes(x=Year,y=PerCapGDP)) + 
  geom_line(aes(color=Country, linetype=Country))

us <- gdp$PerCapGDP[gdp$Country=="United States"]
us <- ts(us, start=min(gdp$Year), end=max(gdp$Year))
us
acf(us)
pacf(us)
install.packages("forecast")
require(forecast)

usBest <- auto.arima(x=us)
plot(diff(us))
plot(diff(us,differences=2))

plot(usBest$residuals)
acf(usBest$residuals)
pacf(usBest$residuals)

predict(usBest,n.ahead=5, se.fit=TRUE)

theForecast <- forecast(usBest, h=5)
plot(theForecast)

install.packages("quantmod")
require(quantmod)

att <- getSymbols("T",auto.assign=FALSE)
att
head(att)

require(xts)
plot(att)
chartSeries(att)
addBBands()
addMACD(32,50,12)
attClose <- att$T.Close
class(attClose)
head(attClose)
install.packages("rugarch")
require(rugarch)

#parameters of the garch model
attSpec <- ugarchspec(variance.model=list(model="sGARCH",
                                           garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(1,1)),
                       distribution.model="std")
attGarch <- ugarchfit(spec=attSpec, data=attClose)
attGarch

usDF <- gdp[gdp$Country == "United States",]
ggplot(usDF, aes(x=Year, y=PerCapGDP, size=GDP)) + geom_point(shape=7)


#xts package -- it's for time series data
