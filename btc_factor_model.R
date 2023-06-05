# Estimates and plots a four factor model for the price of Bitcoin
# Factors: NASDAQ index, US Dollar, Gold price, US Treasury 2 year yield

symbols = c("NASDAQCOM", "CBBTCUSD", "DGS2", "DTWEXBGS")
getSymbols(symbols, src = "FRED")
getSymbols("GLD", src = "yahoo")

data = merge(CBBTCUSD, NASDAQCOM, Cl(GLD), DTWEXBGS, DGS2)
data = na.omit(data)

trunc_data = subset(data, index(data) >= "2020-01-01")

model = lm(trunc_data[,1]~trunc_data[,2]+trunc_data[,3]+trunc_data[,4]+trunc_data[,5])
summary(model)

fit.model = trunc_data[,1] - residuals(model)

plot(trunc_data[,1], main = "BTC/USD vs Factor Model")
lines(fit.model, col="blue")
addLegend("topright", legend.names=c("BTC/USD", "4 Factor Model"), col = c("black", "blue"), lty=c(1,1))

