# Weatherproof Rebalancing Trend Signals

library(quantmod)
library(tseries)
library(moments)

symbols = c('SCHX', 'VEA', 'VWO', 'IEF', 'SPTL', 'IGOV', 'TIP', 'GLD', 'DBC', 'VNQ')
getSymbols(symbols, src = 'yahoo')

data = merge(Cl(SCHX), Cl(VEA), Cl(VWO), Cl(IEF), Cl(SPTL), Cl(IGOV), Cl(TIP), Cl(GLD), Cl(DBC), Cl(VNQ))
data = na.omit(data)

most_recent_close = data.frame(last(data))

window = 189    # 9 months
rolling_mean = rollmean(data, window, align = 'right', fill = NA)

z = ncol(data)
moving_average = rep(0, z)
signal = rep("", z)
for (i in 1:z) {
  moving_average[i] = last(rolling_mean[,i])
  if (last(data[,i]) < moving_average[i]) {
    signal[i] = "Sell & move to cash"
  }
  else {
    signal[i] = "Buy/Hold"
  }
}

most_recent_close
moving_average
signal


###########################################################
# Aggressive Strategy


# 12 month total return
n = 252
data_12mo = tail(data, n)
return_12mo = rep(0,z)
for (i in 1:z) {
  return_12mo[i] = (most_recent_close[,i] - data_12mo[1,i])/data_12mo[1,i]
}

# 6 month total return
n = 126
data_6mo = tail(data, n)
return_6mo = rep(0,z)
for (i in 1:z) {
  return_6mo[i] = (most_recent_close[,i] - data_6mo[1,i])/data_6mo[1,i]
}

# 3 month total return
n = 63
data_3mo = tail(data, n)
return_3mo = rep(0,z)
for (i in 1:z) {
  return_3mo[i] = (most_recent_close[,i] - data_3mo[1,i])/data_3mo[1,i]
}

# 1 month return
n = 21
data_1mo = tail(data, n)
return_1mo = rep(0,z)
for (i in 1:z) {
  return_1mo[i] = (most_recent_close[,i] - data_1mo[1,i])/data_1mo[1,i]
}

# Average
avg_ret = rep(0,z)
for (i in 1:z){
  avg_ret[i] = (return_12mo[i] + return_6mo[i] + return_3mo[i] + return_1mo[i]) / 4
}

# Select Top 5 assets
ranked_values <- rank(-avg_ret, ties.method="first")
data1 = data.frame(symbols, avg_ret, ranked_values)
ranked_data <- data1[order(data1$ranked_values),]
print(ranked_data[1:5,])


##########################################################
# Plots
plot(data[,1], main = 'US Large Cap Equities (SCHX)')
lines(rolling_mean[,1], col = 'red', lwd=2)

plot(data[,2], main = 'Foreign Developed Equities (VEA)')
lines(rolling_mean[,2], col = 'red', lwd=2)

plot(data[,3], main = 'Foreign EM Equities (VWO)')
lines(rolling_mean[,3], col = 'red', lwd=2)

plot(data[,4], main = 'US 10 Year Treasuries (IEF)')
lines(rolling_mean[,4], col = 'red', lwd=2)

plot(data[,5], main = 'US 30 Year Treasuries (SPTL)')
lines(rolling_mean[,5], col = 'red', lwd=2)

plot(data[,6], main = 'Foreign 10 Year Bonds (IGOV)')
lines(rolling_mean[,6], col = 'red', lwd=2)

plot(data[,7], main = 'TIPS (TIP)')
lines(rolling_mean[,7], col = 'red', lwd=2)

plot(data[,8], main = 'Gold (GLD)')
lines(rolling_mean[,8], col = 'red', lwd=2)

plot(data[,9], main = 'Commodities (DBC)')
lines(rolling_mean[,9], col = 'red', lwd=2)

plot(data[,10], main = 'Real Estate (VNQ)')
lines(rolling_mean[,10], col = 'red', lwd=2)


########################################################
#window = 210
#today = Sys.Date()
#past = today - window
#z = ncol(data)
#ma_10mo = rep(0,z)
#signal = rep("",z)
#for (i in 1:z){
#  ma_10mo[i] = mean(data[paste0(past, "/", today), i])
#  if (last(data[,i]) < ma_10mo[i]){
#    signal[i] = "Sell & move to cash"
#  }
#  else {
#    signal[i] = "Buy/Hold"
#  }
#}
#ma_10mo
#signal
