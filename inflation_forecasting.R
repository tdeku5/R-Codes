# Time series models for forecasting annual & monthly Headline CPI
# Will be updated in the near future
# Currently am working on expanding this by breaking CPI into components and evaluating at a more granular level
# and expanding the analysis into PCE and other economic data

# Annual Inflation Forecasting

# AR(1) model for annual inflation
ann_data = read.table("ann_cpi.txt", header=T)
ann_cpi = ts(ann_data[,2], start=1982, frequency=1)
ann_inflation = diff(log(ann_cpi))
ann_inflation = ts(ann_inflation, start=1982, frequency=1)

ann_ar1 = arima(ann_inflation, order=c(1,0,0))
ann_ar1

fit.ann_ar1 = ann_inflation - residuals(ann_ar1)
ts.plot(ann_inflation, fit.ann_ar1, lty=c(1:2))

# phi0 = 0.01859055
# Annual Model: inf(i) = 0.01859055 + 0.3477*inf(i-1) + et

# Forecast
inf2021 = ann_inflation[40]
inf2022_hat = 0.01859055 + 0.3477*inf2021

###############################################################################
# Monthly Inflation Forecasting (Headline CPI)

# ARMA(1,2) model for monthly inflation
monthly_data = read.table("cpi_82_23.txt", header=T)
monthly_cpi = ts(monthly_data[,2], start=1982, frequency=12)
monthly_inflation = diff(log(monthly_cpi))
monthly_inflation = ts(monthly_inflation, start=1982, frequency=12)

mon_arma12 = arima(monthly_inflation, order=c(1,0,2))
#mon_arma12
fit.mon_arma12 = monthly_inflation - residuals(mon_arma12)

phi0 = mon_arma12$coef[4]*(1-mon_arma12$coef[1]) # phi0 = mu*(1-phi1)
phi1 = mon_arma12$coef[1]
theta1 = mon_arma12$coef[2]
theta2 = mon_arma12$coef[3]

# Error term estimation
x = length(monthly_inflation)
ehat = rep(0,x)
ehat[1] = monthly_inflation[1]
ehat[2] = monthly_inflation[2] - phi0 - phi1*monthly_inflation[1] - theta1*ehat[1]
ehat[3] = monthly_inflation[3] - phi0 - phi1*monthly_inflation[2] - theta1*ehat[2] - theta2*ehat[1]
for (j in 3:x)
  ehat[j] = monthly_inflation[j] - phi0 - phi1*monthly_inflation[j-1] - theta1*ehat[j-1] - theta2*ehat[j-2]

# Forecast for next month's print
inf_last = monthly_inflation[x]
next_month_forecast = phi0 + phi1*inf_last + theta1*ehat[492] + theta2*ehat[491]

# Six month forecast
forecast = rep(0,6)
forecast[1] = phi0 + phi1*inf_last + theta1*ehat[492] + theta2*ehat[491]
for (i in 2:6)
  forecast[i] = phi0 + phi1*forecast[i-1]

# Graphs
y_all = c(monthly_inflation, forecast)
y_all = ts(y_all, start=1982, frequency=12)

ts.plot(monthly_inflation, fit.mon_arma12, lty=c(1:1), col=c("blue","red"), lwd=c(2,2), main="U.S. Monthly Headline Inflation: 1/1/1982 - 2/1/2023", ylab="Inflation")
legend(x="bottomright",lty=c(1,2), col=c("blue","red"), legend=c("Inflation", "ARMA(1,2)"))
par(mfrow=c(3,1))
plot(monthly_inflation, main="Monthly Inflation", ylab="Inflation")
plot(fit.mon_arma12, main="ARMA(1,2) for Monthly Inflation", ylab="Inflation")
plot(c(y_all[(x-23):x],rep(NA, 6)), type="l", lwd=2, lty=1, main="Last 24 Months & 6 Month Forecast", xlab="Time", ylab="Inflation")
lines(c(rep(NA, 24), forecast), lty=2, col = "red", lwd=2)

###############################################################################
