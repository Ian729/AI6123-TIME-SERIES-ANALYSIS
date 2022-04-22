data = scan("./wwwusage.txt",skip=1)
mean(data)
plot(data,xlab="Time",ylab="Number of users")
acf(data,lag.max = 50) # cut off after 31
pacf(data) # cut off after  2
# non stationary

# first order Difference
d1 = diff(data)
plot(d1)
lines(d1)
acf(d1,lag.max = 50) # cut off after 24
pacf(d1) # cut off after 3

# Yule-Walker Estimation
yw1 = ar.yw(d1)
summary(yw1);yw1 # AR(3) with parameters 1.106, -0.596, 0.303

# Fit ARIMA(3,1,0) model
arima1 = arima(data, order=c(3,1,0))
arima1
tsdiag(arima1)
AIC(arima1)

# second order difference
d2 = diff(d1)
plot(d2)
lines(d2)
acf(d2,lag.max = 50) # cut off after 27
pacf(d2) # cut off after 2

# Yule-Walker Estimation
yw2 = ar.yw(d2)
summary(yw2);yw2 # AR(2) with parameters 0.2489 and -0.4341

# Fit ARIMA(2,2,0) model
arima2 = arima(data, order=c(2,2,0))
arima2
tsdiag(arima2)
AIC(arima2)

library(forecast)
# Auto ARIMA
autoarima = auto.arima(data)
autoarima
arima3 = arima(data, order=c(1,1,1))
arima3
tsdiag(arima3)
AIC(arima3)
