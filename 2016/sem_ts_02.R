library(forecast) # time series modelling
library(sophisthse) # download russian macro data

# sophist.hse.ru
y <- sophisthse("POPNUM_Y")
tsdisplay(y)

# если стационарный, то похож на AR(1)
ar1 <- Arima(y, order = c(1, 0, 0))
ar1

forecast(ar1, h = 3)

prognoz <- forecast(ar1, h = 3)
plot(prognoz)

# автоподбор
comp <- auto.arima(y)
comp
prognoz2 <- forecast(comp, h = 3)
plot(prognoz2)
