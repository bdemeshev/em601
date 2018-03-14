library(forecast)

set.seed(24)

# e_t ~ N(0; 7)
eps <- rnorm(100, mean = 0, sd = sqrt(7))
eps[1]
eps[1:10]

eps_ts <- ts(eps, 
             frequency = 12, 
          start = c(2001, 2))
eps_ts

ggtsdisplay(eps_ts)

# y_t = e_1 + e_2 + ... + e_t
y <- cumsum(eps_ts)
ggtsdisplay(y)

# y_t = e_t - 2 e_t-2 + 3 e_t-3
# MA(3) модель с коэффициентами
# 0, -2, 3
z <- arima.sim(n = 100, 
    model = list(
      ma = c(0, -2, 3))) * sqrt(7)
ggtsdisplay(z)

Acf(y, plot = FALSE)

